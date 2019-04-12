(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Lwt.Infix

module N1 = Owl_neural_generic.Make (Owl_base_dense_ndarray.S)
open N1
open Graph
module A = Owl_algodiff_generic.Make (Owl_base_dense_ndarray.S)
open A
module G = Graph

type task = {
  mutable state  : Checkpoint.state option;
  mutable nn     : G.network;
}

let make_network input_shape =
  input input_shape
  |> normalisation ~decay:0.9
  |> fully_connected 256 ~act_typ:Activation.Relu
  |> linear 10 ~act_typ:Activation.(Softmax 1)
  |> get_network

let chkpt _state = ()
let params = Params.config
    ~batch:(Batch.Mini 100) ~learning_rate:(Learning_Rate.Adagrad 0.005) 1.

(* Utilities *)

let make_task nn = {
  state = None;
  nn;
}

let delta_nn nn0 nn1 =
  let par0 = G.mkpar nn0 in
  let par1 = G.mkpar nn1 in
  let delta = Owl_utils.aarr_map2 (fun a0 a1 -> Maths.(a0 - a1)) par0 par1 in
  G.update nn0 delta

module Impl (KV: Mirage_kv_lwt.RO) = struct

  let stored_kv_handler : KV.t option ref = ref None
  let get_kv () = match !stored_kv_handler with
    | None -> assert false
    | Some kv -> kv

  let marshal_from fname =
    KV.get (get_kv ()) (Mirage_kv.Key.v fname) >|= function
    | Error _e -> assert false
    | Ok data -> Marshal.from_string data 0

  let nth = ref 0
  let image_len = 28
  let ncat = 10
  let nent = 1000
  let refx = ref (Owl_base_dense_ndarray_s.empty [|nent; image_len * image_len|])
  let refy = ref (Owl_base_dense_ndarray_s.empty [|nent; ncat|])

  let load_image_file () =
    let fname = Printf.sprintf "train-images-idx3-ubyte-%03d.bmp" !nth in
    KV.get (get_kv ()) (Mirage_kv.Key.v fname) >>= function
    | Error _e -> assert false
    | Ok data ->
      let buf = Bytes.of_string data in
      let acc = ref [] in
      Bytes.iter (fun ch ->
          acc := (int_of_char ch |> float_of_int) :: !acc)
        buf;
      let ar = Array.of_list (List.rev !acc) in
      refx := Owl_base_dense_ndarray.S.of_array ar [|nent;image_len;image_len;1|];
      Lwt.return_unit

  let load_label_file () =
    let fname = Printf.sprintf "train-labels-idx1-ubyte-%03d.lvl" !nth in
    KV.get (get_kv ()) (Mirage_kv.Key.v fname) >>= function
    | Error _e -> assert false
    | Ok data ->
      let buf = Bytes.of_string data in
      (* take single digit && accumulate a bitmap of it in a list *)
      let acc = ref [] in
      Bytes.iter (fun ch ->
          let a = Array.init ncat (fun idx ->
              if idx = (int_of_char ch) then 1. else 0.) in
          acc := a :: !acc)
        buf;
      let x = Array.concat (List.rev !acc) in
      refy := Owl_base_dense_ndarray.S.of_array x [|nent;ncat|];
      Lwt.return_unit

  let get_next_batch () =
    let prev = !nth in
    Lwt.async (fun () ->
        load_image_file () >>= fun () ->
        load_label_file () >>= fun () ->
        nth := !nth + 1;
        Lwt.return_unit);
    !refx, !refy

  let init () =
    let _, _ = get_next_batch () in
    Lwt.return_unit

  type key = string

  type value = task

  type model = (key, value) Hashtbl.t

  let start_t = ref 0 (* used in stop function #2 *)

  let model : model =
    let nn = make_network [|28;28;1|] in
    G.init nn;
    let htbl = Hashtbl.create 10 in
    Hashtbl.add htbl "a" (make_task (G.copy nn));
    htbl

  let get keys =
    Array.map (Hashtbl.find model) keys

  let set kv_pairs =
    Array.iter (fun (key, value) ->
      Hashtbl.replace model key value
    ) kv_pairs

  (* on server *)
  let schd nodes =
    Array.map (fun node ->
      Actor_log.info "node: %s schd" node;
      let key = "a" in
      let value = (get [|key|]).(0) in
      let tasks = [|(key, value)|] in
      (node, tasks)
    ) nodes

  (* on worker *)
  let push kv_pairs =
    Array.map (fun (k, v) ->
      Actor_log.info "push: %s, %s" k (G.get_network_name v.nn);
      let ps_nn = G.copy v.nn in
      let x, y = get_next_batch () in
      let state = match v.state with
        | Some state -> G.(train_generic ~state ~params ~init_model:false v.nn (Arr x) (Arr y))
        | None       -> G.(train_generic ~params ~init_model:false v.nn (Arr x) (Arr y))
      in
      Checkpoint.(state.current_batch <- 1);
      Checkpoint.(state.stop <- false);
      v.state <- Some state;
      (* return gradient instead of weight *)
      delta_nn v.nn ps_nn;
      (k, v)
    ) kv_pairs

  (* on server *)
  let pull kv_pairs =
    Gc.compact (); (* avoid OoM *)
    Array.map (fun (k, v) ->
      Actor_log.info "push: %s, %s" k (G.get_network_name v.nn);
      let u = (get [|k|]).(0) in
      let par0 = G.mkpar u.nn in
      let par1 = G.mkpar v.nn in
      Owl_utils.aarr_map2 (fun a0 a1 ->
        Maths.(a0 + a1)
      ) par0 par1
      |> G.update v.nn;
      (k, v)
    ) kv_pairs

  let stop () =
    let v = (get [|"a"|]).(0) in
    match v.state with
    | Some state ->
      let len = Array.length state.loss in
      let loss = state.loss.(len - 1) |> unpack_flt in
      if (loss < 2.0) then true else false
    | None -> false

end
