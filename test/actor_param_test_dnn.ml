(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)


(*
 * This examples shows how to use the Actor engine on one machine. It trains a
 * VGG-like DNN on CIFAR10 dataset using the "data parallelism" approach,
 * which means that, the server and every worker know the whole DNN structure.
 * This approach is easy to implement, and works fine for small DNN.
 *
 * First, let's see how to run this example:
 * 1) Install Actor by `make install`. Note that due to version difference,
 * you might need to change "lwt_ppx" to "lwt.ppx" or vise versa in dune files.
 * Adding "lwt.unix" to the libraries stanza in src/mirage/dune file might also
 * be necessary.
 * 2) Suppose you put the source file in directory "foo", then open 3
 * terminals and run `cd foo/light_actor` on all of them.
 * 3) On one terminal run `_build/default/test/actor_param_test_dnn.exe server`;
 * on the other two, run `_build/default/testactor_param_test_dnn.exe w0` and
 * `_build/default/test/actor_param_test_dnn.exe w1` respectively.
 *
 * Something to note in this example:
 * 1) The parameter server contains only one (k,v) pair to be updated: the
 * whole network itself, and its key "a" in the code is meaningless.
 * 2) Besides network itself, the training status is also necessary to be put
 * in the value. As the code in "push" function shows, it is absolutely
 * necessary to set the "current_batch" and "stop" state manually on each
 * worker after each iteration, or the training will have problems.
 * 3) Only after all the workers start the training process will begin. During
 * training, You can close one worker any time by 'Ctrl-C', and then start it
 * again any time.
 * 4) Feel free to add or remove workers in the "main" function. By default
 * the training won't stop, and two stop functions are provided to explore
 * with.
 *)


open Owl
open Owl.Neural.S
open Graph
open Owl_algodiff.S
open Owl_optimise.S

module G = Owl.Neural.S.Graph

type task = {
  mutable state  : Checkpoint.state option;
  mutable nn     : G.network;
}

let make_network () =
  let nn =
    input [|32;32;3|]
    |> normalisation ~decay:0.9
    |> conv2d [|3;3;3;32|] [|1;1|] ~act_typ:Activation.Relu
    |> conv2d [|3;3;32;32|] [|1;1|] ~act_typ:Activation.Relu ~padding:VALID
    |> max_pool2d [|2;2|] [|2;2|] ~padding:VALID
    |> dropout 0.1
    |> conv2d [|3;3;32;64|] [|1;1|] ~act_typ:Activation.Relu
    |> conv2d [|3;3;64;64|] [|1;1|] ~act_typ:Activation.Relu ~padding:VALID
    |> max_pool2d [|2;2|] [|2;2|] ~padding:VALID
    |> dropout 0.1
    |> fully_connected 512 ~act_typ:Activation.Relu
    |> linear 10 ~act_typ:Activation.(Softmax 1)
    |> get_network
  in
  nn

let chkpt _state = ()
let params = Params.config
  ~batch:(Batch.Sample 100) ~learning_rate:(Learning_Rate.Adagrad 0.005)
  ~checkpoint:(Checkpoint.Custom chkpt) ~stopping:(Stopping.Const 1e-6) 3.

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

let get_next_batch () =
  let x, _, y = Dataset.load_cifar_train_data 1 in
  Dataset.draw_samples_cifar x y 500


(* for debugging only .. *)
(* let print_conv_weight nn =
  (G.mkpar nn).(2).(0)
  |> unpack_arr
  |> Dense.Ndarray.S.get_slice [[0];[0];[];[]]
  |> Dense.Ndarray.S.print *)


module Impl = struct

  type key = string

  type value = task

  type model = (key, value) Hashtbl.t

  let start_t = ref 0 (* used in stop function #2 *)

  let model : model =
    let nn = make_network () in
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

  (* stop function #1 *)
  (* let stop () =
    let v = (get [|"a"|]).(0) in
    match v.state with
    | Some state ->
        let len = Array.length state.loss in
        let loss = state.loss.(len - 1) |> unpack_flt in
        if (loss < 2.0) then true else false
    | None       -> false *)

  (* stop function #2 *)
  (* let stop () =
    Actor_log.info "start_t = %i" !start_t;
    start_t := !start_t + 1;
    !start_t > 7 *)

  let stop () = false

end


include Actor_param_types.Make(Impl)


module M = Actor_param.Make (Actor_net_zmq) (Actor_sys_unix) (Impl)


let ip_of_uuid id =
  try
    (Unix.gethostbyname id).h_addr_list.(0)
    |> Unix.string_of_inet_addr
  with Not_found -> "127.0.0.1"

let main args =
  Actor_log.(set_level DEBUG);
  Random.self_init ();

  (* define server uuid and address *)
  let server_uuid = "server" in
  let server_port =
    try Unix.getenv "SERVER_PORT"
    with Not_found -> "5555" in
  let server_addr = "tcp://" ^ (ip_of_uuid  server_uuid) ^
                    ":" ^ server_port in

  (* define my own uuid and address *)
  let my_uuid = args.(1) in
  let my_addr =
    if my_uuid = server_uuid then
      server_addr
    else
      let port = try Unix.getenv "PORT"
        with Not_found ->
          string_of_int (6000 + Random.int 1000)
      in
      "tcp://" ^ (ip_of_uuid my_uuid) ^ ":" ^ port
  in

  let book = Actor_book.make () in
  Actor_book.add book "w0" "" true (-1);
  Actor_book.add book "w1" "" true (-1);
  if my_uuid <> server_uuid then
    Actor_book.set_addr book my_uuid my_addr;

  (* define parameter server context *)
  let context = {
    my_uuid;
    my_addr;
    server_uuid;
    server_addr;
    book;
  }
  in

  (* start the event loop *)
  Lwt_main.run (M.init context)


let _ =
  main Sys.argv
