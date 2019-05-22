(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module Make_Impl (KV: Mirage_kv_lwt.RO) (R: Mirage_random.C) = struct

  let stored_kv_handler : KV.t option ref = ref None
  let get_kv () = match !stored_kv_handler with
    | None -> assert false
    | Some kv -> kv

  type model = (string, int) Hashtbl.t

  type key = string

  type value = int

  let start_t = ref 0

  let model : model =
    let htbl = Hashtbl.create 128 in
    Hashtbl.add htbl "a" 0;
    Hashtbl.add htbl "b" 0;
    Hashtbl.add htbl "c" 0;
    htbl

  let get keys =
    Array.map (Hashtbl.find model) keys

  let set kv_pairs =
    Array.iter (fun (k, v) ->
        Hashtbl.replace model k v
      ) kv_pairs

  let schd nodes =
    Array.map (fun node ->
        let k = [|"a";"b";"c"|].(Random.int 3) in
        let v = (get [|k|]).(0) in
        let tasks = [|(k, v)|] in
        (node, tasks)
      ) nodes


  let push kv_pairs =
    let ssleep n =
      for i=0 to n * 20_000 do
        if i mod 1000 = 0 then print_char '.';
        flush_all ();
        Gc.compact ();
      done;
      print_char '\n'
    in
    ssleep (Random.int 3);
    Array.map (fun (k, v) ->
        let new_value = v + Random.int 10 in
        Actor_log.info "%s: %i => %i" k v new_value;
        (k, new_value)
      ) kv_pairs

  let pull updates = updates

  let stop () =
    Actor_log.info "start_t = %i" !start_t;
    start_t := !start_t + 1;
    !start_t >= 50

  let init () =
    (* do nothing *)
    Lwt.return_unit

end
