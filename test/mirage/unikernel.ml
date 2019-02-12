module Impl = struct

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
    Array.iter (fun (key, value) ->
      Hashtbl.replace model key value
    ) kv_pairs

  let schd nodes =
    Array.map (fun node ->
      let i = Random.int 3 in
      let key = [|"a"; "b"; "c"|].(i) in
      let value = (get [|key|]).(0) in
      let tasks = [|(key, value)|] in
      (node, tasks)
    ) nodes

  let push kv_pairs =
    Array.map (fun (key, value) ->
      let new_value = value + Random.int 10 in
      Logs.info (fun f -> f "%s: %i => %i" key value new_value);
      (key, new_value)
    ) kv_pairs

  let pull updates = updates

  let stop () =
    Logs.info (fun f -> f "start_t = %i" !start_t);
    start_t := !start_t + 1;
    !start_t >= 50

end

include Actor_param_types.Make(Impl)

module Main (S: Mirage_stack_lwt.V4) = struct

  module N = Actor_net_mirage.Make (S)
  module M = Actor_param.Make (N) (Actor_sys_mirage) (Impl)

  let start (s : S.t)  =
    N.stored_stack_handler := Some s;

    let server_uuid = "server" in
    let server_ip = Key_gen.server_ip () in
    let server_port = Key_gen.server_port () in
    let server_addr = "udp://" ^ server_ip ^ ":" ^ server_port in

    let my_uuid = Key_gen.uuid () in
    let my_ip = Key_gen.ip () in
    let my_port = Key_gen.port () in
    let my_addr = "udp://" ^ my_ip ^ ":" ^ my_port in

    (* define the participants *)
    let book = Actor_book.make () in
    Actor_book.add book "w0" "" true (-1);
    Actor_book.add book "w1" "" true (-1);

    let my_addr =
      if my_uuid = server_uuid then
        server_addr
      else begin
        Actor_book.set_addr book my_uuid my_addr;
        my_addr
      end in

    Logs.info (fun f -> f "uuid=%s addr=%s server=%s" my_uuid my_addr server_addr);

    let context = {
      my_uuid;
      my_addr;
      server_uuid;
      server_addr;
      book;
    }
    in

    M.init context

end
