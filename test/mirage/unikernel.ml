include Actor_param_types.Make(Test.Impl)

module Main (S: Mirage_stack_lwt.V4) = struct

  module N = Actor_net_mirage.Make (S)
  module M = Actor_param.Make (N) (Actor_sys_mirage) (Test.Impl)

  let start (s : S.t)  =
    N.stored_stack_handler := Some s;

    let server_uuid = "server" in
    let server_ip = Key_gen.server_ip () in
    let server_port = Key_gen.server_port () in
    let server_addr = "tcp://" ^ server_ip ^ ":" ^ server_port in

    let my_uuid = Key_gen.uuid () in
    let my_ip = Key_gen.ip () in
    let my_port = Key_gen.port () in
    let my_addr = "tcp://" ^ my_ip ^ ":" ^ my_port in

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
