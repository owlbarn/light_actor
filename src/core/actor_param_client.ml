(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module Make
  (Net  : Actor_net.Sig)
  (Sys  : Actor_sys.Sig)
  (Impl : Actor_param_impl.Sig)
  = struct

  include Actor_param_types.Make(Impl)


  let register server_addr uuid addr =
    Actor_log.debug ">>> %s Reg_Req" server_addr;
    let s = encode_message uuid addr Reg_Req in
    Net.send server_addr s


  let heartbeat context =
    let server_addr = context.server_addr in
    let uuid = context.my_uuid in
    let addr = context.my_addr in
    let rec loop i =
      let%lwt () = Sys.sleep 10. in
      Actor_log.debug ">>> %s Heartbeat #%i" server_addr i;
      let s = encode_message uuid addr (Heartbeat i) in
      Lwt.async (fun () -> Net.send server_addr s);
      loop (i + 1)
    in
    loop 0


  let process context data =
    let m = decode_message data in
    let my_uuid = context.my_uuid in
    let my_addr = context.my_addr in

    match m.operation with
    | Reg_Rep -> (
        Actor_log.debug "<<< %s Reg_Rep" m.uuid;
        Lwt.return ()
      )
    | Exit code -> (
        Actor_log.debug "<<< %s Exit %i" m.uuid code;
        failwith "finished"
      )
    | PS_Schd tasks -> (
        Actor_log.debug "<<< %s PS_Schd" m.uuid;
        let updates = Impl.push tasks in
        let s = encode_message my_uuid my_addr (PS_Push updates) in
        Net.send context.server_addr s
      )
    | _ -> (
        Actor_log.error "unknown message type";
        Lwt.return ()
      )


  let init context =
    let%lwt () = Net.init () in

    (* register client to server *)
    let uuid = context.my_uuid in
    let addr = context.my_addr in
    let%lwt () = register context.server_addr uuid addr in

    try%lwt (
      (* start client service *)
      let thread_0 = Net.listen addr (process context) in
      let thread_1 = heartbeat context in
      let%lwt () = thread_0 in
      let%lwt () = thread_1 in
      Lwt.return ()
    )
    with _ -> (
      (* clean up when client exits *)
      Actor_log.debug "%s exits" uuid;
      let%lwt () = Sys.sleep 1. in
      Net.exit ()
    )


end
