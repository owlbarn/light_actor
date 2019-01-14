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


  let heartbeat context =
    let rec loop i =
      let%lwt () = Sys.sleep 10. in
      Actor_log.debug "Heartbeat #%i %s" i context.my_uuid;
      loop (i + 1)
    in
    loop 0


  let terminate context =
    Hashtbl.iter (fun uuid _ ->
      Actor_log.debug ">>> %s Exit" uuid;
      let addr = Actor_book.get_addr context.book uuid in
      let s = encode_message uuid addr (Exit 0) in
      Lwt.async (fun () -> Net.send addr s)
    ) context.book


  let schedule uuid context =
    Actor_log.debug "Schedule %s" context.my_uuid;
    Actor_barrier_bsp.sync context.book uuid;
    if Impl.stop () = false then (
      let passed = Actor_barrier_bsp.pass context.book in
      let tasks = Impl.schd passed in
      Array.iter (fun (uuid, kv_pairs) ->
        Actor_log.debug ">>> %s PS_Schd" uuid;
        let addr = Actor_book.get_addr context.book uuid in
        let s = encode_message uuid addr (PS_Schd kv_pairs) in
        Lwt.async (fun () -> Net.send addr s)
      ) tasks
    )
    else (
      terminate context;
      failwith "finished"
    )


  let process context data =
    let m = decode_message data in
    let my_uuid = context.my_uuid in
    let my_addr = context.my_addr in

    match m.operation with
    | Reg_Req -> (
        Actor_log.debug "<<< %s Reg_Req" m.uuid;
        Actor_book.set_addr context.book m.uuid m.addr;
        let s = encode_message my_uuid my_addr Reg_Rep in
        let%lwt () = Net.send m.addr s in
        (* bootstrapping is a BSP barrier *)
        schedule m.uuid context;
        Lwt.return ()
      )
    | Heartbeat i -> (
        Actor_log.debug "<<< %s Heartbeat #%i" m.uuid i;
        Lwt.return ()
      )
    | PS_Get _keys -> (
        Actor_log.debug "<<< %s PS_Get" m.uuid;
        Actor_log.error "PS_Get is not implemented";
        Lwt.return ()
      )
    | PS_Set updates -> (
        Actor_log.debug "<<< %s PS_Set" m.uuid;
        Impl.set updates;
        Lwt.return ()
      )
    | PS_Push updates -> (
        Actor_log.debug "<<< %s Push" m.uuid;
        Impl.pull updates |> Impl.set;
        schedule m.uuid context;
        Lwt.return ()
      )
    | _ -> (
        Actor_log.error "unknown message type";
        Lwt.return ()
      )


  let init context =
    let%lwt () = Net.init () in

    try%lwt (
      (* start server service *)
      let thread_0 = Net.listen context.my_addr (process context) in
      let thread_1 = heartbeat context in
      let%lwt () = thread_0 in
      let%lwt () = thread_1 in
      Lwt.return ()
    )
    with _ -> (
      (* clean up when server exits *)
      Actor_log.debug "%s exits" context.my_uuid;
      let%lwt () = Sys.sleep 1. in
      Net.exit ()
    )


end
