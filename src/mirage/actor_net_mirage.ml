(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Lwt.Infix

let port_of_string addr =
  String.split_on_char ':' addr
  |> List.rev |> List.hd |> int_of_string

let addr_of addr =
  let ip_of_string s =
    match String.split_on_char '/' s with
    | _ :: _ :: [x] -> String.split_on_char '/' x |> List.rev |> List.hd
    | _ -> failwith "Invalid address"
  in
  let ip, port =
    match String.split_on_char ':' addr with
    | _ :: ip :: port :: [] -> ip, port
    | _ -> failwith "wrong format"
  in
  let dst_port = int_of_string port in
  let dst = Ipaddr.V4.of_string_exn (ip_of_string ip) in
  dst, dst_port


module Make_Conn (S : Mirage_stack_lwt.V4) = struct

  type t = {
    flow: S.TCPV4.flow;
    len:  int;
    bufs: bytes list;
  }

  let pool : (string, t) Hashtbl.t = Hashtbl.create 128

  let flow addr =
    let v = Hashtbl.find pool addr in
    v.flow

  let add addr flow bytes =
    try
      let v = Hashtbl.find pool addr in
      (* assert (v.flow = flow);
         * assert (v.len >= Bytes.length bytes); *)
      Hashtbl.replace pool addr
        {flow=v.flow; len=v.len; bufs=bytes::v.bufs}
    with Not_found ->
      let len = Marshal.total_size bytes 0 in
      Hashtbl.add pool addr
        {flow=flow; len=len; bufs=[bytes]}

  let is_full addr =
    let v = Hashtbl.find pool addr in
    let sum = List.fold_left
        (fun a el -> a + Bytes.length el) 0 v.bufs in
    v.len = sum

  let bufs addr =
    let v = Hashtbl.find pool addr in
    let bytes = Bytes.concat Bytes.empty (List.rev v.bufs) in
    Hashtbl.remove pool addr;
    bytes

  let exit () =
    Hashtbl.iter (fun _ v ->
        Lwt.async (fun () -> S.TCPV4.close v.flow)
      ) pool
end


module Make (S : Mirage_stack_lwt.V4) = struct

  let prefix = "tcp://"

  module Conn = Make_Conn(S)

  type socket = Conn.t

  let stored_stack_handler : S.t option ref = ref None
  let get_stack () = match !stored_stack_handler with
    | None -> failwith "Uninitialized s"
    | Some s -> s

  let init () =
    Lwt.return_unit

  let exit () =
    Conn.exit ();
    Lwt.return_unit

  let listen addr callback =
    let rec handler flow =
      let string_of_flow flow =
        let ip, port = S.TCPV4.dst flow in
        (Ipaddr.V4.to_string ip) ^ (string_of_int port)
      in
      let daddr = string_of_flow flow in
      S.TCPV4.read flow >>= function
      | Error _e -> failwith "S.TCPV4.read flow"
      | Ok `Eof -> Lwt.return_unit
      | Ok (`Data buf) ->
        Conn.add daddr flow (Cstruct.to_bytes buf);
        if Conn.is_full daddr then
          callback (Bytes.to_string (Conn.bufs daddr)) >>=
          fun () -> handler flow
        else
          handler flow
    in
    let s = get_stack () in
    S.listen_tcpv4 s ~port:(port_of_string addr) handler;
    S.listen s

  let send addr data =
    let%lwt flow =
      try
        Lwt.return (Conn.flow addr)
      with Not_found ->
        S.TCPV4.create_connection (S.tcpv4 (get_stack ())) (addr_of addr)
        >>= function
        | Error _err -> failwith "S.TCPV4.create_connection"
        | Ok flow ->
          Conn.add addr flow (Bytes.of_string (Marshal.to_string "" []););
          Lwt.return flow
    in
    let%lwt _ = S.TCPV4.write flow (Cstruct.of_string data) in
    Lwt.return_unit

  let recv _sock =
    Lwt.return "" (* not used *)

  let close _sock =
    Lwt.return_unit (* not used *)

end
