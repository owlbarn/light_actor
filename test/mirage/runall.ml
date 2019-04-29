open Lwt.Infix

type obj =
  | Server
  | Worker of int

let uuid_of = function
  | Server -> "server"
  | Worker x -> "w" ^ (string_of_int x)

let idx_of = function
  | Server -> 1
  | Worker x -> x + 2

(* stolen fro actor_log.ml *)
let _colorful = ref true

type color = Red | Green | Yellow | Blue | Magenta | Cyan

let _color_to_str = function
  | Red     -> "\027[31m"
  | Green   -> "\027[32m"
  | Yellow  -> "\027[33m"
  | Blue    -> "\027[34m"
  | Magenta -> "\027[35m"
  | Cyan    -> "\027[36m"

let _shall_paint c s =
  match !_colorful with
  | true  -> (_color_to_str c) ^ s ^ "\027[0m"
  | false -> s

let color_of = function
  | Server -> Yellow
  | Worker 0 -> Magenta
  | Worker 1 -> Green
  | Worker _ -> failwith "Incorrect object"

let pfx_of obj =
  _shall_paint (color_of obj) (uuid_of obj)

let cmdstr_of obj =
  let i = idx_of obj in
  let uuid = uuid_of obj in
  Printf.sprintf
    "sudo ./solo5-hvt --disk=fat_block1.img --net=tap%d lwae.hvt --server_ip=192.168.0.1 --ipv4=192.168.0.%d/24 --ip=192.168.0.%d --port=600%d --uuid=%s"
    i i i i uuid

let proc obj =
  let cmdstr = cmdstr_of obj in
  print_endline cmdstr;
  let cmdarr = Lwt_process.shell cmdstr in
  let pr = Lwt_process.open_process_in cmdarr in
  let pfx = (pfx_of obj) ^ ":\t" in
  let rec loop () =
    if false then Lwt.return_unit else (* FIXME: just for build *)
      let ic = pr#stdout in
      Lwt_io.read_line ic >>= fun s ->
      Lwt_io.printl (pfx ^ s) >>= fun () -> loop ()
  in
  loop ()

let () =
  Lwt_main.run (Lwt.join [
      proc Server;
      proc (Worker 0);
      proc (Worker 1);
    ])

(* ocamlfind ocamlopt -o runall -linkpkg -package lwt,lwt.unix runall.ml && rm *.o *.cm? *)
