open Mirage

let uuid =
  let doc = Key.Arg.info ~doc:"uuid of service" ["uuid"] in
  Key.(create "uuid" Arg.(opt string "server" doc))

let server_ip =
  let doc = Key.Arg.info ~doc:"server IP address" ["server_ip"] in
  Key.(create "server_ip" Arg.(opt string "127.0.0.1" doc))

let server_port =
  let doc = Key.Arg.info ~doc:"server port." ["server_port"] in
  Key.(create "server_port" Arg.(opt string "5555" doc))

let ip =
  let doc = Key.Arg.info ~doc:"my IP address" ["ip"] in
  Key.(create "ip" Arg.(opt string "127.0.0.1" doc))

let port =
  let doc = Key.Arg.info ~doc:"my port." ["port"] in
  Key.(create "port" Arg.(opt string "6000" doc))

let main =
  let packages = [
    package "owl-base";
    package "actor";
    package "actor_mirage";
    package "duration";
    package "lwt_ppx";
  ] in
  let keys = List.map Key.abstract [
      server_ip;
      server_port;
      uuid;
      ip;
      port;
    ] in
  foreign ~packages ~keys "Unikernel.Main" (stackv4 @-> kv_ro @-> job)

let disk = generic_kv_ro "t"

let () =
  let stack = generic_stackv4 default_network in
  register "lwae" [
    main $ stack $ disk
  ]
