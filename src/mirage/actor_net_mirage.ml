(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module Make (Time : Mirage_time_lwt.S) = struct

  let sleep time =
    let a = Int64.of_int time in
    let b = Int64.of_int 1_000_000_000 in
    let t = Int64.mul a b in
    Time.sleep_ns t

end
