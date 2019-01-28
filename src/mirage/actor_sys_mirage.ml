(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

let sleep delay =
  Mirage_OS.OS.Time.sleep_ns (Duration.of_sec (int_of_float delay))
