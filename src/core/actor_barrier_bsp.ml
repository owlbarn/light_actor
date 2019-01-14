(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Actor_book


let pass book =
  let fastest = Hashtbl.fold (fun _ node acc ->
    max node.step acc
  ) book min_int
  in

  let synced = Hashtbl.fold (fun _ node acc ->
    not (fastest != node.step || node.busy = true) && acc
  ) book true
  in

  if synced then (
    Hashtbl.fold (fun uuid node acc ->
      if node.busy = false then (
        node.busy <- true;
        Array.append acc [| uuid |]
      )
      else
        acc
    ) book [| |]
  )
  else
    [| (* nobody shall pass *) |]


let sync book uuid =
  let step = Actor_book.get_step book uuid in
  Actor_book.set_busy book uuid false;
  Actor_book.set_step book uuid (step + 1)
