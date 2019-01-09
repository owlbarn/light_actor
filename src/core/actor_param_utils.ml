(*
 * Light Actor - Parallel & Distributed Engine of Owl System
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)


let is_ready book =
  let ready = ref true in
  Hashtbl.iter (fun _ n ->
    if Actor_book.(String.length n.addr = 0) then
      ready := false
  ) book;
  !ready


let arr_to_htbl arr =
  let len = Array.length arr in
  let htbl = Hashtbl.create len in
  Array.iter (fun k ->
    Hashtbl.add htbl k k
  ) arr;
  htbl


let htbl_to_arr htbl =
  Hashtbl.fold (fun k v acc ->
    Array.append acc [| (k,v) |]
  ) htbl [||]
