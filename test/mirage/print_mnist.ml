#require "owl";;

let len = 28;;
let buf = Bytes.make (len*len) '\000';;

let ic = open_in (Printf.sprintf "t/train-images-idx3-ubyte-%03d.bmp" (Unix.getenv "FILENR" |> int_of_string));;
seek_in ic ((Unix.getenv "OFFSET" |> int_of_string) * (len*len));;
really_input ic buf 0 (Bytes.length buf);;
let list = ref [];;
Bytes.iter (fun x -> list := (float_of_int (int_of_char x)) :: !list) buf;;
let ar = Array.of_list (List.rev !list);;
let x = Owl.Arr.of_array ar [|1;Bytes.length buf|];;
let x = Owl.Arr.reshape x [|len;len|];;
Owl.Arr.print x;;


let ic = open_in (Printf.sprintf "t/train-labels-idx1-ubyte-%03d.lvl" (Unix.getenv "FILENR" |> int_of_string));;
seek_in ic (Unix.getenv "OFFSET" |> int_of_string);;
Printf.printf "Correct number is %d\n" (input_char ic |> int_of_char);;
