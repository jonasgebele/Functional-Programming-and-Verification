let hd = function [] -> failwith "BS" | x::xs -> x;;
let t1 = function [] -> failwith "BS" | x::xs -> xs;;
let rec length = function [] -> 0 | x::xs -> 1 + length xs;;
let rec append_rec =
  fun t1 t2 ->
    match t1
      with [] -> t2
      | x::xs -> x :: append_rec xs t2
;;
let rec rev = 
  fun t1 ->
    match t1
      with [] -> []
      | x::xs -> rev xs @ [x]
;;
(* Beginnend zu zählen bei 0 *)
let rec nth : int -> 'a list -> 'a = 
  fun c t1 -> 
    match t1
      with [] -> failwith "none"
      | x::xs -> if c = 0 then x else nth (c-1) xs
;;