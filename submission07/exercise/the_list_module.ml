(* 1 *)
let sum l = List.fold_left (fun x y -> x + y) 0 l
let sum l = List.fold_left (+) 0 l
let squaresum_left l = List.fold_left (fun x y -> x + y * y) 0 l;;
let squaresum_right l = List.fold_right (fun x y -> x * x + y) l 0;;
let squaresum_alternative l = 
  let mapped = List.map (fun x -> x * x) l in sum mapped
;;
squaresum_left [4;4;4];;
squaresum_right [5;5;5];;

(* 2 *)
let float_list l = List.map(fun (x : int) -> float_of_int x) l;; 
let float_list_alternative l = List.map float_of_int l;;
float_list [1;2;3;4];;
float_list_alternative [2;3;4];;

(* 3 *)
let to_string l = "[" ^ 
  List.fold_right (fun x a -> 
    if a = "" then (string_of_int x) ^ a
    else (string_of_int x) ^ ";" ^ a   
  )
  l ""  ^ "]";;
let to_string_alternative l =
  let f a b =
    if a = "" then
      "[" ^ string_of_int b
    else a ^ ";" ^ string_of_int b
  in List.fold_left f "" l ^ "]"
;;
to_string [1;2;3;4;5;6;7];;
to_string [1;2];;
to_string [23560];;

(* 4 *)
let part_even l = 
  List.filter (fun x -> x mod 2 = 0) l 
    @ 
  List.filter (fun x -> x mod 2 <> 0) l
;;
part_even [1;2;3;4];;