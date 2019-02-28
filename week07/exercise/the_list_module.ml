let squaresum1 liste =
  let rec sum l = 
    match l with
    | x::xs -> x + sum xs
    | [] -> 0
  in
  sum (List.map (fun x -> x * x) liste)

let squaresum2 liste =
  let acc = 0 in
  List.fold_left (fun a x -> x * x + a) acc liste

let squaresum3 liste =
  let acc = 0 in
  List.fold_right (fun x a -> x * x + a) liste acc

let float_list1 liste =
  List.map (fun (x:int) -> float_of_int x) liste

let float_list2 liste =
  List.map float_of_int liste

let to_string1 liste =
  "[" ^
  List.fold_left (
    fun a x -> 
      if a = "" then a ^ (string_of_int x) 
      else a ^ ", " ^ (string_of_int x)
  ) "" liste
  ^ "]"

let to_string2 liste =
  "[" ^
  List.fold_right (
    fun x a -> 
    if a = "" then (string_of_int x) ^ a 
    else (string_of_int x) ^ ", " ^ a
  ) liste ""
  ^ "]"

let part_even1 liste =
  let even =
    List.filter (fun x -> x mod 2 == 0) liste
  in
  let odd =
    List.filter (fun x -> x mod 2 <> 0) liste
  in 
  List.append even odd
  (* bzw. even @ odd *)