exception Hell of (int * int);;

let teile (n, m) =
  try Some (n/m) with 
  | Division_by_zero -> None
  | Failure _ -> None
;;
teile (10, 3);;
teile (10, 0);;

let rec member x l =
  try 
    if x = List.hd l then true else member x (List.tl l)
  with 
  | Failure _ -> false
;;
member 2 [1;2;3];;
member 5 [1;2;3];;

(* Man kann auch selbst Exceptions auslösen *)
try 
  raise Division_by_zero
with 
| Failure _ -> 0
| Division_by_zero -> 5
;;

let f (x, y) = x / (y-1)

let g (x, y) = 
  try 
    let n =
      try 
        f (x, y)
      with
      | Division_by_zero -> raise (Failure "Division by zero")
    in
    string_of_int (n*n)
  with 
  | Failure str -> "Error: " ^ str
;;

g (6, 1);;
g (6, 3);;