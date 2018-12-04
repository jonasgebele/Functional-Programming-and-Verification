let rec fac1 = 
  fun a  ->
    match  a
      with (1, acc) -> acc
      | (m, acc) -> fac1 (m-1, m*acc)

let rec loop =
  fun x ->
    if x < 2 then x
      else if x mod 2 = 0 then loop (x/2)
      else loop (3*x+1)

let rec rev =
  fun list ->
    match list
      with [] -> []
      | x::xs -> (rev xs) @ [x]

let rev list = let rec r a l =
  match l 
    with [] -> a
    | x::xs -> r (x::a) xs
  in r [] list