type nat = Zero | Succ of nat

let rec int_to_nat : int -> nat =
  fun c ->
    if c > 0 then Succ ( int_to_nat (c-1))
    else
      if c = 0 then Zero
      else failwith "Negative Numbers not possible"

let rec nat_to_int : nat -> int =
  fun n ->
    match n 
      with Zero -> 0
      | Succ (x) -> 1 + nat_to_int (x)

let rec add : nat -> nat -> nat =
  fun n m -> 
    match n 
      with Zero -> m
      | Succ (x) -> add x (Succ (m))

let rec mul : nat -> nat -> nat =
  fun n m ->
    match n
      with Zero -> Zero
      | Succ (x) -> add m (mul x m)

let rec pow : nat -> nat -> nat =
  fun n m ->
    match m
      with Zero -> (Succ(Zero))
      | Succ (x) -> mul n (pow n x)

let leq : nat -> nat -> bool =
  fun a b -> 
    if a > b then false else true
