type nat = Zero | Succ of nat

let rec int_to_nat zahl =
  match zahl with
  | 0 -> Zero
  | _ -> Succ (int_to_nat (zahl-1))

let rec nat_to_int nat =
  match nat with
  | Zero -> 0
  | Succ(x) -> 1 + nat_to_int x

let rec add nat1 nat2 =
  match nat1 with
  | Zero -> nat2
  | Succ(x) -> add x (Succ (nat2))

let rec mul n1 n2 =
  match n1 with
  | Zero -> Zero
  | Succ(Zero) -> n2
  | Succ(x) -> mul x (add n2 n2)

let rec pow nat1 exp =
  match exp with
  | Zero -> Succ(Zero)
  | Succ(Zero) -> nat1 
  | Succ(x) -> mul nat1 (pow nat1 x)

let rec leq nat1 nat2 =
  match (nat1, nat2) with
  | Zero, Zero -> true
  | Succ(x), Zero -> true
  | Zero, Succ(x) -> false
  | Succ(x), Succ(y) -> leq x y