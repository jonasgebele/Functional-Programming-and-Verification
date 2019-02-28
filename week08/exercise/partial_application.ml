(* 8.1 Partial Application *)
(* Try to infer the types of the respective expressions. *)

let i : 'a -> 'a = fun a -> a

(*let a : (int -> int -> 'c -> -> 'c -> int) -> int = (fun a b c -> c (a+b)) 3*)

let b : 'a todo = fun a b -> (+) b

let c : 'a todo = (fun a b c -> b (c a)::[a]) "x"

let d : 'a todo = fun a b -> List.fold_left b 1 (List.map ( * ) a)

let e : 'a todo = let x = List.map in x (<)