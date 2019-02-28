(* An exception is a first class citizen, is also a plain value from a datatype, 
Name of the Datatype is exn *)
(* 
Provided Build-In Constructors by the Ocaml System for Exceptions:
  Division_by_zero                            -> division by 0
  Invalid_argument of string                  -> wrong usage
  Failure of string                           -> general error
  Match_failure of string * int * int         -> incomplete match
  Not_found                                   -> not found
  Out_of_memory                               -> memory exhausted 
  End_of_file                                 -> end of file
  Exit                                        -> for the user
*)
(* Some Constructors have arguments like match_failure of string
Others have no arguments lie division_by_zero *)
(* Exception is an extensible Datatype, you can add Constructors to this datatype *)
Division_by_zero;;
Failure "complete nonsense!";;

(* Ausnahmebehandlung *)
let teile (n, m) = 
  try Some (n / m)
  with Division_by_zero -> None
;;
teile (10, 3);;
teile (10, 0);;

(* Check if a value is in a list *)
let rec member x l = 
  try
    if x = List.hd l then true else member x (List.tl l)
  with 
    Failure _ -> false
;;
(* List.hd is the head of the list
List.tl is the tail of the list *)

member 2 [1;2;3];;
member 4 [1;2;3];;

(* 
try <exp>
with <pat1> -> <exp1> | ... | <patN> -> <expN>

all expressions should have the same type, also the one in the try-block *)

(* Trigger exceptions on own with keyword raise or directly *)
(*1 + (2/0);;*)
1 + raise Division_by_zero;;
(* we can put "raise" in any context and there wont be an type errer
because by evaluting it will automaticelly jump out of the expr...  *)

let f (x, y) = x / (y-1);;
let g (x, y) = 
  try let n = 
    try f (x, y)
    with Division_by_zero -> raise (Failure "Division by zero")
  in string_of_int (n*n)
  with Failure str -> "Error: " ^ str
;;

g (6, 3);;
g (6, 1);;