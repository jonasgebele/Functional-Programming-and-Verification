(* In presence of side_effects, ordering matters 
Several actions can be sequenced by means of the sequence operator ;
*)
print_string "Hello";
print_string " ";
print_string "world\n";;
(* ; is a Sequence Operator - the left result is computated first, then the
next expression is evaluted and so on, so the commands are executed in a sequence *)

let rec iter f =
  function
   [] -> ()
   | x::[] -> f x
   | x::xs -> f x; iter f xs