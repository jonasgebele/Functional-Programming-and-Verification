(* Createn of New Infix Operators *)

let ( @=+=@ ) x f = f + x;;
1 @=+=@ 2;;

(* You can also define your own operator *)
(* Man kann von den folgenden Zeichen wählen:
! $ % & * + - . / : < = > ? @ ^ | ~ 
*)

(* Instead of defining a new function fun a b -> a + b, the operator
(+) can be used when folding over a list: (+) 0 1 *)