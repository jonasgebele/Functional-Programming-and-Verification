 
                                    (* 1. *)
                                    (*____*)


(* Konstanten *)
true;;
1;;

(* Variablen *)
let a = 5;;
let my_variable = 15.4;;

(* Unäre Operatoren *)
not true;;
- 3;;

(* Binäre Operatoren *)
1+2;;
4.2 *. 3.7;;
"Hello " ^ "World!";;

(* Tupel *)
(1, 2, 3, 4, 5);;
(1, 1.0, true, "Hallo", [], (1, 1.0));;

(* Records *)
type mensch = {vorname: string; nachname:string; alter:int} (* Der Record Type mit diesen Attributen muss deklariert werden *)
let jonas = {vorname = "Jonas"; nachname = "Gebele"; alter = 21};;

(* Record-Access *)
jonas.vorname;;
jonas.nachname;;
jonas.alter;;

(* Liste *)
2::4::6::8::[];;
[1; 2; 3; 4; 5];;
1::[99; 100];;

(* If-Then Else *)
if 9>0 then "Markus" else "Jonas";;
if 5=5 then 1.0 else 0.5;;

(* Pattern-Matching *)
match [4; 5; 2]
  with [] -> 1
  | x::_ -> x
;;
match (3, 4)
  with (0, 0) -> ""
  | (1, _) -> "x"
  | (_, _) -> "abc"
;;

(* Functions Definition *)
fun x -> 2;;
fun a b c -> a + b - c;;

(* Function Aplication *)
(fun x -> 2) (1::[]);;

(* Variabel Bindung *)
let x = 2 in x*x




                                    (* 2. *)
                                    (*____*)

(* a *) let a = fun x y -> x + 2 in a 3 8 :: [];;
(* b *) ((fun x -> x::[]) (9 - 5), true, ('a', 7));;