(* Konstanten *)
true;;
1;;
2.5;;

(* Variablen *)
let a = 5;;
let my_variable = 15.4;;
let example_string = "Hello World";;
(* Unäre Operatoren *)
not true;;
-3;;

(* Binäre Operatoren *)
1+2;;
4.2 *. 3.7;;
"Hello " ^ "World!";;
5 mod 3;;

(* Tupel *)
(1, 2, 3, 4, 5);;
(1, 1.0, true, "Hallo", [], (1, 1.0));;
(1, "Zwei", [3]);;

(* Records *)
type tier = {bezeichnung:string; anzahl_beine:int; hat_fell:bool; gewicht:float; kuerzel:char;}
type mensch = {vorname: string; nachname:string; alter:int} (* Der Record Type mit diesen Attributen muss deklariert werden *)
let jonas = {vorname = "Jonas"; nachname = "Gebele"; alter = 21}
let hase = {bezeichnung = "Hase"; anzahl_beine=4; hat_fell=true; gewicht=100.0; kuerzel='E';};;

(* Record-Access *)
jonas.vorname;;
jonas.nachname;;
jonas.alter;;
hase.hat_fell;;

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
let jonas = fun x -> 2;;
fun a b c -> a + b - c;;

(* Function Aplication *)
(fun x -> 2) (1::[]);;
jonas [4;3;5];;

(* Variabel Bindung *)
let x = 2 in x * x;;

let f = fun a -> a + 2
 in f 7;;