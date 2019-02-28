let f (a, b) = a + b + 1
(* Eingabe ist ein Tupel von Zahlen, Rückgabe ist eine Zahl *)
let g a b = a + b + 1;;
(* 1. Eingabe ist eine Zahl, Rückgabe ist eine Funktion *)
(* 2. Eingabe ist eine Zahl, Rückgabe ist eine Zahl *)

f;;
g;;

f (3, 5);;
let zwischenfunktion = g 3;;
zwischenfunktion 6;;
(* Prinzip der höheren Ordnung nennt sich Currying *)
(* Argument einer Funktion kann auch wieder selbst eine Funktion sein *)

let apply f a b = f (a, b)
(* Dadurch strukturiert man als Eingabe eine Funktion mit einem Tupel in eine 
Partielle Funktion um *)

let plus (x, y) = x + y;;

apply plus;;

let plus2 = apply plus 2

let plus3 = apply plus 3;;

plus2 (plus3 4)