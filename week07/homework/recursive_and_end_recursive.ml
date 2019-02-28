let rec f a =
  match a with 
  | [] -> a
  | x::xs -> (x+1) :: f xs
(* Nicht End-Rekursiv *)

let rec g a b =
  if a = b then 0 else
    if a < b then g (a+1) b
      else g (a-1) b
(* End-Rekursiv *)

let rec h a b c =
  if b then h a (not b) (c*2) else
    if c > 1000 then a else
    h (a+2) (not b) c * 2
(* Nicht End-Rekursiv, wegen dem *2 am Ende *)

let rec i a =
  function 
  | [] -> a
  | x::xs -> i (i (x,x) [x]) xs
(* End-Rekursiv *)

(* Wenn jedes mögliche Ergebnis Endrekursiv ist, dann ist die Funktion Endrekursiv *)
(* Wenn im Syntaxbaum nur Funktionsaufrufe ganz oben stehen, ist die Funktion
normalerweise auch Endrekursiv *)


