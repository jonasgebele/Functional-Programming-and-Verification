(* Definition der Aufzaehlungstypen *)
type farbe = Schelle | Herz | Gras | Eichel;;
type wert = Sieben | Acht | Neun | Unter | Ober | Koenig | Zehn | Ass;;

(* Deklaration der Aufzählungstypen *)
Eichel
let gras_unter = (Gras,Unter);;

(* Vergleichen der Konstruktoren *)
if Schelle < Herz then "Herz ist groesser als Schell" else "Schell ist groesser als Herz";;
if Eichel > Herz then "Eichel ist groesser als Herz" else "Herz ist groesser als Eichel";;

(* Pattern Matching mit syntaktischem Zucker auf Konstruktoren *)
let is_trumpf =
  function 
    | (Herz, _)   -> true
    | (_, Unter)  -> true
    | (_, Ober)   -> true
    | (_, _)      -> false
;;

(* Pattern Matching ohne syntaktischem Zucker auf Konstruktoren *)
let ist_trumpf : farbe * wert -> bool =
    fun (a, b) ->
      match (a, b)
        with  (Herz, _)   -> true
        |     (_, Unter)  -> true
        |     (_, Ober)   -> true
        |     (_, _)      -> false
;;

is_trumpf  (Gras, Ass);;
ist_trumpf (Herz, Ober);;

let string_of_farbe : farbe -> string =
    function 
      | Schelle -> "Schell"
      | Herz    -> "Herz"
      | Gras    -> "Laub"
      | Eichel  -> "Eichel"
;;

string_of_farbe Eichel;;
string_of_farbe Gras;;
string_of_farbe Herz;;
string_of_farbe Schelle;;

let sticht : farbe * wert -> bool =
  function 
    | ((f1, Ober), (f2, Ober)) -> f1 > f2
    | ((_,  Ober), _) -> true
    | (_, (Ober), _) -> false
    | ((f1, Unter), (f2, Unter)) -> f1 > f2
    | ((_, Unter), _) -> true
    | (_ ,(_, Unter)) -> false
    | ((Herz, w1), (Herz, w2)) -> w1 > w2
    | ((Herz, _), _) -> true
    | (_, (Herz, _)) -> false
    | ((f1, w1), (f2, w2)) -> if f1 = f2 then w1 > w2 else false
  ;;