(* Wie bereits gesehen kann auf Daten wie z.B. [1;2;3] zugreifen, ohne 
einen Namen zu vergeben. *)

(* Funktionen sind auch Daten *)

fun x y z -> x + y + z;;
(+);;
( * );;

(* fun leitet die Abstraktion ein *)

(* Rekursive Funktionen können so nicht definiert werden, da ja
kein Name vorhanden ist, der im Rumpf aufgerufen werden kann *)

function None -> 0
  | Some x -> x*x+1;;


(* Namenlose Funktionen werden verwendet, wenn sie nur einmal im Programm
vorkommen. Oft als Argumente für Funktionale *)

List.map (fun x -> x * x) [1;2;3];;

(* Oder um eine Funktion als Ergebnis zurückzuliefern *)
let make_undefined () =
  fun x -> None

let def_one (x,y) = 
  fun x' ->
    if x = x' then Some y
    else None