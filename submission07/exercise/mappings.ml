let is_empty l = if (l = []) then true else false;;
is_empty[];;
is_empty[1;2;3;4];;

let get k list = List.find_opt (fun (key, v) -> key = k) list;;

let put key value list = (key, value)::list;;
(* eigentlich müssten wir ein vorhandenes überschreiben *)
(* let put k v m = (k,v)::remove k m *)

let contains_key key list = if(get key list = None) then false else true;;
let contains_key_alternative k m = 
  let filter_fun (x, y) =
    x = k in
      List.filter (filter_fun) m <> []
;;
let contains_key_second_alternative k m =
  List.filter (fun (x,y) -> x = k) m <> []
;;
contains_key 24 [(1, 5);(24, 3);(6, 0)];;
contains_key 24 [];;
contains_key 4 [(1,0)];;

let remove k m = List.filter (fun (a,b) ->  a <> k) m;;

let keys list = List.map(fun (k,v) -> k) list;;

let values list = List.map(fun (k, v) -> v) list;;



(* _____________________________________________________________________________ *)

(* Associative List Operators *)
(* -------------------------- *)


List.assoc 1 [(1,2); (3,4); (5,6)];;
(* Assoc liefert den Zugehörigen Wert zu einem Schlüssel wieder 
Wenn es nicht gefunden wurde -> Not_found*)

List.assoc_opt 3 [(1,2); (3,4); (5,6); (7,8)];;
(* Assoc_opt liefert den Zugehörigen Wert zu einem Schlüssel wieder
Wenn es nicht gefunden wurde -> None *)

List.mem_assoc 55 [(44,66);(55,88)];;
List.mem_assoc 66 [(44,66)];;
(* Falls der Key gefunden wurde liefert es true, sonst false *)

List.remove_assoc 1 [(1,2); (2,3); (3,4); (1,2)];;
(* Gibt die Liste ohne das Paar mit dem jeweiligen Schlüsse zurück *)



(* Lists of pairs *)
(* -------------- *)

List.split [(1,6); (2, 7); (3,8); (4,9)];;

