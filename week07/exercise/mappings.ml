let is_empty liste =
  if liste = [] then true else false

let rec get key liste =
  match liste with
  | (k, v) :: xs -> if k = key then Some v else get key xs
  | [] -> None

let put key value liste =
  (key, value) :: liste
  (* eigentlich sollte man erst remove machen *)

let rec contains_key key liste =
  match liste with
  | (k, v) :: xs ->
    if key = k then true else contains_key key xs 
  | [] -> false

let rec remove key liste =
  match liste with
  | (k, v) :: xs ->
    if k = key
    then remove key xs
    else (k, v) :: remove key xs
  | [] -> []

let rec keys1 liste =
  match liste with
  | (k, v) :: xs -> k :: keys1 xs
  | [] -> []

let keys2 liste =
  List.map fst liste

let rec values1 liste = 
  match liste with
  | (k, v) :: xs -> v :: values1 xs
  | [] -> []

let values2 liste =
  List.map snd liste
;;

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

(* ___________________________________________________________ *)

(* Mappings based on functions *)
(* --------------------------- *)

(* is_empty_f : ('k -> 'v option) -> bool *)
(* let is_empty_f = failwith "impossible" *)

let get k m = m k

let put k v m = fun x -> if x = k then Some v else m x

let contains_key k m = (get k m) <> None

let remove k m = fun x -> if x = k then None else m x

(* let keys m = failwith "impossible" *)

(* let values m = failwith "impossible" *)