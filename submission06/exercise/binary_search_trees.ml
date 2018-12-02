(* 1. *)
type tree =  Node of int * tree * tree | Leaf
(* Konstruktor muss groß geschreiben sein *)
(* Es wäre theoretisch auch möglich als Menge anstatt einem Tupel, 
allerdings komplizierter dann die Elemente daraus zu filtern, weil 
ja keine Reihenfolge da ist *)
(* type baum = Node of {wert:int; links:tree; rechts:tree} | Leaf *)

(* 2. *)
let t1 =  Node ( 1, Leaf, Leaf)
let t8 =  Node ( 8, Leaf, Leaf)
let t12 = Node (12, Leaf, Leaf)
let t42 = Node (42, t12, Leaf)
let t6 = Node (6, t1, t8)
let t9 = Node (9, t6, t42)

(* 3. *)
(* Hier eine Funktion die zeigt, wie ein "Pattern-Matching für 
rekursive Datentypen geht" *)
(* [1]@[2;3] bindet die beiden Listen aneinander *)
let myVal t = 
  match t 
    with Leaf -> failwith "Der Node hat kein Value"
    | Node(v,l,r) -> v
;;
let rec tolist t = 
  match t
    with Leaf -> []
    | Node (v, l, r) -> tolist l @ [v] @ tolist r
;;

(* 4. *)

let rec insert : int -> tree -> tree =
  fun c t ->
    match t
      with Leaf -> Node (c, Leaf, Leaf)
      | Node (v, l, r) -> 
        if v = c then t else 
          if c > v then Node(v, l, insert c r) else Node(v, insert c l, r)
;;        

(* 5. *)
let rec remove_max t = 
  match t 
    with Leaf -> failwith "unreachable"
    | Node (v, l, Leaf) -> v, l
    | Node (v, l, r) -> 
      let v',r' = remove_max r in v',Node (v, l, r')
;;

let rec remove x t = 
  match t with Leaf -> Leaf
  | Node (v, l, r) -> 
    if x < v then Node (v, remove x l, r)
    else 
      if x > v then Node (v, l, remove x r)
      else 
        if l = Leaf then r 
        else let v',l' = remove_max l in Node (v', l', r)
;;

tolist (remove 9 t9);;