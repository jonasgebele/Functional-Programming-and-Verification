type 'a polymorphic_datatype = Leaf of 'a
  | Node of ('a polymorphic_datatype * 'a polymorphic_datatype)
;;
Leaf 1;;
Leaf 1.9;;
Node (Leaf 1, Node(Leaf 2, Leaf 3));;
Node (Leaf ('a', true), Leaf ('b', false));;
Node (Leaf ('a', true), Leaf ('c', true));;

type ('a, 'b) pair_tree = Blatt of ('a * 'b)
| Knoten of (('a, 'b)  pair_tree * ('a, 'b) pair_tree)
;;
Knoten (Blatt (1, "string"), Blatt (3, "hi"));;
Blatt ("jonas", "jonas");;

(* Funktionen auf Polymorphe Datentypen sind meist selbst auch polymorph *)
let rec size =
  fun polymorphic_datatype ->
    match polymorphic_datatype
      with Leaf _ -> 1
      | Node (t1, t2) -> size t1 + size t2
;;
size (Node (Leaf ('a', true), Leaf ('c', true)));;
size (Leaf 1);;
size (Node (Node(Leaf 1, Leaf 2), Node(Leaf 3, Leaf 4)));;
let rec size_pair =
  fun pair_tree ->
    match pair_tree
      with Blatt _ -> 1
      | Knoten (t1, t2) -> (size_pair t1) + (size_pair t2)
;;
size_pair (Knoten (Blatt (1, 1.3), Blatt (3, 4.3)));;

let rec flatten = 
  fun polymorphic_datatype ->
    match polymorphic_datatype
      with Leaf x -> [x]
      | Node (t1, t2) -> flatten t1 @ flatten t2
;;
flatten (Node (Leaf ('a', true), Leaf ('c', true)));;
flatten (Leaf 1);;
flatten (Node (Node(Leaf 1, Leaf 2), Node(Leaf 3, Leaf 4)));;