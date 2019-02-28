type tree =  Node of int * tree * tree | Leaf
(* Konstruktor muss groß geschreiben sein *)

let t1 =  Node ( 1, Leaf, Leaf)
let t8 =  Node ( 8, Leaf, Leaf)
let t12 = Node (12, Leaf, Leaf)
let t42 = Node (42, t12, Leaf)
let t6 = Node (6, t1, t8)
let t9 = Node (9, t6, t42)
(* or *)
let t2 = 
  Node (9,
    Node(8,
      Node(1,
        Leaf,
        Node(6, Leaf, Leaf)
      ),
      Leaf
    ),
    Node(42,
      Node(12, Leaf, Leaf),
      Leaf
    )
  )

let myVal t = 
  match t 
    with Leaf -> failwith "Der Node hat kein Value"
    | Node(v,l,r) -> v

let rec tolist t = 
  match t
    with Leaf -> []
    | Node (v, l, r) -> tolist l @ [v] @ tolist r

let rec insert value tree =
  match tree with
  | Node (v, tleft, trigth) -> 
    if value > v then Node(v, tleft, insert value trigth) else
    if value < v then Node(v, insert value tleft, trigth) else 
    tree
  | Leaf -> Node(value, Leaf, Leaf)


let rec remove_max t = 
  match t with 
  | Leaf -> failwith "unreachable"
  | Node (v, l, Leaf) -> v, l
  | Node (v, l, r) -> 
      let v',r' = 
        remove_max r 
      in v',Node (v, l, r')

let rec remove x t = 
  match t with 
  | Leaf -> Leaf
  | Node (v, l, r) -> 
    if x < v then Node (v, remove x l, r) else 
    if x > v then Node (v, l, remove x r) else 
      if l = Leaf then r else 
      let v',l' = 
        remove_max l 
      in Node (v', l', r)