type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

let rec insert (eintrag : 'a) (funktion_links_rechts : ('a -> 'a -> int)) (baum :'a tree) =
  match baum with
  | Empty -> Node (eintrag, Empty, Empty)
  | Node (a, t1, t2) -> 
    if funktion_links_rechts eintrag a < 0 
      then Node(eintrag, t1, insert eintrag funktion_links_rechts t2) else 
      if funktion_links_rechts eintrag a > 0 
        then Node(eintrag, insert eintrag funktion_links_rechts t1, t2 ) else
        Node (a, t1, t2)

let rec string_of_tree fu baum =
"\"" ^
  match baum with
  | Empty -> "Empty"
  | Node (a, t1, t2) -> 
    "Node (" ^ (fu a) ^ ", " ^ (string_of_tree fu t1) ^ ", " ^ (string_of_tree fu t2) ^ ")"
^ "\""

let inorder_list baum =
  let rec impl q baum acc = 
    match baum with 
    | Node (x, l, r) ->  impl (Node (x, Empty, r)::q) l acc
    | Empty -> 
      match q with 
      | [] -> acc 
      | Node (x, _, r)::qs -> impl qs r (x::acc)
    | _ -> failwith "unreachable"
  in
  List.rev (impl [] baum [])