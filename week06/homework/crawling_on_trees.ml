type tree = Empty | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop

let rec crawl command_list tree =

  let rec crawl_with_stack command_list tree stack =

    let move_left tree =
      match tree with
      | Node (a, lt, rt) -> lt
      | Empty -> failwith "can't go any further left" 
    in
    let move_right tree =
      match tree with
      | Node (a, lt, rt) -> rt
      | Empty -> failwith "can't go any further right" 
    in
    let create_new tree new_val =
      Node(new_val, Empty, Empty)
    in
    let delete tree =
      Empty
    in
    let push_on_stack stack tree =
      tree::stack
    in
    let pop_tree_off_stack stack tree =
      match stack with
      | x::xs -> x
      | _ -> failwith "stack-error"
    in
    let clear_stack_through_pop stack =
      match stack with
      | x::xs -> xs
      | _ -> failwith "stack-error"
    in
    let move_up tree stack =
      tree (* hier sollte noch was passenderes implementiert werden *)
    in
  
    match command_list with
    | [] -> tree
    | x :: xs ->
      match x with
      | Left -> crawl_with_stack xs (move_left tree) stack
      | Right -> crawl_with_stack xs (move_right tree) stack
      | New(new_val) -> crawl_with_stack xs (create_new tree new_val) stack
      | Delete -> crawl_with_stack xs (delete tree) stack
      | Push -> crawl_with_stack xs tree (push_on_stack stack tree)
      | Pop -> crawl_with_stack xs (pop_tree_off_stack stack tree) (clear_stack_through_pop stack)
      | Up -> crawl_with_stack xs (tree) (stack)
  in crawl_with_stack command_list tree []