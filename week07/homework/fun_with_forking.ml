let length liste = List.fold_left (fun a x -> a + 1) 0 liste;;

let longest_list liste_von_listen =
  List.fold_left (fun a x -> 
    if List.length x > List.length a then x else a  
  ) [] liste_von_listen

let computes_some_dope_shit1 liste =
  List.fold_left (fun a x ->
    match x with 
    | (x, y) -> List.append [(y, x)] a 
    | _ -> failwith "falsches Format"
  ) [] liste

let computes_some_dope_shit2 liste =
  List.fold_left (fun a (x,y) -> a @ [(y, x)]) [] liste

let mix_up_order1 liste =
  List.fold_left (fun a x -> 
    x :: List.rev a
  ) [] liste

let compute_function liste =
  List.fold_left (fun a (k, v) -> fun x -> if x = k then v else a x) (fun _ -> 0) liste