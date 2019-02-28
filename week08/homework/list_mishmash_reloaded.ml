let rec interleave3 l1 l2 l3 =
  let rec interleave2 l1 l2 =
    match l1 with [] -> l2
    | x::xs -> x::interleave2 l2 xs
  in
  match l1 with [] -> interleave2 l2 l3
  | x::xs -> x::interleave3 l2 l3 xs

let rec interleave3 : 'a list -> 'a list -> 'a list -> 'a list =
  fun l1 l2 l3 ->

    let rec interleave2 : 'a list -> 'a list -> 'a list =
      fun l1 l2 l3 ->
        match l2
          with [] -> l1
          | x::xs -> interleave2 xs (List.rev (x::(List.rev l1))) 
    in
    
    match l3
      with [] ->interleave2 l1 l2
      | x::xs -> 