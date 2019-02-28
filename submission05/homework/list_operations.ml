let rec interleave3 l1 l2 l3 =  
  let rec interleave2 l1 l2 =
    match l1 with 
    | [] -> l2
    | x::xs -> x :: interleave2 l2 xs  
  in
  match l1 with 
  | [] -> interleave2 l2 l3
  | x::xs -> x :: interleave3 l2 l3 xs
;;
interleave3 [1;2;3;5] [13;15;16;17] [99;100]