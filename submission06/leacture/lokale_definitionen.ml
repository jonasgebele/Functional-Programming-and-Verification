(* Lokale Definitionen *)
let x = 5
  in let sq = x * x
    in sq + sq
;;

let faciet n = 
   let rec iter m yet = 
    if m > n
      then yet
      else iter (m + 1) (m * yet)
        in iter 2 1
;;