(* Copyied the this Function because it is in the OCaml Standard Library *)
let length list =
  let rec aux n = function
    | [] -> n
    | _::t -> aux (n+1) t
  in aux 0 list
;;

let rec eval_poly : float -> float list -> float =
  fun x poly ->
    match poly
      with [] -> 0.0
      | c::ca -> c *. (x ** float_of_int (length ca)) +. eval_poly x ca
;;


let rec derive_poly : float list -> float list =
  fun poly ->
    match poly
      with [] -> []
      | y::[] -> []
      | x::xs -> x *. (float_of_int (length xs)) :: derive_poly xs
;;