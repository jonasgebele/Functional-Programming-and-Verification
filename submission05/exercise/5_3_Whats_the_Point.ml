(* Define a suitable data type for your point *)
type vector3 = { x:float; y:float; z:float}

(* Define 3 Points with different values *)
let p1 = {x=2.5; y=5.0; z=0.2}
let p2 = {x=0.0; y=0.0; z=13.1}
let p3 = {x=5.4; y=8.2; z=1.0}

let vector3_to_string : vector3 -> string = 
  fun (p1) -> "(" ^ 
  (string_of_float p1.x) ^ ", " ^ 
  (string_of_float p1.y) ^ ", " ^ 
  (string_of_float p1.z) ^ ")"


let vector3_add : vector3 -> vector3 -> vector3 =
  fun p1 p2 -> {x=(p1.x +. p2.x); y=(p1.y +. p2.y); z=(p1.z +. p2.z)}


let vector3_max : vector3 -> vector3 -> vector3 =
  fun p1 p2 ->
    if(p1.x *. p1.x +. p1.y *. p1.y +. p1.z *. p1.z > p2.x *. p2.x +. p2.y *. p2.y +. p2.z *. p2.z)
    then p1
    else p2


    ;;
vector3_to_string ( vector3_add p1 (vector3_max p2 p3));;