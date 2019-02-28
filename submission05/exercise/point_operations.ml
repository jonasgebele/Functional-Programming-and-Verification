type vector3 = { x:float; y:float; z:float}
type vector3_alternative = float * float * float

let p1 = {x=2.5; y=5.0; z=0.2}
let p2 = {x=0.0; y=0.0; z=13.1}
let p3 = {x=5.4; y=8.2; z=1.0}

let p1_alternative = (2.5, 5.0, 2.0)
let p2_alternative = 0.0, 0.0, 13.1
let p3_alternative = (5.4, -8.2, 1.0)

let vector3_to_string : vector3 -> string = 
  fun p1 -> "(" ^ 
  (string_of_float p1.x) ^ ", " ^ 
  (string_of_float p1.y) ^ ", " ^ 
  (string_of_float p1.z) ^ ")"

let vector3_to_string_alternative : vector3_alternative -> string =
  fun (x, y, z) -> "(" ^
    (string_of_float x) ^ ", " ^
    (string_of_float y) ^ ", " ^
    (string_of_float z) ^ ")"

let vector3_add : vector3 -> vector3 -> vector3 =
  fun v1 v2 ->
    {
      x = v1.x +. v2.x;
      y = v1.y +. v2.y;
      z = v1.z +. v2.z
    }
  
let vector3_add_alternative : vector3_alternative -> vector3_alternative -> vector3_alternative =
  fun (x1, y1, z1) (x2, y2, z2) ->
    (x1 +. x2, y1 +. y2, z1 +. z2)

let vector3_max : vector3 -> vector3 -> vector3 =
  fun p1 p2 ->
    if(p1.x *. p1.x +. p1.y *. p1.y +. p1.z *. p1.z > p2.x *. p2.x +. p2.y *. p2.y +. p2.z *. p2.z)
    then p1
    else p2

let vector3_max_alternative : vector3_alternative -> vector3_alternative -> vector3_alternative =
  fun (x1, y1, z1) (x2, y2, z2) -> 
    if x1 *. x1 +. y1 *. y1 +. z1 *. z1 > x2 *. x2 +. y2 *. y2 +. z2 *. z2
    then (x1, y1, z1)
    else (x2, y2, z2)

;;
vector3_to_string (vector3_add p1 (vector3_max p2 p3))
;;
vector3_to_string_alternative (vector3_add_alternative p1_alternative (vector3_max_alternative p2_alternative p3_alternative))