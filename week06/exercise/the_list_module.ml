let hd list =
  match list with
  | x::xs -> x
  | [] -> failwith "Failure hd"

let t1 list =
  match list with
  | x::xs -> xs 
  | [] -> failwith "Failure t1" 

let rec length = function 
  | x::xs -> 1 + length xs
  | [] -> 0

let rec append l1 l2 =
  match l1 with 
  | x::xs -> x :: append xs l2
  | [] -> l2

let rec rev l1 =
  match l1 with
  | x::xs -> rev xs @ [x]
  | [] -> []

(* Beginnend zu zählen bei 0 *)
let rec nth c t1 =
  match t1 with 
  | [] -> failwith "no such element"
  | x::xs -> if c <= 0 then x else nth (c-1) xs