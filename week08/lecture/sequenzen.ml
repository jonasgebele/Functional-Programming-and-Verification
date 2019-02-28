(* Bei Seiteneffekten kommt es auf die Reihenfolge an *)
(* Mehrerer solcher Aktionen kann man mit dem Sequenz-Operator
; ausführen *)
print_string "Hello"; 
print_string " World"; 
print_string "!"; 
3;;

let rec iter f liste =
  match liste with
  | [] -> failwith "irgendwas lief schief"
  | x::[] -> f x
  | x::xs -> f x; iter f xs