(* Ausführliche Schreibweise *)
let rec length : 'a list -> int = 
  fun l -> 
    match l
      with [] -> 0
      | x::xs -> 1 + length xs
;;
length [1;2;3;4;5;6]

(* Syntaktischer Zucker *)
let rec laenge : 'a list -> int =
  function [] -> 0
    | x::xs   -> 1 + laenge xs
;;
laenge [1;2;3;4;5;6;7;8]


(* Mehrere Listen zusammenfügen *)
let rec append_lists =
  fun l p ->
    match l
      with [] -> p
      | x::xs -> x :: append_lists xs p
;;

(* Syntaktischer Zucker mit mehreren Argumenten *)
let rec listen_zusammenfuegen =
  function 
      []      -> fun y -> y
      | x::xs -> fun y -> x :: listen_zusammenfuegen xs y
;;