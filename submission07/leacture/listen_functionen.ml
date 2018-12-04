
List.map;;

let rec map f =
  fun x ->
    match x 
      with [] -> [] 
      | x::xs -> f x :: map f xs
;;
(* Wenn man eine Liste durchgeht und eine Funktion auf jedes einzelne Listenelement
anwendet.
Deswegen ist ein Argument von map ('a -> 'b), weil dies eine Funktion ist, die
ein Argument eines beliebigen Typs ist und einen beliebigen Typ zurückgibt.
Das zweite Argument von map ist vom Typ 'a Liste.
Der Rückgabewert ist eine Liste vom Typ 'b Liste. *)

let add1 : int -> int = fun x -> x + 1;;
map add1 [1;2;3;4;5;6];;

let ints_of_chars : char list -> int list = List.map (fun x -> (int_of_char x));;
let chars_of_ints : int list -> char list = List.map (fun x -> (char_of_int x));;
let subtraction n = List.map (fun x -> (x - n));;
let add n = List.map (fun x -> x + n);;
let remainder n = List.map (fun x -> (x mod n));;
(* Here we just created 5 List Mappers, which can be used on lists *)





(* _______________________________________________________________________________ *)






List.filter;;

(* The first argument is a function that takes one argument and return 
either true or false. The second argument is a list of type 'a.
The return valeu is a liste also of Type 'a *)
List.filter (fun x -> true) [1;2;3;4;5];;
let greater_than_8 : int list -> int list = 
  List.filter (fun x -> if x > 8 then true else false) 
;;
greater_than_8 [1;2;15;16;3;17];;

let greater_than_n n : 'a list -> 'a list =
  List.filter (fun x -> if x > n then true else false)
;;
greater_than_n 6 [1;2;3;4;5;6;7;8;9];;





(* _________________________________________________________________________________ *)





(* fold is a way to obtain a single value from a list. So if you fold a
list of strings, you will get a string, if you fold an list of integers, you 
will get an int and so on *)

(* when you have a function like
sum a b = a + b
 *)

(* fold_right sum 0 [1;2;3] *)

(* calls the function sum ( sum ( sum 0 1) 2) 3 *)



(* or if  you have to function
concat a b = a ^ b
*)

(* fold_left concat '' ['a'; 'b'; 'c'] 
becomes:
concat( concat( concat '' 'c') 'b') 'a'
gives you
'cba'
*)



(* ______________Folgendes hab ich noch so mitbekommen und weiß ned obs stimmt__ *)
(* fold_right (+) 0 [1;2;3] is the sum of 0+1+2+3 *)
(* fold_left (++) "" ["a", "b", "c"] is the concat *)


List.fold_right;;

let rec fold_right f a = 
  fun x -> 
    match x 
      with [] -> fun b -> b
      | x::xs -> fun b -> f x (fold_right f xs b)
;;

(*
        f
      /   \
    a1      f
          /   \
        a2      f
              /   \
            a3      f
                  /   \
                a4 ...
*)

(* ______________________________________________________________________________ *)

List.fold_left;;

(* fold_left operator operates on lists, as your function does, by applying 
function and accumulating a result in a particular manner.
It takes care of and abstracts the recursion.
It deals with the structure of the list, so you can deal with how to combine
the elements of the list in a particular manner. So ou need to figure out the higher-
order function you want to appy to fold_left that operates on the list in the same way.
 *)

let add = 
  fun x ->
    match x
      with []->0
      | h::t -> List.fold_left (fun h t -> h+t) h t
;; 
 
let zusammenfuegen = List.fold_right (fun x y -> x ^ y) ["hi ";"ich "; "heiße"] "";;

let zusammenfuegen = List.fold_left (fun x y -> x ^ y) "" ["hi ";"ich "; "heiße"];;

(*
# fold_left (+);;
val it : int -> int list -> int = <fun>
*)

(* ______________________________________________________________________________ *)





List.find_opt;;

let rec find_opt f =
  fun x ->
    match x
      with [] -> None
      | x::xs -> 
        if f x then Some x
        else find_opt f xs
;;

let search_for_odd = List.find_opt (fun x -> if (x mod 2) = 0 then true else false) [1;3;4;5];;

(* find_opt : (’a -> bool) -> ’a list -> ’a option *)