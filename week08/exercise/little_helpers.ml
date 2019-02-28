(* (%) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let (%) a b c = a (b c);;

(* (@@) : ('a -> 'b) -> 'a -> 'b *)
let (@@) a b = a b;;

(* (|>) : 'a -> ('a -> 'b) -> 'b *)
let (|>) a b = b a;; 

(* When is it possible to derive the implementation from the type? *)
(* Assuming there are no Side-Effects, a pure function x -> y has |x| * |y| possible Implementations.
where |x| indicates the number of values of type x *)
let implementations = [
  (fun _ -> true);
  (fun _ -> false);
  (fun x -> x);
  (fun x -> not x)
]

(* Give Examples where these Implementations could be used *)

List.map (string_of_int % fst) [(1, 'a'); (2, 'b'); (3, 'c')];;
(* instead of *)
List.map (fun x -> string_of_int (fst x)) [(1, 'a'); (2, 'b'); (3, 'c')];;


String.concat "" @@ List.map string_of_int
  @@ List.map fst [(1, 'a'); (2, 'b'); (3, 'c')];;
(* instead of *)
String.concat "" (List.map string_of_int (
  List.map fst [(1, 'a'); (2, 'b'); (3, 'c')]));;