module Sorting =
  struct
    let single list = List.map (fun x -> [x]) list
    
    let rec merge l1 l2 = 
      match (l1, l2) with
      | ([], _) -> l2
      | (_, []) -> l1
      | (x::xs, y::ys) -> 
        if x<y then x :: merge xs l2 else y :: merge l1 ys
    
    let rec merge_lists liste =
      match liste with
      | [] -> [] 
      | [l] -> [l]
      | l1::l2::ll -> merge l1 l2 :: merge_lists ll

    let sort list =
      let list =
        single list
      in
      let rec doit list =
        match list with
        | [] -> []
        | [l] -> l
        | l -> doit (merge_lists l)
      in
      doit list
  end

module type SortJonas =
  sig
    val merge : 'a list -> 'a list -> 'a list
    val sort : 'a list -> 'a list
  end

module MySort : SortJonas = Sorting


(* Signaturen und Typen *)
module type A1 =
  sig
    val f : 'a -> 'b -> 'b
  end

module type A2 =
  sig
    val f : int -> char -> int
  end

module A =
  struct 
    let f x y = x
  end

module A2 : A2 = A;;
A2.f;;