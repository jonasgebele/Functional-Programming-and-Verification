(* Strukturen *)
module Pairs =
  struct
    type 'a pair = 'a * 'a
    let pair (a, b) = (a, b)
    let first (a, b) = a
    let second (a, b) = b
  end
(* darauf antwortet der Compiler mit einer Signatur *)
(* 
module Pairs :
  sig
    type 'a pair = 'a * 'a
    val pair : 'a * 'b -> 'a * 'b
    val first : 'a * 'b -> 'a
    val second : 'a * 'b -> 'b
  end
*)
module Triples =
  struct
    type 'a triples = Triple of 'a * 'a * 'a
    let first (Triple (a,_,_)) = a
    let second (Triple (_,b,_)) = b
    let third (Triple (_,_,c)) = c
  end
(* 
module Triples :
  sig
    type 'a triple = Triple of 'a * 'a * 'a
    val first : 'a triple -> 'a
    val second : 'a triple -> 'a
    val third : 'a triple -> 'a
  end
*)
module Pairs2 =
  struct
    type 'a pair = bool -> 'a
    let pair (a,b) = fun x -> if x then a else b
    let first ab = ab true
    let second ab = ab false
  end
(*
module Pairs2 :
  sig
    type 'a pair = bool -> 'a
    val pair : 'a * 'b -> bool -> 'a
    val first : (bool -> 'a) -> 'a
    val second : (bool -> 'a) -> 'a
*)

;;
(* Öffnen von Strukturen *)
open Pairs2;;
pair;;
pair (4,3) true;;

(* Bestandteil eines anderes Moduls in ein Modul integrieren *)
module A =
  struct
    let x = 1
  end

module B =
  struct
    open A
    let y = 2
  end

module C =
  struct 
    include A
    include B
  end

(* Geschachtelte Strukturen *)
module Quads =
  struct 
    module Pairs =
      struct
        type 'a pair = 'a * 'a
        let pair (a,b) = (a,b)
        let first (a,_) = a
        let second (_,b) = b
      end
      type 'a quad = 'a Pairs.pair * 'a Pairs.pair
      let quad (a,b,c,d) = Pairs.pair (Pairs.pair (a,b), Pairs.pair (c,d))
      let first q = Pairs.first (Pairs.first q)
      let second q = Pairs.second (Pairs.first q)
      let third q = Pairs.first (Pairs.second q)
      let fourth q = Pairs.second (Pairs.second)
    end