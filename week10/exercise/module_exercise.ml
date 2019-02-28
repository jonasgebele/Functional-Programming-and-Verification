module type Set = 
  sig
    type t
    val to_string : t -> string
  end

module type Map = 
  sig
    type key
    type value
    type t
    val empty : t
    val set : key -> value -> t -> t
    val get : key -> t -> value
    val get_opt : key -> t -> value option
    val to_string : t -> string
  end

module StringSet : Set =
  struct
    type t = string
    let to_string eingabe = eingabe ^ " war die Eingabe"
  end

module type OrderedSet =
  sig
    include Set
    val compare : t -> t -> int
    (* größer ist positiv, kleiner ist negativ, gleich ist 0 *)
  end

module BTreeMap (KeySet : OrderedSet) (ValueSet : Set) : Map =
  struct
    type key = KeySet.t
    type value = ValueSet.t
    type t = Empty | Node of (key * value) * t * t

    let empty = Empty
    let rec set key value t =
      match t with
      | Empty -> Node ((key, value), Empty, Empty)
      | Node((k,v), lt, rt) -> 
        if (KeySet.compare k key) < 0 then Node((k,v), (set key value lt), rt) else
        if (KeySet.compare k key) > 0 then Node((k,v), lt, (set key value rt)) else
        t
    let rec get key t =
      match t with
      | Empty -> raise Not_found
      | Node ((k,v), l, r) ->
        if (KeySet.compare k key) < 0 then get key l else
        if (KeySet.compare k key) > 0 then get key r else
        v

    let rec get_opt key t = 
      match t with
      | Empty -> None
      | Node ((k,v), l, r) ->
        if (KeySet.compare k key) < 0 then get_opt key l else
        if (KeySet.compare k key) > 0 then get_opt key r else
        Some v

    let to_string t = "das sollte eine bestimmte Funktion sein..."
  end

module StringSet : Set =
  struct
    type t = string
    let to_string t = t
  end

module IntSet : OrderedSet = 
  struct
    type t = int
    let compare k1 k2 = compare
    let to_string t = (int_of_string)
  end

module IntStringMap = (IntSet) (StringSet)
module IntIntMap = BTreeMap (IntSet) (IntSet)