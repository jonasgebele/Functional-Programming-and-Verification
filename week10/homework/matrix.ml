module type Ring =
  sig
    type t
    val zero : t
    val one : t
    val add : t -> t -> t
    val mul : t -> t -> t
    val compare : t -> t -> int
    val to_string : t -> string
  end

module type Matrix =
  sig
    type elem
    type t
    val create : int -> int -> t
    val identity : int -> t
    val from_rows : elem list list -> t
    val set : int -> int -> elem -> t -> t
    val get : int -> int -> t -> elem
    val transpose : t -> t
    val add : t -> t -> t
    val mul : t -> t -> t
    val to_string : t -> string
  end

module IntRing : Ring =
  struct
    type t = int
    let zero = 0
    let one = 1
    let add a b = a + b
    let mul a b = a * b
    let compare a b = if a < b then -1 else if a > b then 1 else 0
    let to_string a = (string_of_int a)
  end

module FloatRing : Ring =
  struct
    type t = float
    let zero = 0.0
    let one = 1.0
    let add a b = a +. b
    let mul a b = a *. b
    let compare a b = if a < b then -1 else if a > b then 1 else 0
    let to_string a = (string_of_float a)
  end

module type FiniteRing =
  sig
    include Ring
    val elems : t list
  end

module BoolRing : FiniteRing =
  struct
    type t = bool
    let zero = false
    let one = true
    let add a b =
      match (a, b) with
      | true, false -> true
      | false, true -> true
      | false, false -> false
      | true, true -> false
    let mul a b = 
      match (a, b) with
      | false, false -> false
      | true, true -> true
      | false, true -> false
      | true, false -> false
    let compare a b =
      match (a, b) with
      | false, false -> 0 
      | true, true -> 0
      | true, false -> 1
      | false, true -> -1
    let to_string a = (string_of_bool a)
    let elems = [true; false]
  end

module SetRing (D : FiniteRing) : Ring = 
  struct
    type t = D.t list
    let zero = []
    let one = D.elems
    let compare a b =
      let a = 
        List.sort D.compare a 
      in
      let b = 
        List.sort D.compare b 
      in
      let rec impl l1 l2 = 
        match l1, l2 with
        | [],_ | _,[] -> (List.length l1) - (List.length l2)
        | x::xs, y::ys -> 
          let c = 
            D.compare x y 
          in
          if c <> 0 then c else impl xs ys
      in
      impl a b
    let to_string l = "{" ^ (String.concat ", " (List.map D.to_string l)) ^ "}"
    let add a b = List.sort_uniq D.compare (a @ b)
    let mul a b = List.filter (fun x -> List.find_opt (fun y -> D.compare x y = 0) b <> None) a
  end