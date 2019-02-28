module type Decons =
  sig 
    type 'a t
    val decons : 'a t -> ('a * 'a t) option
  end

module type GenFold = functor (X:Decons) -> 
  sig
    val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a X.t -> 'b
    val fold_right : ('a -> 'b -> 'b) -> 'a X.t -> 'b -> 'b
    val size : 'a X.t -> 'a list
    val iter : ('a X.t -> 'a list)
  end

module Fold : GenFold = functor (X:Decons) ->
  struct
    let rec fold_left f b t = 
      match X.decons with 
      | None -> b
      | Some (x,t) -> fold_left f (f b x) t
    let rec fold_right f t b = 
      match X.decons t with 
      | None -> b
      | Some (x,t) -> f x (fold_right f t b)
    let size t = fold_left (fun a x -> a+1) 0 t
    let list_of t = fold_right (fun x xs -> x::xs) t []
    let iter f t = fold_left (fun () x -> f x) () t
  end

  module MyQueue = 
    struct 
      open Queue
      type ’a t = ’a queue
      let decons = function
        Queue([],xs) -> (match rev xs with 
        | [] -> None
        | x::xs -> Some (x, Queue(xs,[])))
        | Queue(x::xs,t) -> Some (x, Queue(xs,t))
    end

  module MyAVL = 
    struct 
      open AVL
      type ’a t = ’a avl
      let decons avl = 
        match extract_min avl with 
        | (None,avl) -> None
        | Some (a,avl) -> Some (a,avl)
    end