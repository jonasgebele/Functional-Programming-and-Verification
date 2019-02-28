module ListQueue = 
  struct
    type 'a queue = 'a list
    let empty_queue () = []
    let is_empty = 
      function
        | [] -> true 
        | _ -> false
    let enqueue xs y = xs @ [y]
    let dequeue (x::xs) = (x,xs)
  end

module type Queue = 
  sig
    type 'a queue
    val empty_queue : unit -> 'a queue
    val is_empty : 'a queue -> bool
    val enqueue : 'a queue -> 'a -> 'a queue
    val dequeue : 'a queue -> 'a * 'a queue
  end

module Queue : Queue = ListQueue;;