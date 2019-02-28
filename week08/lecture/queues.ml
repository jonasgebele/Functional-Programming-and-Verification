(* Datenstruktur, die Folgende Operationen unterstützt:
1. enqueue: 'a -> 'a queue -> 'a queue
2. dequeue: 'a queue -> 'a option * 'a queue
3. is_empty: 'a queue -> bool
4. queue_of_list : 'a list -> 'a queue
5. list_of_queue : 'a queue -> 'a list *)

type 'a queue = Queue of 'a list * 'a list;;

let is_empty =
  fun queue -> 
    match queue 
      with Queue ([], []) -> true
      | _ -> false

let queue_of_list = 
  fun list ->
    Queue (list, [])

let list_of_queue =
  fun queue ->
    match queue
      with Queue(first, []) -> first
      | Queue ([], second) -> second
      | Queue (l1, l2) -> l1 @ List.rev l2
(* Zweie Liste repräsentiert das Ende und muss somit umgedreht werden *)

(* Hinzufügen auf die zweite Liste *)
let enqueue =
  fun x (Queue (first, last)) ->
    Queue (first, x::last)

(* Entnehmen dagegen bei der ersten Liste *)
let dequeue =
  fun queue ->
    match queue
      with Queue ([], last) -> (
        match last
          with [] -> (None, Queue ([], []))
          | x::xs -> (Some x, Queue (xs, []))
      )
      | Queue (x::xs, last) -> (Some x, Queue (xs, last))