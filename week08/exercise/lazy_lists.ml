type 'a lazyList = Const of 'a * ( unit -> 'a lazyList)

let rec lnat n =
  Const (n, (fun () -> lnat (n+1)))

let lfib () =
  let rec impl a b =
    Const (a, (fun () -> impl b (b+a)))
  in impl 0 1

let rec ltake n (Const (a, t)) =
  if n <= 0 then [] else a :: ltake (n-1) (t ())

let rec lfilter fu (Const (a, t)) =
  if fu a then Const (a, (fun () -> lfilter fu (t ())))
  else lfilter fu (t ())