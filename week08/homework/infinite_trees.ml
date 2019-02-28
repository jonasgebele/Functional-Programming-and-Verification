type 'a lazytree = LNode of 'a * (unit -> 'a lazytree) * (unit -> 'a lazytree)

let rec layer_tree n = 
  LNode (n, (fun () -> layer_tree (n+1)), (fun () -> layer_tree (n+1)))

let rec interval_tree (a, b) =
  LNode ((a, b), (fun () -> interval_tree (a, ((a+.b)/.2.0))), (fun () -> interval_tree (((a+.b)/.2.0), b)))

let top n tree =
  if n <= 0 then Empty else
  let LNode (v, lf, rf) = t in
  Node (v, top (n-1) (lf ())), top (n-1) (rf ()))