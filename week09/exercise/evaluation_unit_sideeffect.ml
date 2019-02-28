(* s.madge.me/BPkPcw *)

let x = print_endline "foo" in x, x
(* X hat den typ unit, rückgabewert ist (unit * unit) und der Seiteneffekt wird nur
einmal ausgegeben, als der seiteneffekt an den Typ gebunden wird *)

let x () = print_endline "foo" in x (), x ()
(* Seiteneffekt wird jedes mal ausgegeben, wenn die Funktion ausgegeben wird *)