type folge = Ende | Dann of (int * folge);;

Dann (1, Dann (2, Ende));;

let rec n_tes =
  function 
    | (_, Ende) -> -1
    | (0, Dann (x, _)) -> x
    | (n, Dann (_, rest)) -> n_tes (n-1, rest)
;;

let rec runter = function
  0 -> Ende
  | n -> Dann (n, runter (n-1))
;;

runter 8;;

None;;

Some 10;;