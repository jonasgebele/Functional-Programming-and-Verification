module Pairs =
  struct
    type 'a pair = 'a * 'a
    let pair (a,b) = (a, b)
    let first (a,b) = a
    let second (a,b) = b
    
    
    let jonas = Pairs.first (1, 2);;

  end
(* the definitions inside the module are not visible outside of the module *)