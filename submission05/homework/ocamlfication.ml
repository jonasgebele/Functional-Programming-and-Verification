let rec foo : int -> int -> bool -> int =
  fun x y b -> 
    
  if x > y

  then
    foo y x b

  else

    if x < y

    then

      if b
      then
        foo (x+1) y (not b)
      else 
        foo x (y-1) (not b)

    else x