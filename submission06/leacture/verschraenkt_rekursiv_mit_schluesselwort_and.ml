(* Verschränkt Rekursiv - gegenseitig aufrufend *)
let rec even =
  fun n -> 
    if n = 0 
      then "even"
      else odd (n-1)

  and odd = 
  fun n ->
    if n = 0
      then "odd"
      else even (n-1)
;;


(* Oder abgekürzt *)
let rec gerade    n = if n = 0 then "even" else ungerade  (n-1)
    and ungerade  n = if n = 0 then "odd"  else gerade    (n-1) 
;;