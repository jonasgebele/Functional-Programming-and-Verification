let fac n = 
  let rec fakultaet n acc =
    if n < 2 then acc else fakultaet (n-1) (acc*n)
  in fakultaet n 1

let remove a l =
  let rec loeschen eintrag liste accumulatorListe =
    match liste with
    | x :: xs -> 
      if x = eintrag then loeschen eintrag xs accumulatorListe else
        loeschen eintrag xs (x::accumulatorListe)
    | [] -> List.rev accumulatorListe
  in loeschen a l []

let rec partition f = function
    | [] -> [],[]
    | x::xs -> let a,b = partition f xs
        in if f x then x::a,b else a,x::b

let rec partition =
  fun f list ->
    match list 
      with [] -> [], []
      | x::xs -> let a, b =
        partition f xs in
          if f x then x::a,b else a,x::b

let partition_tail_recursive f l = 
  let rec partition_helper l (a, b) =
    match l
      with [] -> (List.rev a, List.rev b)
      | x::xs -> partition_helper xs (if f x then x::a, b else a, x::b) 
  in partition_helper l ([], [])