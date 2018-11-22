let rec eine_liste : 'a list -> 'a list =
  fun a ->
    match a
    with [] -> []
    | x::xa -> x:: eine_liste xa
;;

let rec zwei_listen : 'a list -> 'a list -> 'a list =
  fun a b ->
    match a
    with [] -> eine_liste b
    | x::xa ->
      match b
      with [] -> eine_liste a
      | y::yb ->
        x::y::zwei_listen xa yb
;;

let rec interleave3 : 'a list -> 'a list -> 'a list -> 'a list =
  fun a b c ->
    match a
    with [] -> zwei_listen b c
    | x::xa ->
      match b 
      with [] -> zwei_listen a c
      |y::yb ->
        match c
        with [] -> zwei_listen a b
        | z::zc -> 
          x::y::z:: (interleave3 xa yb zc) 

;;