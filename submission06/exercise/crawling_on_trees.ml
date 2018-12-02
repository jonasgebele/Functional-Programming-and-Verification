type tree = Empty | Node of int * tree * tree

type command = Left | Right | Up | New of int | Delete | Push | Pop

let rec crawl : command list -> command =
  function
    | x::xs -> if x = Left
;;

crawl [Left; Right; Up];;