type hex = Digit of int | Letter of char

let char2dez c = if c >= 'A' && c <= 'F' then (Char.code c)-55 else
  if c >= 'a' && c <= 'f' then (Char.code c) - 87 else -1
;; 

char2dez 'F';;

let hex2dez = 
  function 
    Digit n -> n
    | Letter c -> char2dez c
;;

let a = Letter 'a';;

hex2dez a;;