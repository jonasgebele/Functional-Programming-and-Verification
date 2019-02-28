type folge = Ende | Dann of (int * folge)

let rec length_of_folge folge =
  match folge with
    | Dann (a, b) -> 1 + length_of_folge b
    | Ende -> 0

let rec sum_of_folge = function
  | Ende -> 0
  | Dann (a, weiter) -> a + sum_of_folge weiter

let rec create_folge list length =
  let controlle laenge =
    match laenge with 
      | 0 -> Ende
      | _ -> failwith "irgendwas lief schief :/"
  in  
  match list with
    | [] -> controlle length
    | x::xs -> Dann (x, create_folge xs (length-1))
