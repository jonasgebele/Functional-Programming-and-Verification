type rat = int * int (* num, denom *)
type unary_op = Neg
type binary_op = Add | Sub |  Mul | Div
type expr = Const of rat | UnOp of unary_op * expr | BinOp of binary_op * expr * expr

let eval_unop : rat -> rat =
  fun r ->
    match r
      with (a, b) -> (-a, b)

let eval_binop : rat -> rat  -> binary_op -> rat =
  fun r1 r2 b ->
    match r1 with (a1, b1) -> 
      match r2 with (a2, b2) -> 
        if b = Add then (a1*b2+a2*b1, b1*b2) else if 
        b = Sub then (a1*b2-a2*b1, b1*b2) else if
        b = Mul then (a1*a2, b1*b2) else (a1*b2, b1*a2)

let rec eval_expr : expr -> rat =
  fun e -> 
    match e
      with 
        Const(r) -> r
      | UnOp(u, e1) -> eval_unop (eval_expr e1)
      | BinOp(b, e1, e2) -> eval_binop (eval_expr e1) (eval_expr e2) b