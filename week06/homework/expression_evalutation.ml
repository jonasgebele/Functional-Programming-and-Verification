type rat = int * int
type unary_op = Neg
type binary_op = Add | Sub | Mul | Div
type expr = Const of rat
  | UnOp of unary_op * expr
  | BinOp of binary_op * expr * expr

let rec eval_expr expression =
  match expression with
  | Const (rat) -> rat
  | UnOp (Neg, expr) -> 
    let (n, d) =
      eval_expr expr
    in -n, d

  | BinOp (binary_op, expr1, expr2) -> 
      let (n1, d1) =
        eval_expr expr1
      in

      let (n2, d2) =
        eval_expr expr2
      in

      match binary_op with
      | Add -> (n1*d2+n2*d1, d1*d2)
      | Sub -> (n1*d2-n2*d1, d1*d2)
      | Mul -> (n1*n2, d1*d2)
      | Div -> (n1*d2, d1*n2)