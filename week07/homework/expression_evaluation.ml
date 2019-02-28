let rec eval_expr (s : state) (e : expr) : value =
  match e with 
  | Const c -> Rat c
  | UnOp (Neg, e) -> (match eval_expr s e with
    | Rat (n, d) -> Rat (-n, d)
    | _ -> failwith "invalid type")
  | BinOp (op, e1, e2) ->
    (match eval_expr s e1, eval_expr s e2 with
    | Rat (n1, d1), Rat (n2, d2) ->
      (match op with
      | Add -> Rat (n1*d2+n2*d1,d1*d2)
      | Sub -> Rat (n1*d2-n2*d1,d1*d2)
      | Mul -> Rat (n1*n2,d1*d2)
      | Div -> Rat (n1*d2,d1*n2))
    | _ -> failwith "invalid type")
  | Var v -> (match s v with Some x -> x
    | None -> failwith "unknown variable")
  | Bind (v, e, b) -> let x = eval_expr s e in
    eval_expr (fun r -> if r = v then Some x else s r) b
  | Func (a, b) -> Fun (a, s, b)
  | App (f, a) -> let v = eval_expr s a in
    (match eval_expr s f with Fun (x, s', b) ->
      eval_expr (fun r -> if r = x then Some v else s' r) b
      | _ -> failwith "not a function")
  | Ite (c, t, e) -> (match eval_expr s c with
    | Rat (0, _) -> eval_expr s e
    | Rat (_, _) -> eval_expr s t
    | _ -> failwith "invalid type")