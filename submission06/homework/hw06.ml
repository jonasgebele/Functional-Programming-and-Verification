let todo _ = failwith "TODO"

(* 6.5: type definitions *)
type nat = Zero | Succ of nat


(* 6.6: type definitions *)
type quadtree_node = NoPoint 
                   | Point of int * int
                   | QNode of quadtree_node (* bottom left *)
                            * quadtree_node (* top left *)
                            * quadtree_node (* bottom right *)
                            * quadtree_node (* top right *)
type quadtree = { width:int; height:int; root:quadtree_node }

(* 6.6: utilitiy functions *)
(* print a graphical representation (svg) of a quadtree (2. argument) to a file (1. argument) *)
let print_quadtree filename qtree =
  let file = open_out filename in 
  let rec impl (x1, y1, x2, y2) = function NoPoint -> ()
    | Point (x,y) -> Printf.fprintf file "<circle cx=\"%d\" cy=\"%d\" r=\"1\" fill=\"black\"/>\n" x (qtree.height - y)
    | QNode (nn, np, pn, pp) -> 
      let xmid = (x1 + x2) / 2 in 
      let ymid = (y1 + y2) / 2 in 
      Printf.fprintf file "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"1\"/>\n" 
        x1 (qtree.height-ymid) x2 (qtree.height-ymid);
      Printf.fprintf file "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"1\"/>\n" 
        xmid (qtree.height -y1) xmid (qtree.height-y2);
      impl (x1, y1, xmid, ymid) nn; 
      impl (x1, ymid, xmid, y2) np; 
      impl (xmid, y1, x2, ymid) pn; 
      impl (xmid, ymid, x2, y2) pp
  in
  Printf.fprintf file "<?xml version=\"1.0\" standalone=\"no\"?>\n
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n
    <svg viewBox = \"0 0 %d %d\">\n
    <rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"white\"/>\n" qtree.width qtree.height
    qtree.width qtree.height;
  impl (0, 0, qtree.width, qtree.height) qtree.root;
  Printf.fprintf file "</svg>";
  close_out file


(* 6.7 definitions *)
type unary_op = Neg
type binary_op = Add | Sub | Mul | Div
type rat = int * int (* num, denom *)
type expr = Const of rat
          | UnOp of unary_op * expr
          | BinOp of binary_op * expr * expr
	

(* 6.8: type definitions *)
type tree = Empty 
          | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop

(* 6.8: utilitiy functions *)
(* print a graphical representation (dot) of a binary tree (2. argument) to a file (1. argument) *)
let print_tree filename btree = 
  let file = open_out filename in
  Printf.fprintf file "digraph Tree {\n";
  let rec print next_id = function Empty -> 
    Printf.fprintf file "\tn%d[shape=rectangle,label=\"\"];\n" next_id; next_id + 1, next_id
  | Node (x, l, r) ->
    let node_id = next_id in
    Printf.fprintf file "\tn%d[label=\"%d\"];\n" node_id x;
    let next_id, lid = print (next_id + 1) l in
    let next_id, rid = print next_id r in 
    (Printf.fprintf file "\tn%d -> n%d[label=\"L\"];\n" node_id lid);
    (Printf.fprintf file "\tn%d -> n%d[label=\"R\"];\n" node_id rid);
    next_id, node_id
  in
  ignore(print 0 btree);
  Printf.fprintf file "}";
  close_out file


(*****************************************************************************)
(**************************** HOMEWORK STARTS HERE ***************************)
(*****************************************************************************)
(* Assignment 6.5 [3 points] *)
let rec int_to_nat : int -> nat =
  fun c ->
    if c > 0 then Succ ( int_to_nat (c-1))
    else
      if c = 0 then Zero
      else failwith "Negative Numbers not possible"

let rec nat_to_int : nat -> int =
  fun n ->
    match n 
      with Zero -> 0
      | Succ (x) -> 1 + nat_to_int (x)

let rec add : nat -> nat -> nat =
  fun n m -> 
    match n 
      with Zero -> m
      | Succ (x) -> add x (Succ (m))

let rec mul : nat -> nat -> nat =
  fun n m ->
    match n
      with Zero -> Zero
      | Succ (x) -> add m (mul x m)

let rec pow : nat -> nat -> nat =
  fun n m ->
    match m
      with Zero -> (Succ(Zero))
      | Succ (x) -> mul n (pow n x)

let leq : nat -> nat -> bool =
  fun a b -> 
    if a > b then false else true


(*****************************************************************************)
(* Assignment 6.6 [6 points] *)
let insert : int * int -> quadtree -> quadtree =
  fun (x, y) rect ->
    match rect.root
      with NoPoint ->  {width = rect.width; height = rect.height; root = (Point(x, y)) }
      | Point(_,_) ->  {width = rect.width; height = rect.height; root = (Point(x, y)) }
      | _          ->  {width = rect.width; height = rect.height; root = (Point(x, y)) }


(*****************************************************************************)
(* Assignment 6.6 [4 points] *)
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


(*****************************************************************************)
(* Assignment 6.8 [7 points] *)
let crawl = todo


(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)
(* example inputs, you may use them to test your implementations,
   but [do not change] *)
let a66_t = { width=16; height=16; root=NoPoint }

let a67_ex1 = BinOp (Mul, BinOp (Sub, Const (3, 5), Const (2, 1)), BinOp (Div, Const (3, 2), Const (7, 5)))
let a67_ex2 = BinOp (Add, UnOp (Neg, a67_ex1), BinOp (Div, Const (7, 1), Const (2, 1)))

let a68_t_l = Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty))
let a68_t_r = Node (6, Node (5, Empty, Empty), Node (7, Empty, Empty))
let a68_t = Node (4, a68_t_l , a68_t_r)

(*****************************************************************************)
(* TESTS [do not change] *)
let (=~) (n,d) (n',d') =
  let k, n = if n < 0 then -1, -n else 1, n in 
  let k, d = if d < 0 then -k, -d else k, d in
  let rec gcd a b = 
    if b = 0 then a else gcd b (a mod b)
  in
  let g = gcd n d in 
  (n',d') = (k * n / g, d / g)
let insert_points = List.fold_left (fun t p -> insert p t) a66_t
let tests = [
  (* tests for 6.5 *)
  __LINE_OF__ (fun () -> (int_to_nat 0) = Zero);
  __LINE_OF__ (fun () -> (int_to_nat 1) = Succ Zero);
  __LINE_OF__ (fun () -> (int_to_nat 3) = Succ (Succ (Succ Zero)));
  __LINE_OF__ (fun () -> (nat_to_int Zero) = 0);
  __LINE_OF__ (fun () -> (nat_to_int (Succ Zero)) = 1);
  __LINE_OF__ (fun () -> (nat_to_int (Succ (Succ (Succ Zero)))) = 3);
  __LINE_OF__ (fun () -> (add Zero Zero) = Zero);
  __LINE_OF__ (fun () -> (add (Succ Zero) (Succ Zero)) = Succ (Succ Zero));
  __LINE_OF__ (fun () -> (add (Succ (Succ Zero)) (Succ (Succ Zero))) = Succ (Succ (Succ (Succ Zero))));
  __LINE_OF__ (fun () -> (mul Zero Zero) = Zero);
  (* tests for 6.6 *)
  __LINE_OF__ (fun () -> (insert_points [5,5]).root = Point (5,5));
  __LINE_OF__ (fun () -> (insert_points [5,5;5,5]).root = Point (5,5));
  __LINE_OF__ (fun () -> (insert_points [8,2;8,12]).root = QNode (NoPoint, NoPoint, Point (8,2), Point (8, 12)));
  __LINE_OF__ (fun () -> (insert_points [8,8;0,0;8,8]).root = QNode (Point (0,0), NoPoint, NoPoint, Point (8, 8)));
  __LINE_OF__ (fun () -> (insert_points [4,4;12,12]).root = QNode (Point (4,4), NoPoint, NoPoint, Point (12,12)));
  __LINE_OF__ (fun () -> (insert_points [4,4;4,12;12,12]).root = QNode (Point (4,4), Point (4, 12), NoPoint, Point (12, 12)));
  __LINE_OF__ (fun () -> (insert_points [6,6;2,2]).root = QNode (QNode (Point (2,2), NoPoint, NoPoint, Point (6,6)), NoPoint, NoPoint, NoPoint));
  __LINE_OF__ (fun () -> (insert_points [2,14;6,11;11,2;14,6]).root = QNode (NoPoint, QNode (NoPoint, Point (2,14), Point (6, 11), NoPoint), QNode (Point (11,2), NoPoint, NoPoint, Point(14,6)), NoPoint));
  (* tests for 6.7 *)
  __LINE_OF__ (fun () -> (eval_expr (Const (2, 3))) =~ (2, 3));
  __LINE_OF__ (fun () -> (eval_expr (UnOp (Neg, Const (4, 5)))) =~ (-4, 5));
  __LINE_OF__ (fun () -> (eval_expr (UnOp (Neg, UnOp (Neg, Const (12, 3))))) =~ (4, 1));
  __LINE_OF__ (fun () -> (eval_expr (BinOp (Add, Const (1, 4), Const (1, 8)))) =~ (3, 8));
  __LINE_OF__ (fun () -> (eval_expr (BinOp (Sub, Const (1, 4), Const (1, 8)))) =~ (1, 8));
  __LINE_OF__ (fun () -> (eval_expr (BinOp (Mul, Const (3, 4), Const (1, 8)))) =~ (3, 32));
  __LINE_OF__ (fun () -> (eval_expr (BinOp (Div, Const (3, 4), Const (1, 8)))) =~ (6, 1));
  __LINE_OF__ (fun () -> (eval_expr a67_ex1) =~ (-3,2));
  __LINE_OF__ (fun () -> (eval_expr a67_ex2) =~ (5, 1));
  (* tests for 6.8 *)
  __LINE_OF__ (fun () -> (crawl [New 3] Empty) = Node (3, Empty, Empty));
  __LINE_OF__ (fun () -> (crawl [New 3] a68_t) = Node (3, Empty, Empty));
  __LINE_OF__ (fun () -> (crawl [New 3; Right; New 2] Empty) = Node (3, Empty, Node (2, Empty, Empty)));
  __LINE_OF__ (fun () -> (crawl [Left; New 3] a68_t) = Node (4, Node (3, Empty, Empty), a68_t_r));
  __LINE_OF__ (fun () -> (crawl [Right; New 3] a68_t) = Node (4, a68_t_l, Node (3, Empty, Empty)));
  __LINE_OF__ (fun () -> (crawl [Left; Delete] a68_t) = Node (4, Empty, a68_t_r));
  __LINE_OF__ (fun () -> (crawl [Left; Delete; New 8] a68_t) = Node (4, Node (8, Empty, Empty), a68_t_r));
  __LINE_OF__ (fun () -> (crawl [Left; Push; Right; Pop] a68_t) = Node (4, Node (2, Node (1, Empty, Empty), Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty))), a68_t_r));
  __LINE_OF__ (fun () -> (crawl [Left; Up; New 3] a68_t) = Node (3, Empty, Empty));
  __LINE_OF__ (fun () -> (crawl [Left; Right; Up; Left; Up; Up; New 3] a68_t) = Node (3, Empty, Empty));
  __LINE_OF__ (fun () -> (crawl [Left; Push; Up; Right; Push; Up; Left; Pop; Up; Right; Pop] a68_t) = Node (4, a68_t_r, a68_t_l));  
]

let () =
  let rec input_lines ch =
    (try Some (input_line ch) with _ -> None) (* catch stupid EOF exception *)
    |> function Some line -> line :: input_lines ch | None -> []
  in
  let lines = input_lines (open_in __FILE__) in
  let open List in
  let open Printf in
  let fail l =
    let line = nth lines (l-1) in
    let test = String.sub line 25 (String.length line - 27) in
    printf "test \027[31;m%s\027[0;m (line %d) failed!\n" test l;
  in
  let test (l, t) =
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)
