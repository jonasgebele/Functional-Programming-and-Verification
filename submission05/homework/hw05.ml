let todo _ = failwith "TODO"

(* Existing definitions from tutorial assignments *)
type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grades : (int * float) list;
}

type database = student list

let insert s db = s::db

let rec find_by_id id db = match db with [] -> []
  | x::xs -> if x.id = id then [x] else find_by_id id xs

let rec find_by_last_name name db = match db with [] -> []
  | x::xs -> if x.last_name = name 
    then x::find_by_last_name name xs
    else find_by_last_name name xs


(*****************************************************************************)
(**************************** HOMEWORK STARTS HERE ***************************)
(*****************************************************************************)

(*****************************************************************************)
(* Assignment 5.5 [6 Points] *)
let rec remove_by_id : int -> student list -> student list =
  fun id db ->
    match db
      with [] -> []
      | x :: xs ->
        if x.id = id 
        then remove_by_id id xs
        else x::remove_by_id id xs
;;

let rec count_in_semester : int -> student list -> int =
  fun semester db ->
    match db 
      with [] -> 0
      | x :: xs ->
      if x.semester = semester 
      then 1 + count_in_semester semester xs
      else 0 + count_in_semester semester xs
;;

let get_tupel_inhalt : int * float -> float = fun (_, a) -> a ;;

let rec summe_noten : (int * float) list -> float = 
  fun a -> 
    match a
    with [] -> 0.0
    | y :: ya -> get_tupel_inhalt y +. summe_noten ya
;; 

let rec anzahl_noten : (int * float) list -> int =
  fun a ->
    match a
    with [] -> 0
    | y :: ya -> 1 + anzahl_noten ya
;;

let rec student_avg_grade : int -> database -> float =
  fun id db ->
  match db
    (* Bei leerer Liste - Abbrechen *)
    with [] -> 0.0
    | x :: xs ->
      if x.id = id 
      
      (* ID passt - Druchschnitt berechnen *)
      then
        (* Berechne Summe aller Noten *) (* Geteilt durch *) (* Summe aller Noten *)
        (summe_noten x.grades) /. (float_of_int (anzahl_noten x.grades))

      (* ID nicht passt - Weitersuchen *)
      else student_avg_grade id xs
;;

let get_tupel_inhalt_kurs : int * float -> int = fun (a, _) -> a ;;

let rec note_des_schuelers : int -> (int * float) list -> float =
  fun course notenliste ->
    match notenliste
      with [] -> 0.0
      | x :: xs -> 
        if get_tupel_inhalt_kurs x = course
        then  get_tupel_inhalt x
        else  note_des_schuelers course xs
;;

let rec summe_aller_noten_eines_kurses_berechnen : int -> database -> float =
  fun course db ->
  match db

    (* Keine Schüler *)
    with [] -> 0.0

    (* Schüler existieren *)
    | x :: xs ->       
      note_des_schuelers course x.grades +. summe_aller_noten_eines_kurses_berechnen course xs
;;

let rec hat_dieser_schueler_den_kurs_belegt  : int -> (int * float) list -> int =
  fun course notenliste ->
    match notenliste
      with [] -> 0
      | x :: xs -> 
        if get_tupel_inhalt_kurs x = course
        then  1
        else  0
;;

let rec wie_oft_wurde_kurs_belegt : int -> database -> int =
  fun course db ->
    match db
      with [] -> 0
      | x :: xs -> (hat_dieser_schueler_den_kurs_belegt course x.grades) + wie_oft_wurde_kurs_belegt course xs
;;

let rec course_avg_grade : int -> database -> float = 
  fun course db ->
    (summe_aller_noten_eines_kurses_berechnen course db) /. (float_of_int (wie_oft_wurde_kurs_belegt course db))
;;


(*****************************************************************************)
(* Assignment 5.6 [3 Points] *)
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


(*****************************************************************************)
(* Assignment 5.7 [3 Points] *)
let rec foo : int -> int -> bool -> int =
  fun x y b -> 
  if x > y
  then
    foo y x b
  else
    if x < y
    then
      if b
      then
        foo (x+1) y (not b)
      else 
        foo x (y-1) (not b)
    else x
;;

(*****************************************************************************)
(* Assignment 5.8 [4 Points] *)
(* Copyied the this Function because it is in the OCaml Standard Library *)
let length list =
  let rec aux n = function
    | [] -> n
    | _::t -> aux (n+1) t
  in aux 0 list
;;

let rec eval_poly : float -> float list -> float =
  fun x poly ->
    match poly
      with [] -> 0.0
      | c::ca -> c *. (x ** float_of_int (length ca)) +. eval_poly x ca
;;


let rec derive_poly : float list -> float list =
  fun poly ->
    match poly
      with [] -> []
      | y::[] -> []
      | x::xs -> x *. (float_of_int (length xs)) :: derive_poly xs
;;


(*****************************************************************************)
(* Assignment 5.9 [4 Points] *)
let lt_seq l = todo()


(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)
(* example inputs, you may use them to test your implementations,
   but [do not change] *)
let a55_ex1 = [
  { first_name = "Anton"; last_name = "Maier"; id=173; semester=3; grades=[1, 1.7; 4, 2.3; 18, 3.0] };
  { first_name = "Betty"; last_name = "Schmidt"; id=418; semester=1; grades=[] };
  { first_name = "Carla"; last_name = "Kurz"; id=223; semester=2; grades=[1, 4.0; 3, 1.0; 7, 1.3; 12, 1.0] };
  { first_name = "Denis"; last_name = "Uler"; id=19; semester=3; grades=[1, 2.2; 7, 1.0; 8, 5.0] }
]

type 'a a56_test_input = { l1 : 'a list; l2 : 'a list; l3 : 'a list }
let a56_ex1 = { l1 = [0;1;2]; l2 = [10;11;12]; l3 = [20;21;22] }
let a56_ex2 = { l1 = ['a';'b']; l2 = ['A';'B';'C';'D']; l3 = ['!'] }
let a56_ex3 = { l1 = []; l2 = []; l3 = [] }

type a57_test_input = { x : int; y : int; b : bool }
let a57_ex1 = { x = 0; y = 0; b = false }
let a57_ex2 = { x = 3; y = 18; b = true }
let a57_ex3 = { x = 22; y = -4; b = true }
let a57_ex4 = { x = -100; y = -100; b = false }
let a57_ex5 = { x = 7; y = 8; b = false }

type a58_test_input = { poly : float list; x : float }
let a58_ex1 = { poly = [0.]; x = 3. }
let a58_ex2 = { poly = [1.]; x = 8. }
let a58_ex3 = { poly = [1.;0.]; x = -14. }
let a58_ex4 = { poly = [1.;0.;1.;0.]; x = -3.5 }
let a58_ex5 = { poly = [2.;-1.;8.]; x = 10.8 }
let a58_ex6 = { poly = [23.;-103.;13.;1.;0.;0.;52.]; x = 2.2 }

let a59_ex1 = [1;2;2;3;4;2;2;2;3;1]
let a59_ex2 = [true;false;false;true]
let a59_ex3 = ['a';'a';'b';'b';'a';'b';'b';'a';'a']
let a59_ex4 = [0.;1.;2.;0.;2.;1.;2.;1.;2.;3.]


(*****************************************************************************)
(* TESTS [do not change] *)
let (=.) a b = (abs_float (a -. b)) < 0.01
let tests = [
  (* tests for 5.5 *)
  __LINE_OF__ (fun () -> (remove_by_id 42 a55_ex1) = a55_ex1);
  __LINE_OF__ (fun () -> (remove_by_id 173 a55_ex1) = List.tl a55_ex1);
  __LINE_OF__ (fun () -> (remove_by_id 418 a55_ex1) = (List.hd a55_ex1) :: (List.tl (List.tl a55_ex1)));
  __LINE_OF__ (fun () -> (count_in_semester 4 a55_ex1) = 0);
  __LINE_OF__ (fun () -> (count_in_semester 1 a55_ex1) = 1);
  __LINE_OF__ (fun () -> (count_in_semester 3 a55_ex1) = 2);
  __LINE_OF__ (fun () -> (student_avg_grade 42 a55_ex1) =. 0.0);
  __LINE_OF__ (fun () -> (student_avg_grade 418 a55_ex1) =. 0.0);
  __LINE_OF__ (fun () -> (student_avg_grade 173 a55_ex1) =. 7./.3.0);
  __LINE_OF__ (fun () -> (student_avg_grade 223 a55_ex1) =. 7.3/.4.0);
  __LINE_OF__ (fun () -> (course_avg_grade 22 a55_ex1) =. 0.0);
  __LINE_OF__ (fun () -> (course_avg_grade 8 a55_ex1) =. 5.0);
  __LINE_OF__ (fun () -> (course_avg_grade 7 a55_ex1) =. (2.3/.2.));
  __LINE_OF__ (fun () -> (course_avg_grade 1 a55_ex1) =. (7.9/.3.));
  (* tests for 5.6 *)
  __LINE_OF__ (fun () -> (interleave3 a56_ex1.l1 a56_ex1.l2 a56_ex1.l3) = [0;10;20;1;11;21;2;12;22]);
  __LINE_OF__ (fun () -> (interleave3 a56_ex2.l1 a56_ex2.l2 a56_ex2.l3) = ['a';'A';'!';'b';'B';'C';'D']);
  __LINE_OF__ (fun () -> (interleave3 a56_ex3.l1 a56_ex3.l2 a56_ex3.l3) = []);
  (* tests for 5.7 *)
  __LINE_OF__ (fun () -> ((foo a57_ex1.x a57_ex1.y a57_ex1.b) = 0));
  __LINE_OF__ (fun () -> ((foo a57_ex2.x a57_ex2.y a57_ex2.b) = 11));
  __LINE_OF__ (fun () -> ((foo a57_ex3.x a57_ex3.y a57_ex3.b) = 9));
  __LINE_OF__ (fun () -> ((foo a57_ex4.x a57_ex4.y a57_ex4.b) = -100));
  __LINE_OF__ (fun () -> ((foo a57_ex5.x a57_ex5.y a57_ex5.b) = 7));
  (* tests for 5.8 *)
  __LINE_OF__ (fun () -> (eval_poly a58_ex1.x a58_ex1.poly) =. 0.);
  __LINE_OF__ (fun () -> (eval_poly a58_ex2.x a58_ex2.poly) =. 1.);
  __LINE_OF__ (fun () -> (eval_poly a58_ex3.x a58_ex3.poly) =. -14.);
  __LINE_OF__ (fun () -> (eval_poly a58_ex4.x a58_ex4.poly) =. -46.375);
  __LINE_OF__ (fun () -> (eval_poly a58_ex5.x a58_ex5.poly) =. 230.48);
  __LINE_OF__ (fun () -> (eval_poly a58_ex6.x a58_ex6.poly) =. -2333.322368);
  __LINE_OF__ (fun () -> (derive_poly a58_ex1.poly) = []);
  __LINE_OF__ (fun () -> (derive_poly a58_ex2.poly) = []);
  __LINE_OF__ (fun () -> (derive_poly a58_ex3.poly) = [1.]);
  __LINE_OF__ (fun () -> (derive_poly a58_ex4.poly) = [3.;0.;1.]);
  __LINE_OF__ (fun () -> (derive_poly a58_ex5.poly) = [4.;-1.]);
  __LINE_OF__ (fun () -> (derive_poly a58_ex6.poly) = [138.;-515.;52.;3.;0.;0.]);
  (* tests for 5.9 *)
  __LINE_OF__ (fun () -> (lt_seq a59_ex1) = [2;2;3]);
  __LINE_OF__ (fun () -> (lt_seq a59_ex2) = [true]);
  __LINE_OF__ (fun () -> (lt_seq a59_ex3) = ['a';'b';'b']);
  __LINE_OF__ (fun () -> (lt_seq a59_ex4) = [1.;2.]);
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
    let line = nth lines l in
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

