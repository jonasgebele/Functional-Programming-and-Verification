type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grades : (int * float) list
}

type database = student list

let insert : student -> database -> database = 
  fun s db -> s::db

let rec find_by_id : int -> database -> student list =
  fun id student_list ->
    match student_list
      with [] ->  []
      | x::xs -> 
        if x.id = id
        then [x]
        else find_by_id id xs

let rec find_by_last_name : string -> database -> student list =
  fun last_name database ->
    match database
      with [] -> []
      | x::xs -> 
        if x.last_name = last_name
        then x :: find_by_last_name last_name xs
        else find_by_last_name last_name xs

let rec remove_by_id : int -> database -> database =
  fun id database ->
    match database
      with [] -> []
      | x::xs -> 
        if x.id = id
        then remove_by_id id xs
        else x :: remove_by_id id xs

let rec count_in_semester semester database =
  match database
    with [] -> 0
    | x::xs ->
      if x.semester = semester
      then 1 + count_in_semester semester xs
      else 0 + count_in_semester semester xs



let rec student_avg_grade id database =
  
  let rec get_number_of_grades grade_list =
    match grade_list with
      | [] -> 0.0
      | x :: xs -> 1.0 +. get_number_of_grades xs
  in

  let rec get_sum_of_all_grades grade_list =
    match grade_list with
    | [] -> 0.0
    | x :: xs -> (snd x) +. get_sum_of_all_grades xs
  in

  match find_by_id id database with
    | [{ grades = [] }] -> 0.0 
    | [x] -> (get_sum_of_all_grades x.grades) /. (get_number_of_grades x.grades)
    | _ -> 0.0 

let rec course_avg_grade course student_database =

  let rec get_all_grades_from_that_course course student_database =
    match student_database with
    | [] -> []
    | x::xs -> 
      if x.grades ... 
      then (snd x.grades) :: get_all_grades_from_that_course course xs
      else get_all_grades_from_that_course course xs
  in

  let rec get_sum_of_grade_list grade_list =
    match grade_list with
    | [] -> 0.0
    | x::xs -> x +. get_sum_of_grade_list xs
  in

  get_sum_of_grade_list (get_all_grades_from_that_course course student_database)
    /.
  List.length (get_all_grades_from_that_course course student_database) 

(* ______________________________________________________________ *)

let get_tupel_inhalt_kurs : int * float -> int = fun (a, _) -> a

let rec note_des_schuelers : int -> (int * float) list -> float =
  fun course notenliste ->
    match notenliste
      with [] -> 0.0
      | x :: xs -> 
        if get_tupel_inhalt_kurs x = course
        then  get_tupel_inhalt x
        else  note_des_schuelers course xs

let rec summe_aller_noten_eines_kurses_berechnen : int -> database -> float =
  fun course db ->
  match db

    (* Keine Schüler *)
    with [] -> 0.0

    (* Schüler existieren *)
    | x :: xs ->       
      note_des_schuelers course x.grades +. summe_aller_noten_eines_kurses_berechnen course xs

let rec hat_dieser_schueler_den_kurs_belegt  : int -> (int * float) list -> int =
  fun course notenliste ->
    match notenliste
      with [] -> 0
      | x :: xs -> 
        if get_tupel_inhalt_kurs x = course
        then  1
        else  0

let rec wie_oft_wurde_kurs_belegt : int -> database -> int =
  fun course db ->
    match db
      with [] -> 0
      | x :: xs -> (hat_dieser_schueler_den_kurs_belegt course x.grades) + wie_oft_wurde_kurs_belegt course xs

let rec course_avg_grade : int -> database -> float = 
  fun course db ->
    (summe_aller_noten_eines_kurses_berechnen course db) /. (float_of_int (wie_oft_wurde_kurs_belegt course db))