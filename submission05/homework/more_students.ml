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