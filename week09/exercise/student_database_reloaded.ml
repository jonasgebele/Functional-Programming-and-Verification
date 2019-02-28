type student =
    {
        first_name : string;
        last_name : string;
        id : int;
        semester : int;
        grades : (int * float) list
    }

type database = student list

exception Corrupt_database_file

let store_db filename db =
    let file =
        open_out filename
    in
    let write_in_grades grade =
        Printf.fprintf file "%d;%f\n" (fst grade) (snd grade)
    in
    let write_in_students student = 
        Printf.fprintf file "%s;%s;%d;%d\n" student.first_name student.last_name student.id student.semester
        ;
        List.iter write_in_grades student.grades
    in
    List.iter write_in_students db;
    close_out file

let load_db filename =
    let file = 
        open_in filename 
    in
    let rec read_grades gc grades =
        if gc <= 0 then List.rev grades else
            try
                let line = 
                    input_line file 
                in
                match String.split_on_char ';' line with
                | [course_s;grade_s] ->
                    let course,grade = (try
                    int_of_string course_s, float_of_string grade_s
                    with _ -> raise Corrupt_database_file) in
                    if course < 0 || grade < 1.0 || grade > 5.0 then raise Corrupt_database_file
                    else read_grades (gc-1) ((course,grade)::grades)
                | _ -> raise Corrupt_database_file
            with End_of_file -> raise Corrupt_database_file
    in
    let rec read_students db =
        try
            let line = 
                input_line file  
            in
            match String.split_on_char ';' line with
            | ""::_ | _::""::_ -> raise Corrupt_database_file
            | [first_name;last_name;id_s;sem_s;gc_s] ->
              let id,semester,gc = try
                int_of_string id_s, int_of_string sem_s, int_of_string gc_s
                with _ -> raise Corrupt_database_file in
                if id < 0 || semester < 0 || gc < 0 || gc > 100 then raise Corrupt_database_file
                else if List.find_opt (fun s -> s.id = id) db <> None then raise Corrupt_database_file
                else
                let grades = read_grades gc [] in
                read_students ({ first_name; last_name; id; semester; grades }::db)
            | _ -> raise Corrupt_database_file
        with End_of_file -> db
    in
        try
            let db = read_students [] |> List.rev in
            close_in file;
            db
        with e -> close_in file; raise e