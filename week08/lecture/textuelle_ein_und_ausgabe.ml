(* Ein- und Ausgabe wird mit Hilfe von Seiteneffekten realisiert
d.h. mit Funktionen, deren Rückgabewert unit ist *)
print_string;;
read_line;;
let eingabe = read_line ();;
print_string "Konsolenausgabe\n";;

(* Um aus einer Datei zu lese
n, muss man diese zum Lesen öffnen *)
open_in;;
let file_in_this_directory = open_in "textdatei.txt";;

input_line;;
input_line file_in_this_directory;;

(* Kanal muss auch wieder geschlossen werden *)
close_in file_in_this_directory;;

(* Eingabe eines Characters von der Standardeingabe *)
input_char stdin;;

let standardEingabe = stdin;;

input_char standardEingabe;;

let neuer_file_discreptor = open_in "textdatei.txt";;
in_channel_length neuer_file_discreptor;;

let buf = "";;

input stdin 10 2 buf;;