(* Input *)

print_string "Hello World!\n";;
(* Return Valeu is Unit and the Side-Effekt is the Printing on the Screen *)
read_line ();;

(* in order to read from files the file must be opened for reading *)
let infile = open_in "text";;
input_line infile;;
try 
  input_line infile
with 
  End_of_file -> "Another line does not exist!";;
(* Benötigt man den Kanal nicht mehr, sollte man ihn geregelt schließen *)
close_in infile;;
(* 
stdin : in_channel = the standard input as channel
input_char : in_channel -> char = returns the next character of the channel
in_channel_length : in_channel -> int = returns the total length of the channel
*)
stdin;;
input_char;;
in_channel_length;;

(* Output *)

let outfile = open_out "test";;
output_string outfile "Hello ";;
output_string outfile "World\n";;
close_out outfile;;