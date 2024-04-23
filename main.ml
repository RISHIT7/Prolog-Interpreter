open Interpreter;;
open Lexer;;
open Parser;;

if Array.length Sys.argv <> 2 then begin
  Printf.printf "Usage: %s <input_file>\n" Sys.argv.(0);
  exit 0;
end
;;

let init_prog = Parser.program Lexer.token (Lexing.from_channel (open_in Sys.argv.(1)));;
let counter = ref 1;;
let new_prog = modifyInitialProg init_prog 1;;

Printf.printf "%s Loaded Successfully!!\n" Sys.argv.(1);;

try
  while (true) do
    Printf.printf "%d ?- " !counter;
    let line = read_line() in
    counter := !counter + 1;
    if line = "halt." then exit 0
    else try
      let goal = Parser.goal Lexer.token (Lexing.from_string line) in
      match (interpret_goal goal new_prog) with
          (true, _) -> Printf.printf "true.\n"
        | (false, _) -> Printf.printf "false.\n";
    with e -> Printf.printf "%s\n" (Printexc.to_string e)
  done
with _ -> print_string "\n% halt\n"