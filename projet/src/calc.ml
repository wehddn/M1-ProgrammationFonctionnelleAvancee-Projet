open Syntax
open Eval

let menu_evaluate str =
  let ast = Parser.expr Lexer.token (Lexing.from_channel str) in
  let result = eval ast in
  print_float result; print_newline ()

let rec menu () =
  print_endline "Choose an action:";
  print_endline "1. Evaluate";
  print_endline "2. Do something else";
  print_endline "0. Exit";
  match read_int () with
  | 0 -> print_endline "Exiting..."
  | 1 -> 
    print_endline "Evaluate : ";
    menu_evaluate stdin;
    menu ()
  | 2 -> print_endline "Performing action 2"; menu ()
  | _ -> print_endline "Invalid choice"; menu ()
  
let () =
  menu ()