open Syntax
open Eval
open Simpl
open Subst


let parse_expression str =
  Parser.expr Lexer.token (Lexing.from_channel str)

let menu_evaluate str =
  let ast = parse_expression str in
  let result = eval ast in
  print_float result; print_newline ()

let menu_simpl str =
  let ast = parse_expression str in
  let result = simpl ast in
  print_string (to_string result); print_newline ()

let menu_subst str =
  print_endline "Saisir l'expression à remplacer";
  let e = Parser.expr Lexer.token (Lexing.from_string  (read_line ())) in
  print_endline "Entrer la variable à remplacer";
  let x = read_line () in
  print_endline "Saisir l'expression remplaçante";
  let e' = Parser.expr Lexer.token (Lexing.from_string  (read_line ())) in
  let result = subst e x e' in
  print_string (to_string result); print_newline ()

let rec menu () =
  print_endline "Choose an action:";
  print_endline "1. Evaluate";
  print_endline "2. Simplify";
  print_endline "3. Substitution";
  print_endline "0. Exit";
  match read_int () with
  | 0 -> print_endline "Exiting..."
  | 1 -> 
    print_endline "Evaluate : ";
    menu_evaluate stdin;
    menu ()
  | 2 -> 
    print_endline "Simplify : ";
    menu_simpl stdin;
    menu ()
  | 3 ->
    print_endline "Substitution :";
    menu_subst stdin;
    menu ()
  | _ -> print_endline "Invalid choice"; menu ()
  
let () =
  menu ()