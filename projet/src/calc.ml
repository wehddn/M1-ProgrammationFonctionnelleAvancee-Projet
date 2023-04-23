open Syntax
open Eval
open Simpl
open Subst
open Derive
open Plot
(*open Graphics*)


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
  let e = Parser.expr Lexer.token (Lexing.from_channel str) in
  print_endline "Entrer la variable à remplacer";
  let x = read_line () in
  print_endline "Saisir l'expression remplaçante";
  let e' = Parser.expr Lexer.token (Lexing.from_channel str) in
  let result = subst e x e' in
  print_string (to_string result); print_newline ()

let menu_derive str =
  print_endline "Saisir l'expression à dériver";
  let e = Parser.expr Lexer.token (Lexing.from_channel str) in
  print_endline "Entrer la variable par rapport à laquelle dériver";
  let x = read_line () in
  let result = derive e x in
  print_string (to_string result); print_newline ()

let menu_plot str =
  print_endline "Enter the expression to plot:";
  let str = read_line () in
  let e = parse_expression str in
  print_endline "Enter the variable to plot against:";
  let x = read_line () in
  print_endline "Enter the lower x bound:";
  let a = read_float () in
  print_endline "Enter the upper x bound:";
  let b = read_float () in
  print_endline "Enter the lower y bound:";
  let c = read_float () in
  print_endline "Enter the upper y bound:";
  let d = read_float () in
  plot e x a b c d;;

let rec menu () =
  print_endline "Choose an action:";
  print_endline "1. Evaluate";
  print_endline "2. Simplify";
  print_endline "3. Substitute";
  print_endline "4. Derive";
  print_endline "5. Plot";
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
    print_endline "Substitute :";
    menu_subst stdin;
    menu ()
  |4 -> 
    print_endline "Derive :";
    menu_derive stdin;
    menu ()
  |5 -> 
    print_endline "Plot :";
    menu_plot stdin;
    menu ()
  | _ -> print_endline "Invalid choice"; menu ()
  
let () =  
    menu ();;
