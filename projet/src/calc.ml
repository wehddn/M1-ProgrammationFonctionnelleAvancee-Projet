open Syntax
open Eval
open Simpl
open Subst
open Derive
open Plot
open Graphics


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
    print_endline "Entrez l'expression à tracer:";
    let e = Parser.expr Lexer.token (Lexing.from_channel str) in
    print_endline "Entrez la variable:";
    let x = read_line () in
    print_endline "Voulez-vous entrer des intervalles personnalisés pour les axes ? (o/n)";
    let custom_intervals = read_line () in
    if custom_intervals = "o" || custom_intervals = "O" then (
      print_endline "Entrez la valeur minimale de l'axe des x:";
      let x_min = read_float () in
      print_endline "Entrez la valeur maximale de l'axe des x:";
      let x_max = read_float () in
      print_endline "Entrez la valeur minimale de l'axe des y:";
      let y_min = read_float () in
      print_endline "Entrez la valeur maximale de l'axe des y:";
      let y_max = read_float () in
      plot_expression e x x_min x_max y_min y_max
    ) else (
      plot_expression e x (-5.0) 5.0 (-5.0) 5.0
    )
  


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
