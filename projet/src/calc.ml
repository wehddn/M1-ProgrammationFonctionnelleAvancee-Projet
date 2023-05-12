open Syntax
open Eval
open Simpl
open Subst
open Derive
open Integ
open Plot
open Graphics


let last_result = ref None

let parse_expression str =
  Parser.expr Lexer.token (Lexing.from_channel str)

let menu_evaluate str =
  let _ = match !last_result with
  | Some result ->
    print_string "Résultat de la dernière opération : ";
    print_string (to_string result);
    print_newline ()
  | None -> ()  (* Aucun dernier résultat disponible *)
  in
    let ast = parse_expression str in
    let result = eval ast in
    print_float result;
    print_newline ()
  


let menu_simpl str =
  let _ = match !last_result with
  | Some result ->
    print_string "Résultat de la dernière opération : ";
    print_string (to_string result);
    print_newline ()
  | None -> ()  (* Aucun dernier résultat disponible *)
  in
  let ast = parse_expression str in
  let result = simpl ast in
  last_result := Some result;  (* Stocker le dernier résultat *)
  print_string (to_string result); print_newline ()

let menu_subst str =
  let _ = match !last_result with
  | Some result ->
    print_string "Résultat de la dernière opération : ";
    print_string (to_string result);
    print_newline ()
  | None -> ()  (* Aucun dernier résultat disponible *)
  in
  print_endline "Saisir l'expression à remplacer";
  let e = Parser.expr Lexer.token (Lexing.from_channel str) in
  print_endline "Entrer la variable à remplacer";
  let x = read_line () in
  print_endline "Saisir l'expression remplaçante";
  let e' = Parser.expr Lexer.token (Lexing.from_channel str) in
  let result = subst e x e' in
  last_result := Some result;  (* Stocker le dernier résultat *)
  print_string (to_string result); print_newline ()
  
let menu_derive str =
  let _ = match !last_result with
  | Some result ->
    print_string "Résultat de la dernière opération : ";
    print_string (to_string result);
    print_newline ()
  | None -> ()  (* Aucun dernier résultat disponible *)
  in
  print_endline "Saisir l'expression à dériver";
  let e = Parser.expr Lexer.token (Lexing.from_channel str) in
  print_endline "Entrer la variable par rapport à laquelle dériver";
  let x = read_line () in
  let result = derive e x in
  last_result := Some result;  (* Stocker le dernier résultat *)
  print_string (to_string result); print_newline ()

  let menu_integ str =
    print_endline "Saisir l'expression à intégrer";
    let e = Parser.expr Lexer.token (Lexing.from_channel str) in
    print_endline "Entrer la variable d'intégration";
    let x = read_line () in
    print_endline "Entrer la borne inférieure";
    let a = Parser.expr Lexer.token (Lexing.from_channel str) in
    print_endline "Entrer la borne supérieure";
    let b = Parser.expr Lexer.token (Lexing.from_channel str) in
    let result = integ e x a b in
    print_float result; print_newline () 
  
let menu_plot str =
  let _ = match !last_result with
  | Some result ->
    print_string "Résultat de la dernière opération : ";
    print_string (to_string result);
    print_newline ()
  | None -> ()  (* Aucun dernier résultat disponible *)
  in
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
  print_endline "5. Integrale";
  print_endline "6. Plot";
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
  | 5 -> 
    print_endline "Integrate :";
    menu_integ stdin;
  | 6 -> 
    print_endline "Plot :";
    menu_plot stdin;
    menu ()
  | _ -> print_endline "Invalid choice"; menu ()
  
let () =  
    menu ();;
