open Syntax
open Eval
open Simpl
open Subst
open Derive
open Integ
open Plot
open Graphics

let parse_expression str =
  Parser.expr Lexer.token (Lexing.from_channel str)

let menu_evaluate str =
  print_endline "Saisir l'expression à évaluer :";
  let e = parse_expression str in
  let result = eval e in
  print_float result;
  print_newline ()
  
let menu_simpl str =
  print_endline "Entrez l'expression à simplifier :";
  let e = parse_expression str in
  let result = simpl e in
  print_string (to_string result); 
  print_newline ()


let menu_subst str =
  print_endline "Entrez l'expression à remplacer :";
  let e = parse_expression str in
  print_endline "Entrez la variable à remplacer :";
  let x = read_line () in
  print_endline "Entrez l'expression remplaçante :";
  let e' = parse_expression str in
  let result = subst e x e' in
  print_string (to_string result); print_newline ()
  

let menu_derive str =
  print_endline "Entrez l'expression à dériver :";
  let e = parse_expression str in
  print_endline "Entrer la variable par rapport à laquelle dériver :";
  let x = read_line () in
  let result = derive e x in
  print_string (to_string result); print_newline ()


let menu_integ str =
  print_endline "Entrez l'expression à intégrer : ";
  let e = parse_expression str in
  print_endline "Entrer la variable d'intégration :";
  let x = read_line () in
  print_endline "Entrer la borne inférieure :";
  let a = parse_expression str in
  print_endline "Entrer la borne supérieure :";
  let b = parse_expression str in
  let result = integ e x a b in
  print_float result; print_newline () 
  

let menu_plot str =
  print_endline "Entrez l'expression à tracer :";
  let e = parse_expression str in
  print_endline "Entrez la variable :";
  let x = read_line () in
  print_endline "Voulez-vous entrer des intervalles personnalisés pour les axes ? (o/n)";
  let custom_intervals = read_line () in
  if custom_intervals = "o" || custom_intervals = "O" then (
    print_endline "Entrez la valeur minimale de l'axe des x :";
    let x_min = read_float () in
    print_endline "Entrez la valeur maximale de l'axe des x :";
    let x_max = read_float () in
    print_endline "Entrez la valeur minimale de l'axe des y :";
    let y_min = read_float () in
    print_endline "Entrez la valeur maximale de l'axe des y :";
    let y_max = read_float () in
    plot_expression e x x_min x_max y_min y_max
  ) else (
    plot_expression e x (-5.0) 5.0 (-5.0) 5.0
  )
  

let print_menu () =
  print_endline "Choose an action:";
  print_endline "1. Évaluer";
  print_endline "2. Simplifier";
  print_endline "3. Substituer";
  print_endline "4. Dériver";
  print_endline "5. Intégrer";
  print_endline "6. Tracer";
  print_endline "0. Quitter"

let handle_choice choice =
  match choice with
  | 0 -> print_endline "Quitter..."
  | 1 -> menu_evaluate stdin
  | 2 -> menu_simpl stdin
  | 3 -> menu_subst stdin
  | 4 -> menu_derive stdin
  | 5 -> menu_integ stdin
  | 6 -> menu_plot stdin
  | _ -> print_endline "Choix non valide"

let rec menu () =
  print_menu ();
  try
    let choice = read_int () in
    handle_choice choice;
    match choice with
    | 0 -> ()
    | _ -> menu ()
  with 
  | Failure s -> 
    (match s with
      | "int_of_string" -> print_endline "Choix non valide"
      | s -> print_endline s
    ); menu ()
  | Dune__exe__Lexer.Error s -> print_endline ("Impossible de traiter le symbole " ^ s); menu ()
  | Dune__exe__Parser.Error -> print_endline "Impossible de traiter l'expression"; menu ()
  | _ -> menu ()

let () = menu ()