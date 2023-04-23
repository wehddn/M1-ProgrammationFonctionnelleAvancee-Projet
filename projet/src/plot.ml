open Syntax
open Eval
open Simpl
open Subst
open Derive
open Graphics

let plot_expression e x a b c d =
  let step = 1 in
  let rec plot_x x_val y_val =
    if x_val <= b then begin
      let y = eval (subst e x (Num x_val)) in
      let y' = eval (subst (derive e x) x (Num x_val)) in
      let y_screen = int_of_float (((y -. c) /. (d -. c)) *. float_of_int (size_y () - 1)) in
      let y'_screen = int_of_float (((y' -. c) /. (d -. c)) *. float_of_int (size_y () - 1)) in

      plot x_val y_screen;
      plot x_val y'_screen;
      plot_x (x_val + step) (y_val)
    end
  
  
  in
  let rec plot_y x_val y_val =
    if y_val <= d then begin
      let x = eval (subst e x (Num y_val)) in
      let x' = eval (subst (derive e x) x (Num y_val)) in
      let x_screen = int_of_float (((x -. a) /. (b -. a)) *. float_of_int (size_x () - 1)) in
      let x'_screen = int_of_float (((x' -. a) /. (b -. a)) *. float_of_int (size_x () - 1)) in

      plot x_screen y_val;
      plot x'_screen y_val;
      plot_y x_val (y_val + step)
    end
  in
  if not (is_single_variable e x) then
    failwith "Expression must be a function of a single variable";
  open_graph (Printf.sprintf " %dx%d" (size_x ()) (size_y ()));
  plot_x a 0;
  plot_y 0 c;
  close_graph ()


let command_plot str =   
  let tokens = Lexer.lex str in
  let expr, var = parse_expression (List.nth tokens 0), List.nth tokens 1 in
  match List.length tokens with
  | 2 -> plot_expression expr var (-5.0) 5.0 (-5.0) 5.0
  | 6 ->
    let a, b, c, d = float_of_string (List.nth tokens 2), float_of_string (List.nth tokens 3),
                      float_of_string (List.nth tokens 4), float_of_string (List.nth tokens 5) in
    plot_expression expr var a b c d
  | _ -> failwith "Invalid plot command"