open Syntax
open Eval
open Simpl
open Subst
open Derive
open Graphics

let plot_expression expr var =
  
  let eval_expr x =
    let subst_expr = subst expr var (Num x) in
    eval subst_expr in

  (* Set up graphics window *)
  let width = 800 in
  let height = 600 in
  open_graph (Printf.sprintf " %dx%d" width height);
  set_window_title "Plot";
  set_color black;

  (* Define scaling factors *)
  let x_min = -5.0 in
  let x_max = 5.0 in
  let y_min = -5.0 *. float_of_int height /. float_of_int width in
  let y_max = 5.0 *. float_of_int height /. float_of_int width in
  let x_range = x_max -. x_min in
  let y_range = y_max -. y_min in

  (* Draw x and y axes *)
  moveto 0 (int_of_float ((-1.0 *. y_min /. y_range) *. float_of_int height));
  lineto width (int_of_float ((-1.0 *. y_min /. y_range) *. float_of_int height));
  moveto (int_of_float ((-1.0 *. x_min /. x_range) *. float_of_int width)) 0;
  lineto (int_of_float ((-1.0 *. x_min /. x_range) *. float_of_int width)) height;

  (* Draw curve *)
  let num_points = 1000 in  (*nombre de points utilisés pour dessiner la courbe*)
  let delta_x = x_range /. float_of_int (num_points - 1) in (*la distance entre deux points voisins sur l'axe des x*)
  let points = Array.init num_points (fun i -> (*initialiser un tableau ou chaque élément est une paire (x',y')*)
    let x = x_min +. float_of_int i *. delta_x in
    let y = eval_expr (int_of_float x) in
    let x' = int_of_float (((x -. x_min) /. x_range) *. float_of_int width) in
    let y' = int_of_float (((y -. y_min) /. y_range) *. float_of_int height) in
    (x', y')) in
  draw_poly_line points;

  (* Wait for user input *)
  ignore (read_key ());
  close_graph ()