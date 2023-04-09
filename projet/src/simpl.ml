open Syntax
open Eval

type tree_simpl =
  | E
  | T of (expr * expr list)

let tree_add_expr tree expr = 
  match tree with
    | E -> tree
    | T (value, list) -> T (value, list@[expr])

let rec tree_add_exprs tree exprs = 
  match exprs with
    | [] -> tree
    | x::t -> tree_add_exprs (tree_add_expr tree x) t

let tree_add_tree tree tree' = 
  match tree with
    | E -> tree
    | T (value, list) -> T (value, list@[tree'])

let tree_to_string tree =
  match tree with
  | E -> "E"
  | T (value, sub_trees) ->
      let sub_tree_strings = List.map Syntax.to_string sub_trees in
      let sub_tree_string = String.concat ", " sub_tree_strings in
      Printf.sprintf "T (%s, [%s])" (Syntax.to_string value) sub_tree_string

let expr_from_tree tree =
  match tree with
    | E -> []
    | T (expr, list) -> expr::list

let remove_duplicates tree =
  match tree with
  | T (value, lst) -> T (value, List.sort_uniq compare lst)
  | E -> tree

let simpl_arith expr =
  match expr with
  (* x - x = 0 *)
  | App2 (Minus, e1, e2) when e1 = e2 -> Num 0

  (* a * 0 = 0 *)
  | App2 (Mult, e1, e2) when e1 = Num 0 || e2 = Num 0 -> Num 0 

  (* a * 1 = a *)
  | App2 (Mult, e1, e2) -> 
    if e1 = Num 1 then e2 else
    if e2 = Num 1 then e1 else expr

  (* log(exp(x)) = x *)
  | App1 (Log, App1 (Exp, e)) -> e 

  | _ -> expr

let simpl_trig expr =
  match expr with  
  (* tan(x) = sin(x)/cos(x) *)
  | App1 (Tan, e) -> App2 (Div, App1 (Sin, e), App1 (Cos, e))
  | _ -> expr

let replace_e1 op e2 value_list = 
  List.map (fun value -> App2 (op, value, e2)) value_list 

let replace_e2 op e1 value_list = 
  List.map (fun value -> App2 (op, e1, value)) value_list 

let rec simpl_aux expr = 
  let tree = T (expr, []) in
  let expr' = simpl_arith expr in
  let tree = if expr' = expr then tree else tree_add_expr tree expr' in
  let expr' = simpl_trig expr in
  let tree = if expr' = expr then tree else tree_add_expr tree expr' in
  print_endline (tree_to_string tree);
  match expr with
  | Num _ -> tree
  | Var _ -> tree
  | App0 _ -> tree 
  | App1 _ -> tree
  | App2 (op, e1, e2) ->
    let e1_tree = simpl_aux e1 in
    let e1_expr_list = expr_from_tree e1_tree in
    let res1_list = replace_e1 op e2 e1_expr_list in
    let tree = tree_add_exprs tree res1_list in

    let e2_tree = simpl_aux e2 in
    let e2_expr_list = expr_from_tree e2_tree in
    let res2_list = replace_e2 op e1 e2_expr_list in
    let tree = tree_add_exprs tree res2_list in
    
    let tree = remove_duplicates tree in
    tree

let simpl expr =
  let tree = simpl_aux expr in 
  print_endline (tree_to_string tree);
   expr