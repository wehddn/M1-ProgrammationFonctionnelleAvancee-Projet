open Syntax
open Eval

let simpl_arith expr =
  match expr with
  (* x - x = 0 *)
  | App2 (Minus, e1, e2) when e1 = e2 -> Num 0

  (* x - 0 = x *)
  | App2 (Minus, e1, e2) when e2 = Num 0 -> e1 

  (* x - 0 = x *)
  | App2 (Minus, e1, e2) when e1 = Num 0 -> App1 (UMinus, e2)

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
  (* tan(x) = sin(x)/cos(x) *)
  | App2 (Div, App1 (Sin, e1), App1 (Cos, e2)) when e1 = e2 -> App1 (Tan, e1)
  | _ -> expr

let replace2_e1 op e2 value_list = 
  List.map (fun value -> App2 (op, value, e2)) value_list 

let replace1_e1 op value_list = 
  List.map (fun value -> App1 (op, value)) value_list 

let replace2_e2 op e1 value_list = 
  List.map (fun value -> App2 (op, e1, value)) value_list 

let replace2_e1_e2 op e1_lst e2_lst = 
  List.concat (List.map (fun e1 -> List.map (fun e2 -> App2 (op, e1, e2)) e2_lst) e1_lst)

let lst_expr_to_string lst_expr =
  match lst_expr with
  | sub_lst_exprs ->
    let sub_lst_expr_strings = List.map Syntax.to_string sub_lst_exprs in
    let sub_lst_expr_string = String.concat ", " sub_lst_expr_strings in
    Printf.sprintf "%s" sub_lst_expr_string

let remove_duplicates lst_expr =
  match lst_expr with
  | lst -> List.sort_uniq compare lst

let simplify expr =
  let expr_arith = simpl_arith expr in
  let expr_trig = simpl_trig expr in
  let expr_lst = if expr = expr_arith then [expr] else [expr_arith] in
  let expr_lst = if expr = expr_trig then expr_lst@[] else expr_lst@[expr_trig] in
  expr_lst

let rec simpl_aux expr = 
  match expr with
  | Num _ -> [expr]
  | Var _ -> [expr]
  | App0 _ -> [expr] 
  | App1 (op, e) -> 
    let e_lst = simplify e in

    let e1_simpl = List.map (fun expr -> simpl_aux expr) e_lst |> List.concat in

    let res_list = replace1_e1 op e1_simpl in
    let res_list = remove_duplicates res_list in
    res_list

  | App2 (op, e1, e2) ->
    let e1_lst = simplify e1 in

    let e2_lst = simplify e2 in

    let e1_simpl = List.map (fun expr -> simpl_aux expr) e1_lst |> List.concat in
    let e2_simpl = List.map (fun expr -> simpl_aux expr) e2_lst |> List.concat in

    let res1_list = replace2_e1 op e2 e1_simpl in
    let res2_list = replace2_e2 op e1 e2_simpl in
    let res3_list = replace2_e1_e2 op e1_simpl e2_simpl in
    let res_list = res1_list@res2_list@res3_list in

    let res_list = remove_duplicates res_list in
    
    res_list

let rec count_nodes expr =
  match expr with
  | Var _ -> 1
  | Num _ -> 1
  | App0 _ -> 1
  | App1 (_, e) -> 1 + count_nodes e
  | App2 (_, e1, e2) -> 1 + count_nodes e1 + count_nodes e2

let shortest_expr exprs =
  let rec helper min_expr min_count = function
    | [] -> min_expr
    | e :: es ->
      let count = count_nodes e in
      if count < min_count then helper e count es
      else helper min_expr min_count es
  in
  match exprs with
  | [] -> None
  | e :: es -> Some (helper e (count_nodes e) es)

let simpl expr =
  let expr_lst = simplify expr in

  let lst_expr = List.map (fun expr -> simpl_aux expr) expr_lst |> List.concat in
  let lst_expr = List.map (fun expr -> simplify expr) lst_expr |> List.concat in

  let result = shortest_expr lst_expr in
  match result with
  | None -> expr
  | Some result -> result