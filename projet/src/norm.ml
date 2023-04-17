open Syntax

let rec count_nodes expr =
  match expr with
  | Var _ -> 1
  | Num _ -> 1
  | App0 _ -> 1
  | App1 (_, e) -> 1 + count_nodes e
  | App2 (_, e1, e2) -> 1 + count_nodes e1 + count_nodes e2

type node =
  | Leaf of expr
  | Internal1 of op1 * node
  | Internal2 of op2 * node list

let rec print_node node =
  match node with
  | Leaf expr ->
      (match expr with
       | Num n -> print_int n
       | Var s -> print_string s
       | App0 op0 -> print_string (str0 op0)
       | App1 (op1, e) -> print_string (str1 op1); print_char '('; print_node (Leaf e); print_char ')'
       | App2 (op2, e1, e2) -> print_string (str2 op2); print_char '('; print_node (Leaf e1); print_string ", "; print_node (Leaf e2); print_char ')')
  |Internal1 (op, node) -> print_string (str1 op); print_char '('; print_node node; print_char ')'
  | Internal2 (op, children) ->
      (match op with
       | Plus -> print_char '('; print_string "+; "; print_node_list children; print_char ')'
       | Mult -> print_char '('; print_string "*; ";print_node_list children; print_char ')'
       | op -> print_char '('; print_string ((str2 op) ^ "; "); print_node_list children; print_char ')')

and print_node_list nodes =
  match nodes with
  | [] -> ()
  | [node] -> print_node node
  | node :: nodes' -> print_node node; print_string ", "; print_node_list nodes'

let rec replace_minus expr =
  match expr with
  | App2 (Minus, e1, e2) -> App2 (Plus, replace_minus e1, App1 (UMinus, replace_minus e2))
  | _ -> expr

let rec build_tree expr =
  let expr = replace_minus expr in
  match expr with
  | App2 (op, e1, e2) when op == Plus || op == Mult->
      let node1 = build_tree e1 in
      let node2 = build_tree e2 in
      (match node1, node2 with
       | Internal2 (op1, children1), Internal2 (op2, children2)
         when op1 = op && op2 = op ->
           Internal2 (op, children1 @ children2)
       | Internal2 (op1, children1), _ when op1 = op ->
           Internal2 (op, children1 @ [build_tree e2])
       | _, Internal2 (op2, children2) when op2 = op ->
           Internal2 (op, build_tree e1 :: children2)
       | _, _ ->
           Internal2 (op, [build_tree e1; build_tree e2]))
  | App2 (op, e1, e2) -> Internal2 (op, [build_tree e1; build_tree e2])
  | App1 (op, e1) -> Internal1 (op, build_tree e1)
  | _ ->
      Leaf expr

let cmp e1 e2 = 
  let e1_norm = match e1 with | App1 (UMinus, e) -> e | _ -> e1 in
  let e2_norm = match e2 with | App1 (UMinus, e) -> e | _ -> e2 in
  let e1_count = count_nodes e1_norm in
  let e2_count = count_nodes e2_norm in
  if e1_count < e2_count then
    -1
  else if e1_count > e2_count then
    1
  else (* e1_count = e2_count *)
    let e1_str = Syntax.to_string e1_norm in
    let e2_str = Syntax.to_string e2_norm in
    if e1_str < e2_str then
      -1
    else if e1_str > e2_str then
      1
    else (* e1_str = e2_str *)  
      if e1 = App1 (UMinus, e1_norm) then
        1
      else if e2 = App1 (UMinus, e2_norm) then 
        -1 
      else
        0
        

let rec to_expr node =
  match node with
  | Leaf expr -> expr
  | Internal1 (op, node) -> App1 (op, to_expr node)
  | Internal2 (op, children) ->
      let exprs = to_expr_list children in
      let exprs = List.sort cmp exprs in
      match op with
      | op -> appn_helper op exprs

and to_expr_list nodes =
  List.map (fun node -> to_expr node) nodes

and appn_helper op exprs =
  match exprs with
  | [] -> failwith "empty expression list"
  | [e] -> e
  | _ -> appn_helper_helper op exprs

and appn_helper_helper op remaining_exprs =
  match remaining_exprs with
  | [] -> failwith "empty expression list"
  | [e] -> e
  | e1::e2::[] -> App2 (op, e1, e2)
  | e1::e2::es -> appn_helper_helper op ((App2 (op, e1, e2))::es)

      
let norm expr = 
  let tree = build_tree expr in
  let res = to_expr tree in
  res