open Syntax

let rec count_nodes expr =
  match expr with
  | Var _ -> 1
  | Num _ -> 1
  | App0 _ -> 1
  | App1 (_, e) -> 1 + count_nodes e
  | App2 (_, e1, e2) -> 1 + count_nodes e1 + count_nodes e2

let rec norm expr = 
  match expr with
  | Var v -> Var v
  | Num n -> Num n
  | App0 op -> App0 op
  | App1 (op, e) -> App1 (op, norm e)
  | App2 (op, e1, e2) when op = Plus || op = Mult -> 
    let e1_norm = norm e1 in 
    let e2_norm = norm e2 in
    let e1_count = count_nodes e1 in
    let e2_count = count_nodes e2 in
    if e1_count < e2_count then
      App2 (op, e1_norm, e2_norm)
    else if e1_count > e2_count then
      App2 (op, e2_norm, e1_norm)
    else (* e1_count = e2_count *)
      let e1_str = Syntax.to_string e1_norm in
      let e2_str = Syntax.to_string e2_norm in
      if e1_str < e2_str then
        App2 (op, e1_norm, e2_norm)
      else if e1_str > e2_str then
        App2 (op, e2_norm, e1_norm)
      else (* e1_str = e2_str *)
        App2 (op, e1_norm, e2_norm)
  | App2 (op, e1, e2) -> App2 (op, norm e1, norm e2)