open Syntax
open Eval

(*
  The code consists of two main parts, the first one is a set of
  simplification rules implemented in the simpl_functions, and the 
  second one is a recursive application of the simplification 
  rules to an expression.
*)

let simpl_functions expr =
  match expr with
  (* x + 0 = 0*)
  | App2 (Plus, e1, e2) -> 
    if e1 = Num 0 then e2 else
    if e2 = Num 0 then e1 else expr

  (* x - x = 0 *)
  | App2 (Minus, e1, e2) when e1 = e2 -> Num 0

  (* x - 0 = x *)
  | App2 (Minus, e1, e2) when e2 = Num 0 -> e1 

  (* 0 - x = -x *)
  | App2 (Minus, e1, e2) when e1 = Num 0 -> App1 (UMinus, e2)

  (* a * 0 = 0 *)
  | App2 (Mult, e1, e2) when e1 = Num 0 || e2 = Num 0 -> Num 0

  (* a * 1 = a *)
  | App2 (Mult, e1, e2) -> 
    if e1 = Num 1 then e2 else
    if e2 = Num 1 then e1 else expr

  (* a / 1 = a *)
  | App2 (Div, e1, e2) when e2 = Num 1 -> e1

  (* 0 / a = a *)
  | App2 (Div, e1, e2) when e1 = Num 0 -> e2

  (* log(exp(x)) = x *)
  | App1 (Log, App1 (Exp, e)) -> e 

  (* tan(x) = sin(x)/cos(x) *)
  | App1 (Tan, e) -> App2 (Div, App1 (Sin, e), App1 (Cos, e))

  | _ -> expr

(*
  Function applies the simplification rules to an expression and 
  returns the simplified expression if any simplification was 
  performed.
*)
let simplify expr =
  let expr' = simpl_functions expr in
  if expr = expr' then expr else expr'

let rec simpl_aux expr = 
  let expr = simplify expr in
  match expr with
  | Num _ -> expr
  | Var _ -> expr
  | App0 _ -> expr 
  | App1 (op, e) -> App1 (op, simpl_aux e)
  | App2 (op, e1, e2) -> App2 (op, simpl_aux e1, simpl_aux e2) 

let rec count_nodes expr =
  match expr with
  | Var _ -> 1
  | Num _ -> 1
  | App0 _ -> 1
  | App1 (_, e) -> 1 + count_nodes e
  | App2 (_, e1, e2) -> 1 + count_nodes e1 + count_nodes e2

(*
  The function returns the simplified expression with the 
  smallest number of nodes.
*)
let simpl expr =
  let rec aux expr node_count =
    let res = simpl_aux expr in
    let new_count = count_nodes res in
    if new_count < node_count then aux res new_count else res
  in
  aux expr (count_nodes expr)