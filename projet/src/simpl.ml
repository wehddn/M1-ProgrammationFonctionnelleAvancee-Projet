open Syntax
open Eval

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


let rec simpl_aux expr =
  let expr' = simpl_arith expr in
  let expr' = simpl_trig expr' in 
  let expr2 = if expr' = expr then expr else simpl_aux expr' in
  match expr2 with
  | Num n -> Num n
  | Var v -> Var v
  | App0 _ -> failwith "App0"
  | App1 (op, e) -> App1 (op, simpl_aux e)
  | App2 (op, e1, e2) -> App2 (op, simpl_aux e1, simpl_aux e2)

let rec simpl expr =
  let result = simpl_aux expr in
  if result = expr then expr else simpl result