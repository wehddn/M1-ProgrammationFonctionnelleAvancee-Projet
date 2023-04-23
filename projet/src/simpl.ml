open Syntax
open Norm

(*
  The code consists of two main parts, the first one is a set of
  simplification rules implemented in the simpl_arith, and the 
  second one is a recursive application of the simplification 
  rules to an expression.
*)

let simpl_arith expr =
  match expr with
  (* - - x = x *)
  | App1 (UMinus, App1 (UMinus, e1)) -> e1

  (* x + (-y) = y - x *)
  | App2 (Plus, e1, App1 (UMinus, e2)) -> App2 (Minus, e1, e2)

  (* 0 + x = 0*)
  | App2 (Plus, e1, e2) -> 
    if e1 = Num 0 || e1 = FloatNum 0. then e2 else expr

  (* x - x = 0 *)
  | App2 (Minus, e1, e2) when e1 = e2 -> Num 0

  (* x - 0 = x *)
  | App2 (Minus, e1, e2) when e2 = Num 0 || e2 = FloatNum 0. -> e1 

  (* 0 - x = -x *)
  | App2 (Minus, e1, e2) when e1 = Num 0 -> App1 (UMinus, e2)

  (* 0 * x = 0 *)
  | App2 (Mult, e1, e2) when e1 = Num 0 -> Num 0

  (* 1 * x = x *)
  | App2 (Mult, e1, e2) -> 
    if e1 = Num 1 then e2 else expr

  (* x / 1 = x *)
  | App2 (Div, e1, e2) when e2 = Num 1 -> e1

  (* 0 / x = 0 *)
  | App2 (Div, e1, e2) when e1 = Num 0 && e2 <> Num 0 -> Num 0

  (* log(exp(x)) = x *)
  | App1 (Log, App1 (Exp, e)) -> e 

  | _ -> expr

let is_even num = num mod 2 = 0

let is_odd num = num mod 2 = 1

let simpl_trig expr =
  match expr with
  (* cos(x)^2+sin(x)^2 = 1 *)
  | App2(Plus, App2(Expo, App1(Cos, e1), Num 2), App2(Expo, App1(Sin, e2), Num 2)) when e1 = e2 -> Num 1

  (* sin(x)/cos(x) = tan(x) *)
  | App2 (Div, App1 (Sin, e1), App1 (Cos, e2)) when e1 = e2 -> App1 (Tan, e1)

  (* cos(a)*sin(b)+cos(b)*sin(a) = sin(a+b) a-b+b-a cos(a-b)*sin(b-a)+cos(b-a)*sin(a-b) *)
  | App2 (Plus, App2 (Mult, App1 (Cos, a1), App1 (Sin, b1)), App2 (Mult, App1 (Cos, b2), App1 (Sin, a2)))
    when a1 = a2 && b1 = b2 -> App1 (Sin, (App2 (Plus, a1, b1)))

  (* cos(b)*sin(a)-cos(a)*sin(b) = sin(a-b) *)
  | App2 (Minus, App2 (Mult, App1 (Cos, b1), App1 (Sin, a1)), App2 (Mult, App1 (Cos, a2), App1 (Sin, b2)))
    when a1 = a2 && b1 = b2 -> App1 (Sin, (App2 (Minus, a1, b1)))

  (* cos(a)*cos(b)-sin(a)*sin(b) = cos(a+b) *)
  | App2 (Minus, App2 (Mult, App1 (Cos, a1), App1 (Cos, b1)), App2 (Mult, App1 (Sin, a2), App1 (Sin, b2)))
    when a1 = a2 && b1 = b2 -> App1 (Cos, (App2 (Plus, a1, b1)))

  (* cos(a)*cos(b) + sin(a)*sin(b) = cos(a-b) *)
  | App2 (Plus, App2 (Mult, App1 (Cos, a1), App1 (Cos, b1)), App2 (Mult, App1 (Sin, a2), App1 (Sin, b2)))
    when a1 = a2 && b1 = b2 -> App1 (Cos, (App2 (Minus, a1, b1)))
  
  (* (tan(a) + tan(b))/(1 - tan(a)*tan(b)) = tan(a+b) *)
  | App2 (Div, App2 (Plus, App1 (Tan, a1), App1 (Tan, b1)), App2 (Minus, Num 1, App2 (Mult, App1 (Tan, a2), App1 (Tan, b2))))
    when a1 = a2 && b1 = b2 -> App1 (Tan, (App2 (Plus, a1, b1)))

  (* (tan(a) - tan(b))/(1 + tan(a)*tan(b)) = tan(a-b) *)
  | App2 (Div, App2 (Minus, App1 (Tan, a1), App1 (Tan, b1)), App2 (Plus, Num 1, App2 (Mult, App1 (Tan, a2), App1 (Tan, b2))))
    when a1 = a2 && b1 = b2 -> App1 (Tan, (App2 (Minus, a1, b1)))

  (* sin(x+2*pi) = sin(x); cos(x+2*pi) = cos(x) *)
  | App1 (op, App2 (Plus, e, App2 (Mult, Num n, App0 (Pi)))) 
    when is_even n && (op = Sin || op = Cos) -> App1 (op, e)

  (* tan(x+pi) = tan(x) *)
  | App1 (op, App2 (Plus, e, App0 (Pi))) 
    when op = Tan -> App1 (op, e)
  | App1 (op, App2 (Plus, e, App2 (Mult, Num n, App0 (Pi)))) 
    when is_odd n && op = Tan -> App1 (op, e)

  (* 2*cos(x)*sin(x) = sin(2*x) *)
  | App2(Mult, App2(Mult, Num 2, App1(Cos, e1)), App1(Sin, e2)) when e1 = e2 -> App1(Sin, App2(Mult, Num 2, e1))

  (* cos(x)^2 - sin(x)^2 = cos(2*x) *)
  | App2(Minus, App2(Expo, App1(Cos, e1), Num 2), App2(Expo, App1(Sin, e2), Num 2)) 
    when e1 = e2 -> App1(Cos, App2(Mult, Num 2, e1))

  (* (2*tan(x))/(1 - tan(x)^2) = tan(2*x) *)
  | App2(Div, App2(Mult, Num 2, App1(Tan, e1)), App2(Minus, Num 1, App2(Expo, App1(Tan, e2), Num 2))) 
    when e1 = e2 -> App1(Tan, App2(Mult, Num 2, e1))

  | _ -> expr

(*
  Function applies the simplification rules to an expression and 
  returns the simplified expression if any simplification was 
  performed.
*)
let simplify expr =
  let expr' = simpl_arith expr in
  let expr' = simpl_trig expr' in
  if expr = expr' then expr else expr'

let rec simpl_aux expr = 
  
  let expr = simplify expr in
  match expr with
  | Num _ -> expr
  | FloatNum _ -> expr
  | Var _ -> expr
  | App0 _ -> expr 
  | App1 (op, e) -> App1 (op, simpl_aux e)
  | App2 (op, e1, e2) -> App2 (op, simpl_aux e1, simpl_aux e2) 

let rec count_nodes expr =
  match expr with
  | Var _ -> 1
  | Num _ -> 1
  | FloatNum _ -> 1
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
  let rec aux' expr node_count =
    let expr_norm = norm expr in
    let res = aux expr_norm (count_nodes expr_norm) in
    let new_count = count_nodes res in
    if new_count < node_count then aux' res new_count else res
  in
  aux' expr (count_nodes expr)