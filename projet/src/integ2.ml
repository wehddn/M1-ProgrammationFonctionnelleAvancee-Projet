open Syntax
open Primitive
open Subst
open Eval
open Simpl
open Norm

type integral = expr * string

type node =
  | Expr of expr
  | Integral of integral
  | Const of float * integral
  | Sum of node * node

let rec local_eval expr = 
  match expr with
  | Num n -> expr
  | FloatNum n -> expr
  | Var v -> expr
  | App0 e -> FloatNum (eval (App0 e))
  | App1(op, e1) ->
    let e1localeval = local_eval e1 in
    (try FloatNum (eval (App1(op, e1localeval))) with | _ -> App1(op, e1))
  | App2(op, e1, e2) -> 
    let e1localeval = local_eval e1 in
    let e2localeval = local_eval e2 in
    (try
      FloatNum (eval (App2(op, e1localeval, e2localeval)))
    with | _ -> (try
                  FloatNum (eval (App2(op, e1localeval, e2)))
                with | _ -> (try
                              FloatNum (eval (App2(op, e1, e2localeval)))
                            with | _ -> App2(op, e1localeval, e2localeval))))

let formule pexpr (x : string) a b = 
  let saexpr = subst pexpr x a in
  let sbexpr = subst pexpr x b in
  eval sbexpr -. eval saexpr 

let integ (expr : expr) (x : string) (a : expr) (b : expr) : float =
  let expr = norm expr in
  let expr = local_eval expr in
  let pexpr = primitive expr (Light.(Var x)) in
  let res = if expr <> pexpr then formule pexpr x a b else 0.
  in res