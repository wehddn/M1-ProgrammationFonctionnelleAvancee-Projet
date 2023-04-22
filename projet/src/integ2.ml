open Syntax
open Primitive
open Subst
open Eval
  
let formule pexpr (x : string) a b = 
  let saexpr = subst pexpr x a in
  let sbexpr = subst pexpr x b in
  eval sbexpr -. eval saexpr 

let integ (expr : expr) (x : string) (a : expr) (b : expr) : float =
  let pexpr = primitive expr (Light.(Var x)) in
  let res = if expr <> pexpr then formule pexpr x a b else 0.
  in res