open Syntax
open Eval
open Simpl
open Subst
open Derive
(*
let rec integ_step (u : expr) (dv : expr) (x : string) (e : expr) : expr =
    match e with
    | Num n -> App2 (Mult, Num n, u)
    | Var x -> App2 (Mult, Var x, u)
    | App1 (UMinus, e1) -> App1 (UMinus, integ_step u dv e1)
    | App2 (Plus, e1, e2) -> App2 (Plus, integ_step u dv e1, integ_step u dv e2)
    | App2 (Minus, e1, e2) -> App2 (Minus, integ_step u dv e1, integ_step u dv e2)
    | App2 (Mult, e1, e2) -> 
        let du = derive u x in
        let v = integ e1 x (Num 0) (Num 1) in
        let f = App2 (Mult, du, v) in
        let g = App2 (Mult, dv, integ_step u dv e1) in
        App2 (Plus, f, g)
    | _ -> raise (Invalid_argument "Unsupported expression")*)
  (*
let rec integ (e : expr) (x : string) (a : expr) (b : expr) : expr =
  let dx = App2 (Minus, Var x, Num 0) in
  match e with
  | Num _ | Var _ -> App2 (Mult, e, dx)
  | App1 (UMinus, e1) -> App1 (UMinus, integ e1 x a b)
  | App2 (Plus, e1, e2) -> App2 (Plus, integ e1 x a b, integ e2 x a b)
  | App2 (Minus, e1, e2) -> App2 (Minus, integ e1 x a b, integ e2 x a b)
  | App2 (Mult, e1, e2) ->
      let u = Var x in
      let v = integ e2 x a b in
      let du = dx in
      let dv = derive e2 x in
      (*App2 (Minus, App2 (Mult, e1, v), App1 (Integ, App2 (Mult, du, v)))
       | integ_step u dv*)
  | App2 (Div, e1, e2) ->
      let u = Var x in
      let v = integ e2 x a b in
      let du = dx in
      let dv = derive e2 x in
      let f = App2 (Minus, App2 (Mult, e1, v), App1 (Integ, App2 (Mult, du, v))) in
      let g = App1 (Integ, App2 (Mult, dv, App1 (Expo, App2 (Mult, App1 (UMinus, b), du)))) in
      App2 (Minus, f, g) |> integ_step u dv
  | App2 (Expo, e1, e2) ->
      let u = App1 (Log, e1) in
      let v = integ e2 x a b in
      let du = derive e1 x in
      let dv = derive e2 x in
      let f = App2 (Mult, e1, v) in
      let g = App1 (Integ, App2 (Mult, dv, App1 (Expo, e1))) in
      let h = App1 (Integ, App2 (Mult, v, du)) in
      let i = App2 (Mult, f, g) in
      let j = App2 (Mult, App2 (Minus, v, App1 (Integ, App2 (Mult, dv, u))), h) in
      App2 (Plus, i, j) |> integ_step (Var x) dv
  | App1 (Sin, e1) ->
      let u = App1 (Cos, e1) in
      let v = integ e1 x a b in
      App2 (Minus, App1 (Cos, App1 (Subst, App2 (Mult, App1 (UMinus, b), u), e1, Var x)), App1 (Cos, App1 (Subst, App2 (Mult, App1 (UMinus, a), u), e1, Var x)))
*)
