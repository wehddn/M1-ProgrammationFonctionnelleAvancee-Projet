open Syntax
open Norm
open Eval
open Set

(*
  The code consists of two main parts, the first one is a set of
  simplification rules implemented in the simpl_arith, and the 
  second one is a recursive application of the simplification 
  rules to an expression.
*)

module ExprSet = Set.Make(struct
type t = expr
let compare = Norm.cmp
end)

let simpl expr =

  let simpl_eval expr = 
    try FloatNum (eval expr) with | _ -> expr
  in

  let checkNum e n = if e = Num n || e = FloatNum (float_of_int n) then true else false
  in

  let simpl_arith expr =
    match expr with
    (* - - x = x *)
    | App1 (UMinus, App1 (UMinus, e1)) -> e1

    (* x + (-y) = y - x *)
    | App2 (Plus, e1, App1 (UMinus, e2)) -> App2 (Minus, e1, e2)

    (* -x + y = y - x *)
    | App2 (Plus, App1 (UMinus, e1), e2) -> App2 (Minus, e2, e1)

    (* 0 + x = 0*)
    | App2 (Plus, e1, e2) when checkNum e1 0 -> e2

    (* x - x = 0 *)
    | App2 (Minus, e1, e2) when e1 = e2 -> Num 0

    (* x - 0 = x *)
    | App2 (Minus, e1, e2) when checkNum e2 0 -> e1 

    (* 0 - x = -x *)
    | App2 (Minus, e1, e2) when checkNum e1 0 -> App1 (UMinus, e2)

    (* 0 * x = 0 *)
    | App2 (Mult, e1, e2) when checkNum e1 0 -> Num 0

    (* 1 * x = x *)
    | App2 (Mult, e1, e2) when checkNum e1 1 -> e2

    (* x / 1 = x *)
    | App2 (Div, e1, e2) when checkNum e2 1 -> e1

    (* 0 / x = 0 *)
    | App2 (Div, e1, e2) when checkNum e1 0 && not (checkNum e2 0) -> Num 0
    | App2 (Div, e1, e2) when checkNum e2 0 -> failwith "Division par 0"

    (* log(exp(x)) = x *)
    | App1 (Log, App1 (Exp, e)) -> e

    (* x*x = x^2 *)
    | App2 (Mult, e1, e2) when e1 = e2 -> App2(Expo, e1, Num 2)

    (* a*x*x = a*x^2 *)
    | App2 (Mult, App2 (Mult, e1, e2), e3) when e2 = e3 -> App2 (Mult, e1, App2(Expo, e2, Num 2))

    (* a/b/c = a/(b*c) *)
    | App2(Div, (App2(Div, e1, e2)), e3) when not (checkNum e2 0) && not (checkNum e3 0) ->
      App2(Div, e1, (App2(Mult, e2, e3)))
    
    (* a/(b*c) = a/b/c *)
    | App2(Div, e1, (App2(Mult, e2, e3))) when not (checkNum e2 0) && not (checkNum e3 0) ->
      App2(Div, e1, (App2(Mult, e2, e3)))

    (* a*(b/c) = (a*b)/c *)
    | App2(Mult, e1, App2(Div, e2, e3)) when not (checkNum e3 0) ->
      App2(Div, App2(Mult, e1, e2), e3)

    (* (a/b)*c = (a*c)/b *)
    | App2(Mult, App2(Div, e1, e2), e3) when not (checkNum e2 0) -> 
      App2(Div, App2(Mult, e1, e3), e2)
    
    (* (a*b)/c = a*(b/c) *)
    | App2(Div, App2(Mult, e1, e2), e3) when not (checkNum e2 0) -> 
      App2(Mult, e1, App2(Div, e2, e3))

    (* x^0 = 1 *)
    | App2(Expo, e1, e2) when checkNum e2 0 -> Num 1

    (* x^1 = x *)
    | App2(Expo, e1, e2) when checkNum e2 1 -> e1
    
    (* x^a * x^b = x^(a+b) *)
    | App2(Mult, App2(Expo, e1, e2), App2(Expo, e3, e4)) when e1 = e3 ->
      App2(Expo, e1, App2(Plus, e2, e4))
    | App2(Mult, e1, App2(Expo, e3, e4)) when e1 = e3 ->
      App2(Expo, e1, App2(Plus, Num 1, e4))
    | App2(Mult, App2(Expo, e1, e2), e3) when e1 = e3 ->
      App2(Expo, e1, App2(Plus, e2, Num 1))

    (* x^a / x^b = x^(a-b) *)
    | App2(Div, App2(Expo, e1, e2), App2(Expo, e3, e4)) when e1 = e3 ->
      App2(Expo, e1, App2(Minus, e2, e4))
    | App2(Div, e1, App2(Expo, e3, e4)) when e1 = e3 ->
      App2(Expo, e1, App2(Minus, Num 1, e4))
    | App2(Div, App2(Expo, e1, e2), e3) when e1 = e3 ->
      App2(Expo, e1, App2(Minus, e2, Num 1))
    
    (* a ^ b ^ c = a ^ (b*c) *)
    | App2(Expo, App2(Expo, e1, e2), e3)
    | App2(Expo, e1, App2(Expo, e2, e3)) -> App2(Expo, e1, App2(Mult, e2, e3))

    | _ -> expr
  in

  let is_even num = num mod 2 = 0
  in

  let is_odd num = num mod 2 = 1
  in

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
  in

  (*
    Function applies the simplification rules to an expression and 
    returns the simplified expression if any simplification was 
    performed.
  *)
  let simplify expr =
    let expr' = simpl_eval expr in
    let expr' = simpl_arith expr' in
    let expr' = simpl_trig expr' in
    if expr = expr' then expr else expr'
  in

  let rec simpl_aux expr = 
    let expr = simplify expr in
    match expr with
    | Num _ -> expr
    | FloatNum _ -> expr
    | Var _ -> expr
    | App0 _ -> expr 
    | App1 (op, e) -> App1 (op, simpl_aux e)
    | App2 (op, e1, e2) -> App2 (op, simpl_aux e1, simpl_aux e2) 
  in

  let diff2 set set1 set2 =
    ExprSet.diff (ExprSet.diff set set1) set2
  in

  (*
    The function returns the smallest simplified expression
  *)

  let rec aux set set_processed =
    let set_norm = ExprSet.map (fun x -> norm x false) set in
    let set_norm = ExprSet.diff set_norm set_processed in
    let set = ExprSet.union set set_norm in
    let set_simplify = ExprSet.map simpl_aux set in
    let set_simplify = ExprSet.diff set_simplify set_norm in
    let set_simplify = ExprSet.diff set_simplify set_processed in
    let set_processed = ExprSet.union set_processed set in
    if ExprSet.is_empty set_simplify then set_processed else aux set_simplify set_processed
  in

  let set = ExprSet.empty in
  let set = ExprSet.add expr set in
  let res = aux set ExprSet.empty in
  ExprSet.min_elt res