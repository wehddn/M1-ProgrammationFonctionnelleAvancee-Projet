open Syntax
open Eval
open Simpl
open Subst

let rec derive e x =
  match e with
  (* e est une variable = x, la dérivée est 1 *)
  | Var x' when x = x' -> Num 1
  (* e est une variable différente de x, la dérivée est 0 *)
  | Var _ -> Num 0
  (* e est un nombre, la dérivée est 0 *)
  | Num _ -> Num 0
  (* e est Pi | E, la dérivée est 0 *)
  | App0 _ -> Num 0
  (* e est une opération unaire *)
  | App1 (op1, e1) ->
      (match op1 with
       | Sqrt -> App2 (Div, derive e1 x, App2 (Mult, Num 2, App1 (Sqrt, e1)))
       | Exp -> App2 (Mult, derive e1 x, e)
       | Log -> App2 (Div, derive e1 x, e1)
       | Sin -> App2 (Mult, derive e1 x, App1 (Cos, e1))
       | Cos -> App2 (Mult, derive e1 x, App1 (UMinus, App1 (Sin, e1)))
       | Tan -> App2 (Mult, derive e1 x, App2 (Plus, Num 1, App2 (Expo, App1 (Tan, e1), Num 2)))
       | ASin -> App2 (Div, derive e1 x, App1 (Sqrt, App2 (Minus, Num 1, App2 (Expo, e1, Num 2))))
       | ACos -> App2 (Div, App1 (UMinus, derive e1 x), App1 (Sqrt, App2 (Minus, Num 1, App2 (Expo, e1, Num 2))))
       | ATan -> App2 (Div, derive e1 x, App2 (Plus, Num 1, App2 (Expo, e1, Num 2)))
       | UMinus -> App1 (UMinus, derive e1 x))
  (* e est une opération binaire *)
  | App2 (op2, e1, e2) ->
      (match op2 with
       | Plus -> App2 (Plus, derive e1 x, derive e2 x)
       | Mult -> App2 (Plus, App2 (Mult, derive e1 x, e2), App2 (Mult, e1, derive e2 x))
       | Minus -> App2 (Minus, derive e1 x, derive e2 x)
       | Div -> App2 (Div, App2 (Minus, App2 (Mult, derive e1 x, e2), App2 (Mult, e1, derive e2 x)), App2 (Expo, e2, Num 2))
       | Expo -> App2 (Mult, e, App2 (Plus, App2 (Mult, derive e2 x, App1 (Log, e1)), App2 (Div, App2 (Mult, derive e1 x, e2), e1))))
 (* (e1^e2)' = e1^e2 * (e2 * log(e1))' + log(e1^e2) * e1' *)
 (* gauche: e1^e2 * (e2 * log(e1))' *)
 (* droite: log(e1^e2) * e1' *)
 (*| App2 (Expo, e1, e2) ->
  let gauche = App2 (Mult, e2, App2 (Expo, e1, App2 (Minus, e2, Num 1))) in
  let droite = App2 (Mult, App2 (Expo, e1, e2), derive e1 x) in
  App2 (Mult, App1 (Plus, gauche, droite), derive (App1 (Log, e1)) x) *)
     

