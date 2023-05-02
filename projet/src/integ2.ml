open Syntax
open Primitive
open Subst
open Eval
open Simpl
open Norm
open Derive

type integral = expr * string

type node =
  | Leaf of expr
  | Integral of integral
  | Internal2 of op2 * node * node


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

let rec build_tree expr x =
  let expr = replace_minus expr in
  match expr with
  | App2 (op, e1, e2) when op == Mult ->
    let e1type = (match e1 with | FloatNum _ | Num _ -> true | _ -> false) in
    let e2type = (match e2 with | FloatNum _ | Num _ -> true | _ -> false) in
    (match e1type, e2type with
    | true, true -> Leaf (local_eval (App2 (op, e1, e2)))
    | true, false -> Internal2 (op, Leaf e1, build_tree e2 x)
    | false, true -> Internal2 (op, build_tree e1 x, Leaf e2)
    | false, false -> Integral (App2 (op, e1, e2), x)
    ) 
  | App2 (op, e1, e2) when op == Plus ->
    Internal2 (op, build_tree e1 x, build_tree e2 x)
  | App2 (op, e1, e2) -> Integral (expr, x)
  | App1 (op, e1) -> Integral (expr, x)
  | _ ->
    Integral (expr, x)

and replace_minus expr =
  match expr with
  | App2 (Minus, e1, e2) -> App2 (Plus, replace_minus e1, App1 (UMinus, replace_minus e2))
  | _ -> expr

let rec node_to_string n =
  match n with
  | Leaf e ->
    "Leaf(" ^ (Syntax.to_string e) ^ ")"
  | Integral i ->
    "Integral(" ^ integral_to_string i ^ ")"
  | Internal2 (op, n1, n2) ->
    "Internal2(" ^ (Syntax.str2 op) ^ ", " ^ node_to_string n1 ^ ", " ^ node_to_string n2 ^ ")"

and integral_to_string i =
  let expr, str = i in (Syntax.to_string expr) ^ " d" ^ str 

let formule pexpr (x : string) a b = 
  let saexpr = subst pexpr x a in
  let sbexpr = subst pexpr x b in
  eval sbexpr -. eval saexpr 

let eval_primitive expr x a b =
  let pexpr = primitive expr (Light.(Var x)) in
  if expr <> pexpr then Some (formule pexpr x a b) else None

let rec eval_tree tree x a b = 
  match tree with
  | Leaf e -> eval_primitive e x a b
  | Integral (e, x) -> eval_primitive e x a b
  | Internal2 (op, n1, n2) ->
    let n1eval = eval_tree n1 x a b in 
    let n2eval = eval_tree n2 x a b in
    (match n1eval, n2eval with
    | Some x, Some y -> Some (eval (App2(op, FloatNum x, FloatNum y)))
    | _ -> None 
    )

let rec parts expr x =
  match expr with 
  | App1 (Log, e) when Syntax.to_string e = x ->
    parts (App2 (Mult, Num 1, App1 (Log, e))) x
  | App2 (Mult, e1, App1 (Log, e2))
  | App2 (Mult, App1 (Log, e2), e1) ->
    let u = (App1 (Log, e2)) in
    let du = derive u x in
    let dv = e1 in
    let v = primitive dv (Light.(Var x)) in (*TODO integrate *)
    Internal2 (Minus, Internal2(Mult, Leaf u, Leaf v), Integral (App2(Mult, v, du), x))
  | _ -> Leaf expr


let integ (expr : expr) (x : string) (a : expr) (b : expr) : float =
  let res = eval_primitive expr x a b in 
  match res with 
  | Some v -> v
  | None -> 
    let expr = simpl expr in
    let expr = local_eval expr in
    let expr' = norm expr true in
    let t = build_tree expr' x in
    let restree = eval_tree t x a b in 
    (match restree with
    | Some v -> v 
    | None -> 
    let p = parts expr x in print_endline ("parts : " ^ (node_to_string p)); 0.)