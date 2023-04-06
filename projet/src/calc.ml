open Syntax

let rec eval expr =
  match expr with
  | Num n -> n
  | App2 (Plus, e1, e2) -> eval e1 + eval e2
  | _ -> failwith "Not implemented"

let () =
  let ast = Parser.expr Lexer.token (Lexing.from_channel stdin) in
  let result = eval ast in
  print_int result; print_newline ()