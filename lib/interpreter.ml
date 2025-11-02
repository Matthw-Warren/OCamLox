

(*Could be more specific with error msgs here of course
Eg. in add - if left is an int and right is a string, we could throw 
  Expected type Int, got type Bool*)








let rec evaluate_exp exp = let open Expressions in let open Token in
  match exp with 
  | Literal l -> l
  | Grouping exp -> evaluate_exp exp
  | Binary {left; operator; right} -> 
    let l = evaluate_exp left in let r = evaluate_exp right in 
    let ex = ErrorHandling.RuntimeError {token_type = operator; message = "type error"} in 
    let res = match operator with
    | Plus -> Lit.add l r ex
    | Minus -> Lit.minus l r ex
    | Asterix -> Lit.times l r ex
    | Slash -> Lit.slash l r ex
    | Equal_equal -> Lit.equal l r
    | Not_Equal -> Lit.lnot (Lit.equal l r ) 
    | Greater | Less | Greater_equal | Less_equal as c -> Token.compare c l r ex
    | t -> raise (ErrorHandling.RuntimeError {token_type =t ; message = "Invalid Token in Binary expression"})
  in res
  | Unary {  symbol = s; operand = x} -> 
    let x = evaluate_exp x in 
    let res =
      match s with
      | Not -> Lit.lnot x 
      | Minus -> Lit.minus (Lit.LInt 0) x (ErrorHandling.RuntimeError {token_type = s; message = "type error"})
      | t -> raise (ErrorHandling.RuntimeError {token_type = t; message = "Invalid Token in Binary expression"})
    in res
  | Variable tok -> tok.literal

(*If we're making a REPL -> we don't to end the whole thing here,
 would just restart the loop*)

let evaluate_stmt stmt = let open Statements in let open Token in 
    match stmt with
    | ExpStmt exp -> evaluate_exp exp
    | PrintStmt exp -> let res = 
      match evaluate_exp exp with
      | Lit.LString s -> print_endline s; Lit.LNil
      | _ -> raise (ErrorHandling.runtime_error {token_type = Print; line = -1; lexeme = "print"; literal = Lit.LNil} "Can only print strings")
      in res
    | _ -> Lit.LNil

