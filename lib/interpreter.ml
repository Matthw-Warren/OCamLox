

(*Could be more specific with error msgs here of course
Eg. in add - if left is an int and right is a string, we could throw 
  Expected type Int, got type Bool*)

let rec evaluate ast = let open Ast in let open Token in
  match ast with 
  | Literal l -> l
  | Grouping ast -> evaluate ast
  | Binary {left; operator; right} -> 
    let l = evaluate left in let r = evaluate right in 
    let ex = ErrorHandling.RuntimeError {token = operator; message = "type error"} in 
    let res = match operator with
    | Plus -> Lit.add l r ex
    | Minus -> Lit.minus l r ex
    | Asterix -> Lit.times l r ex
    | Slash -> Lit.slash l r ex
    | Equal_equal -> Lit.equal l r
    | Not_Equal -> Lit.lnot (Lit.equal l r ) 
    | Greater | Less | Greater_equal | Less_equal as c -> Token.compare c l r ex
    | t -> raise (ErrorHandling.RuntimeError {token =t ; message = "Invalid Token in Binary expression"})
  in res
  | Unary {  symbol = s; operand = x} -> 
    let x = evaluate x in 
    let res =
      match s with
      | Not -> Lit.lnot x 
      | Minus -> Lit.minus (Lit.LInt 0) x (ErrorHandling.RuntimeError {token = s; message = "type error"})
      | t -> raise (ErrorHandling.RuntimeError {token = t; message = "Invalid Token in Binary expression"})
    in res

(*If we're making a REPL -> we don't to end the whole thing here,
 would just restart the loop*)
