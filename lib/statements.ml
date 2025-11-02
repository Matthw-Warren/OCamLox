
type stmt = ExpStmt of Expressions.exp 
| PrintStmt of Expressions.exp  
| Stmt of {first: Expressions.exp; second : stmt}
| VarDecl of {id : Token.token ; value : Expressions.exp option }
| Empty


let print_stmt stmt = 
  match stmt with
  | PrintStmt x -> print_endline ("Print: " ^ (Expressions.expression_to_string x))
  | ExpStmt x -> print_endline (Expressions.expression_to_string x)
  | _ -> print_endline "Not yet dealt with"