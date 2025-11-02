
let source = " cheese; "

let s = OCamLox.Lexer.init_scanner source


let s = OCamLox.Lexer.scanTokens s

let tokens = s.token_list

let () = OCamLox.Token.print_tokens tokens

let p = OCamLox.Parser.init_parser tokens


let p = OCamLox.Parser.parse_stmts p

let rec eval_statements stmtList = 
match stmtList with
|[] -> ()
| s::t ->  (OCamLox.Lit.lit_to_string (OCamLox.Interpreter.evaluate_stmt s))
                |> print_endline ; eval_statements t
       
let () = eval_statements p.stmtList

(* let exp  = OCamLox.Parser.get_expression p

let out = OCamLox.Interpreter.evaluate exp

let () = print_endline (OCamLox.Expressions.expression_to_string exp); 
        print_endline "\n"; print_endline (OCamLox.Lit.lit_to_string out);; 
 *)

