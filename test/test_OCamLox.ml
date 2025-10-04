
let source = "4 - ( 2 - 6 ) * 3 "

let s = OCamLox.Lexer.init_scanner source


let s = OCamLox.Lexer.scanTokens s

let tokens = s.token_list

let p = OCamLox.Parser.init_parser tokens

let ast  = OCamLox.Parser.get_ast_exp p

let out = OCamLox.Interpreter.evaluate ast

let () = print_endline (OCamLox.Ast.expression_to_string ast); 
        print_endline "\n"; print_endline (OCamLox.Lit.lit_to_string out);;




