
let source = "\"testing\" - ( 2 - 1 ) * 3 == true"

let s = OCamLox.Lexer.init_scanner source


let s = OCamLox.Lexer.scanTokens s

let tokens = s.token_list

let p = OCamLox.Parser.init_parser tokens

let ast  = OCamLox.Parser.get_ast_exp p

let () = print_endline (OCamLox.Ast.expression_to_string ast)



