let source = "<< != 
(( <))  !"

let s = OCamLox.Lexer.init_scanner source



let s = OCamLox.Lexer.scanTokens s

let tokens = s.token_list



let print_tokens tokens = let string_list = List.map OCamLox.Token.token_to_string tokens in 
  let rec print_list l  = 
  match l with
  | [] -> print_endline ""
  | s::tl -> print_endline (s ^ "\n"); print_list tl;
  in print_list string_list

let () = print_tokens tokens