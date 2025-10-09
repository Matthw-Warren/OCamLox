type parseError = {line:int ; lexeme : string ; message: string }
[@@deriving show]
exception ParseError of parseError

type runtimeError = {token_type :Token.tokenType ; message:string}
[@@deriving show]
exception RuntimeError of runtimeError


let had_Error = ref false


let report_error line where message =
  print_endline ("[Line " ^ string_of_int line ^ "] Error " ^ where ^ ": " ^ message )

let error token message = let open Token in 
  if token.token_type = Eof then 
    report_error token.line "at end" message
  else 
    report_error token.line ("at '" ^ token.lexeme ^ "'") message
  
let parse_error token message = let open Token in 
  error token message; 
  ParseError {line = token.line; lexeme = token.lexeme; message = message}

let runtime_error token message = let open Token in 
  error token message; 
  RuntimeError {token_type = token.token_type; message = message}
