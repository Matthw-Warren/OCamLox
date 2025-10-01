type parseError = {line:int ; lexeme : string ; message: string }
exception ParseError of parseError



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

  