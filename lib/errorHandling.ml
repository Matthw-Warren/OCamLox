
let report_error token message = let open Token in
  match token.token_type with 
  |Token.Eof -> (string_of_int token.line ^ " at end " ^ message) |> print_endline
  | _ ->  (string_of_int token.line ^ " at '" ^ token.lexeme  ^ "'" ^ message) |> print_endline