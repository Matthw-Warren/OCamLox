
type tokenType = (*Begin with single char ones*) |Left_bracket
| Right_bracket | Left_curly | Right_curly
|Comma | Dot | Minus | Plus | Semicolon | Slash | Asterix 
(*Then single or double chars*) |Not | Not_Equal | Equal 
|Equal_equal | Greater | Greater_equal
|Less | Less_equal
(*Literals*) |Identifier of string | String of string | Number of int 
(*Keywords*)|And | Class | Else | False | Fun| If | Nil | Or 
|Print | Return | Super | This | True | Var | While |Eof



type token = {
  token_type : tokenType; 
  lexem : string;
  line : int
}

type scanner = {
  source : string;
  token_list : token list;
  start_point :int;
  current :int;
  line: int;
}

let init_scanner source = {source; token_list = []; start_point= 0;
 current = 0; line =1;
}

let scanner_at_end scanner = scanner.current >= String.length scanner.source

let advance_scanner scanner = {scanner with current = scanner.current +1}
  
let get_char scanner =
  if scanner.current >= String.length scanner.source then None
  else Some (String.get scanner.source (scanner.current))



let add_token scanner t = 
  let new_token = {token_type = t; 
  lexem = String.sub scanner.source scanner.start_point scanner.current;
  line = scanner.line;
    } in
  {scanner with token_list = List.append scanner.token_list [new_token]}


exception UnknownCharacter of char

let match_next scanner expected = 
  let new_c = get_char scanner in 
  match new_c with
  |None -> (false, scanner)
  |Some c -> 
    if c = expected then 
      let scanner = advance_scanner scanner in  (true, scanner)
    else (false ,scanner)



let rec blitz_comment scanner = 
  match get_char scanner with
  |  None | Some '\n' -> scanner
  | _ -> blitz_comment (advance_scanner scanner)

let add_comment scanner = 
  match get_char (advance_scanner scanner) with
  | None -> add_token scanner Slash
  | Some c -> if c = '/' then blitz_comment scanner
  else add_token scanner Slash


let scanToken scanner = 
  let currchar = get_char scanner in 
  let scanner = advance_scanner scanner in 
  match currchar with
  | None -> scanner
  | Some c -> (match c with 
    | '(' ->  add_token scanner Left_bracket
    | ')'-> add_token scanner Right_bracket
    | '{'-> add_token scanner Left_curly
    | '}'-> add_token scanner Right_curly
    | ','-> add_token scanner Comma
    | '.'-> add_token scanner Dot
    | '-'-> add_token scanner Minus
    | '+'-> add_token scanner Plus
    | ';'-> add_token scanner Semicolon
    | '*'-> add_token scanner Asterix
    | '!' -> let (matched, scanner) = match_next scanner '='
      in if matched then add_token scanner Not_Equal
      else add_token scanner Not
    | '=' -> let (matched, scanner) = match_next scanner '='
      in if matched then add_token scanner Equal_equal
      else add_token scanner Equal
    | '<' -> let (matched, scanner) = match_next scanner '='
      in if matched then add_token scanner Less_equal
      else add_token scanner Less
    | '>' -> let (matched, scanner) = match_next scanner '='
      in if matched then add_token scanner Greater_equal
      else add_token scanner Greater
    | '/' ->  add_comment scanner

    | c -> raise (UnknownCharacter c) )









