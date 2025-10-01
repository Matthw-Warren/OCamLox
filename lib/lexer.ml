open Token

type scanner = {
  source : string;
  token_list : token list;
  start :int;
  current :int;
  line: int;
}

exception UnclosedString
exception UnexpectedCharacter of char * int
exception UnknownCharacter of char * int


let init_scanner source = {source; token_list = []; start= 0;
 current = 0; line =1;
}

let scanner_at_end scanner = scanner.current >= String.length scanner.source

let advance_scanner scanner = {scanner with current = scanner.current +1}
  
let get_char scanner =
  if scanner.current >= String.length scanner.source then None
  else Some (String.get scanner.source (scanner.current))

let peek scanner = 
  match get_char scanner with
  |None -> '\x00'
  |Some c -> c

let peek_next scanner =
  if (scanner.current +1 ) >= String.length scanner.source then None
  else Some (String.get scanner.source (scanner.current +1))  

let increment_line scanner = {scanner with line = scanner.line +1}


let add_token scanner t = 
  let new_token = {token_type = t; 
  lexeme = String.sub scanner.source scanner.start (scanner.current - scanner.start);
  line = scanner.line;
  literal = Lit.create_LNil ()
    } in
  {scanner with token_list = List.append scanner.token_list [new_token]}


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




let rec scan_string scanner =
  let c = get_char scanner in 
  match c with 
  |None -> raise UnclosedString
  |Some c -> 
    match c with
    | '"' ->  advance_scanner scanner
    | '\n' -> scan_string (advance_scanner (increment_line scanner))
    | _ -> scan_string (advance_scanner scanner)

let add_string scanner = 
  let scanner = scan_string scanner in
  let stringliteral = String.sub scanner.source (scanner.start +1) (scanner.current - scanner.start -2) in 
  let tok = {token_type = String;
   lexeme = stringliteral;
    line = scanner.line;
    literal = Lit.create_LString stringliteral} in 
  {scanner with token_list = List.append scanner.token_list [tok]}
  

let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z'  | '_'-> true | _ -> false

let is_alphanum x = is_digit x || is_alpha x





let rec scan_num scanner = 
  let c = get_char scanner in 
  match c with
  |None| Some ' '| Some '\n' -> scanner
  |Some '.' -> 
  let dotcase scanner = 
    match peek_next scanner with
    |None -> raise (UnexpectedCharacter ('.', scanner.line))
    |Some x when (is_digit x || x = ' ' || x = '\n') -> scan_num( advance_scanner scanner)
    |Some x -> raise (UnexpectedCharacter (x, scanner.line))
  in dotcase scanner
  |Some c when is_digit c -> scan_num (advance_scanner scanner)
  |Some c -> raise (UnexpectedCharacter (c, scanner.line))

let add_num scanner = 
  let scanner = scan_num scanner in
  let numstring = String.sub scanner.source (scanner.start) (scanner.current - scanner.start) 
    in let tok = {token_type = Number; lexeme=  numstring; 
    line = scanner.line; literal= Lit.create_LNum (float_of_string numstring)}
  in {scanner with token_list = List.append scanner.token_list [tok]}


let rec scan_word scanner = 
  let c = get_char scanner in 
  match c with
  | Some ' '|  Some '\n' | None -> scanner
  | Some x when is_alphanum x -> scan_word (advance_scanner scanner)
  | Some c -> raise (UnexpectedCharacter (c, scanner.current))

let add_word scanner = 
  let scanner = scan_word scanner in
  let word_string = String.sub scanner.source (scanner.start) (scanner.current - scanner.start) in
  let token = {token_type = map_keyword word_string; lexeme = word_string; 
    line = scanner.line; literal  = Lit.create_LNil () } in
  {scanner with token_list = List.append scanner.token_list [token]}



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
    | ' '| '\r' | '\t' -> scanner
    | '\n' ->  increment_line scanner
    | '"' -> add_string scanner
    | c when (is_digit c || c = '.') -> add_num scanner
    | c when (is_alpha c || c = '_') -> add_word scanner
    | c -> raise (UnknownCharacter (c,scanner.line)) )


let rec scanTokens scanner = 
  if scanner_at_end scanner then 
    let tokenlist = List.append scanner.token_list [{token_type = Eof;
   lexeme = ""; line = scanner.line ; literal = Lit.create_LNil () } ] in 
  {scanner with token_list = tokenlist}
  else let scanner = {scanner with start = scanner.current} in 
  scanTokens (scanToken scanner)






