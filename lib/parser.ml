
exception TokenListExceeded
exception UnexpectedToken of Token.tokenType

type parser = {tokens : Token.token list ; current :int}


let init_parser tokens = {tokens ; current =0}

(*Surely a more efficeint way to do this - use list structure 
- will return to*)


let get_token parser = List.nth parser.tokens parser.current
 

let get_token_type parser = let t = get_token parser in t.token_type

let get_previous parser = List.nth parser.tokens (parser.current -1)

let at_end parser = (get_token_type parser) = Token.Eof

let advance_parser parser = if at_end parser then raise TokenListExceeded
else {parser with current = parser.current +1 }

let check_token_type parser token_type = if at_end parser then false
else (get_token_type parser) = token_type


let munch parser token_type = let c = get_token_type parser in 
if token_type = c then advance_parser parser
else raise (UnexpectedToken c)


let raise_parser_error token message= 
  ErrorHandling.report_error token message



let rec get_ast parser = get_eq_ast parser

and get_eq_ast parser = 
  let (left, parser) = get_comparison_ast parser in 
  get_eq_ast_rec parser left

and get_eq_ast_rec parser left =
  let c = get_token_type parser in 
    match c with 
    | Token.Equal_equal  | Token.Not_Equal -> 
      let parser = advance_parser parser in
      let (right , parser ) = get_comparison_ast parser in 
      let new_left = Ast.Binary {operator = c; left = left; right = right} in
      get_eq_ast_rec parser new_left
    | _ -> (left,parser) 

and get_comparison_ast parser = 
  let (left, parser) = get_term_ast parser in 
  get_comparison_ast_rec parser left

and get_comparison_ast_rec parser left = 
  let c = get_token_type parser in 
  match c with 
  |Token.Greater| Token.Greater_equal | Token.Less | Token.Less_equal ->
    let parser = advance_parser parser in
    let (right, parser) =  get_term_ast parser in 
    let new_left = Ast.Binary {operator = c; left = left; right = right} in
    get_comparison_ast_rec parser new_left
  | _ -> (left,parser)

and get_term_ast parser = 
  let (left,parser)  = get_factor_ast parser in
  get_term_ast_rec parser left  

and get_term_ast_rec parser left = 
  let c = get_token_type parser in 
  match c with
  |Token.Plus | Token.Minus -> 
  let parser = advance_parser parser in
  let (right, parser) = get_factor_ast parser in 
  let new_left = Ast.Binary {operator = c; left = left; right = right} in
  get_term_ast_rec parser new_left
  | _ -> (left, parser)

and get_factor_ast parser = 
  let (left,parser)  = get_unary_ast_rec parser in
  get_factor_ast_rec parser left  

and get_factor_ast_rec parser left = 
  let c = get_token_type parser in 
  match c with
  |Token.Slash | Token.Asterix -> 
  let parser = advance_parser parser in
  let (right, parser) = get_unary_ast_rec parser in 
  let new_left = Ast.Binary {operator = c; left = left; right = right} in
  get_factor_ast_rec parser new_left
  | _ -> (left, parser)
 
and get_unary_ast_rec parser = 
  let c = get_token_type parser in 
  match c with 
  | Token.Minus | Token.Not ->   
  let parser = advance_parser parser in
  let (right,parser)  = get_unary_ast_rec parser in 
 ( Ast.Unary {symbol = c ; operand  = right}, parser)
 | _ -> get_primary_ast parser 

and get_primary_ast parser  =
  let c = get_token_type parser in
  let parser = advance_parser parser in 
  match c with 
  | Token.False -> (Ast.Literal (Lit.LBool false), parser)
  | Token.True -> (Ast.Literal (Lit.LBool true), parser)
  | Token.Nil -> (Ast.Literal Lit.LNil, parser)
  
  | Token.Number | Token.String -> let t = get_previous parser in 
    let l = t.literal in (Ast.Literal l, parser)
  | Token.Left_bracket -> 
    let (exp, parser) = get_ast parser in 
    let parser = munch parser Token.Right_bracket in
      (Ast.Grouping exp, parser) 

  |_ -> let token = get_previous parser in raise
    (ErrorHandling.parse_error token "Unexpected Token in parse")



let get_ast_exp parser = let (a,_) = get_ast parser in a


let rec synchronise parser = let open Token in 
    if at_end parser then parser else 
    let parser = advance_parser parser in 
    if (get_previous parser).token_type = Semicolon then parser
    else match get_token_type parser with
    | Class | Fun | Var | For | If | While | Print | Return -> parser
    | _ -> synchronise parser




let parse tokens = let open ErrorHandling in
   let parser = init_parser tokens in 
    try (get_ast_exp parser)
    with ParseError _ -> Ast.Literal Lit.LNil