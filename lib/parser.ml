
exception TokenListExceeded
exception UnexpectedToken of Token.tokenType

type parser = {stmtList : Statements.stmt list ; 
  tokens : Token.token list ; current :int}


let init_parser tokens = {stmtList = [];  tokens ; current =0}

(*Surely a more efficeint way to do this - use list structure 
- will return to*)


let get_token parser = List.nth parser.tokens parser.current
 

let get_token_type parser = let t = get_token parser in t.token_type

let get_previous parser = List.nth parser.tokens (parser.current -1)

let at_end parser = (get_token_type parser) = Token.Eof

let advance_parser parser = 
  if at_end parser then 
  let tok = get_previous parser in raise (ErrorHandling.parse_error tok "Exceeded tokens before expression complete")
else {parser with current = parser.current +1 }

let check_token_type parser token_type = if at_end parser then false
else (get_token_type parser) = token_type


let munch parser token_type = let c = get_token_type parser in 
if token_type = c then advance_parser parser
else raise (UnexpectedToken c)


let raise_parser_error token message= 
  ErrorHandling.report_error token message


exception TempExn of Token.token


let rec get_exp parser = get_eq_exp parser

and get_eq_exp parser =  
  let (left, parser) = get_comparison_exp parser in 
  get_eq_exp_rec parser left

and get_eq_exp_rec parser left =
  let c = get_token_type parser in 
    match c with 
    | Token.Equal_equal  | Token.Not_Equal -> 
      let parser = advance_parser parser in
      let (right , parser ) = get_comparison_exp parser in 
      let new_left = Expressions.Binary {operator = c; left = left; right = right} in
      get_eq_exp_rec parser new_left
    | Token.Semicolon -> (left, advance_parser parser) 
    | Token.Eof -> (left,parser) 
    | _ -> (left,parser) 

and get_comparison_exp parser = 
  let (left, parser) = get_term_exp parser in 
  get_comparison_exp_rec parser left

and get_comparison_exp_rec parser left =                                                                                         
  let c = get_token_type parser in 
  match c with 
  |Token.Greater| Token.Greater_equal | Token.Less | Token.Less_equal ->
    let parser = advance_parser parser in
    let (right, parser) =  get_term_exp parser in 
    let new_left = Expressions.Binary {operator = c; left = left; right = right} in
    get_comparison_exp_rec parser new_left
  | Token.Semicolon -> (left, advance_parser parser) 
  | Token.Eof -> (left,parser) 
  | _ -> (left,parser) 

and get_term_exp parser = 
  let (left,parser)  = get_factor_exp parser in
  get_term_exp_rec parser left  

and get_term_exp_rec parser left = 
  let c = get_token_type parser in 
  match c with
  |Token.Plus | Token.Minus -> 
  let parser = advance_parser parser in
  let (right, parser) = get_factor_exp parser in 
  let new_left = Expressions.Binary {operator = c; left = left; right = right} in
  get_term_exp_rec parser new_left
  | Token.Semicolon -> (left, advance_parser parser) 
  | Token.Eof -> (left,parser) 
  | _ -> (left,parser) 

and get_factor_exp parser = 
  let (left,parser)  = get_unary_exp_rec parser in
  get_factor_exp_rec parser left  

and get_factor_exp_rec parser left = 
  let c = get_token_type parser in 
  match c with
  |Token.Slash | Token.Asterix -> 
  let parser = advance_parser parser in
  let (right, parser) = get_unary_exp_rec parser in 
  let new_left = Expressions.Binary {operator = c; left = left; right = right} in
  get_factor_exp_rec parser new_left
  | Token.Semicolon -> (left, advance_parser parser) 
  | Token.Eof -> (left,parser) 
  | _ -> (left,parser) 
  
and get_unary_exp_rec parser = 
  let c = get_token_type parser in 
  match c with 
  | Token.Minus | Token.Not ->   
  let parser = advance_parser parser in
  let (right,parser)  = get_unary_exp_rec parser in 
 ( Expressions.Unary {symbol = c ; operand  = right}, parser)
 | _ -> get_primary_exp parser 

and get_primary_exp parser  =
  let c = get_token_type parser in
  let parser = advance_parser parser in 
  match c with 
  | Token.False -> (Expressions.Literal (Lit.LBool false), parser)
  | Token.True -> (Expressions.Literal (Lit.LBool true), parser)
  | Token.Nil -> (Expressions.Literal Lit.LNil, parser)
  
  | Token.Number | Token.String -> let t = get_previous parser in 
    let l = t.literal in (Expressions.Literal l, parser)
  | Token.Left_bracket -> 
    let (exp, parser) = get_exp parser in 
    let parser = munch parser Token.Right_bracket in
      (Expressions.Grouping exp, parser)
  | Token.Eof -> (Expressions.Literal Lit.LNil, parser)
  | Token.Semicolon -> (Expressions.Literal Lit.LNil ,parser)
  |_ -> let token = get_previous parser in raise
    (ErrorHandling.parse_error token "Unexpected Token in parse")



let get_expression parser = let (a,_) = get_exp parser in a


let rec synchronise parser = let open Token in 
    if at_end parser then parser else 
    let parser = advance_parser parser in 
    if (get_previous parser).token_type = Semicolon then parser
    else match get_token_type parser with
    | Class | Fun | Var | For | If | While | Print | Return -> parser
    | _ -> synchronise parser





let rec get_stmt parser = 
  let t = get_token_type parser in 
  match t with
  | Token.Print -> get_print_stmt (advance_parser parser)
  | _ -> let (exp, parser) = get_exp parser 
    in (Statements.ExpStmt exp, parser)
  
and get_print_stmt parser = let (exp, parser) = get_exp parser in (Statements.PrintStmt exp, parser)
 



let rec parse_stmts parser = 
  match get_token_type parser with 
  | Token.Eof -> parser
  | _ -> let (stmt, parser) = get_stmt parser 
  in let parser = parse_stmts parser
  in {parser with stmtList = (List.append [stmt] parser.stmtList ) }



let parse tokens = let open ErrorHandling in
   let parser = init_parser tokens in 
    try (get_expression parser)
    with ParseError _ -> Expressions.Literal (Lit.LNil)