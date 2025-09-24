(*Note that the records indeed capture our associativity rules!
 - left can  be factor -
 whereas right can only be unary
This is the left associativity of  * and / *)

(*So this type is just faithfully representing what is going on in the CFG.
We actually implement these rules using our parser functions below (just as we 
implement the ideas of Regular languages and such with our lexer - we dont explicitly 
need a class for the possible strings in the langauge defined by a Regex.)*)

type expression = Equality of equality 

and equality = Comparison of comparison
| EqualityExp of {operator : Token.tokenType ; left : equality ; right : comparison}

and comparison = Term of term 
| ComparisonExp of {operator: Token.tokenType; left : comparison; right : term }

and term = Factor of factor 
| TermExp of {operator: Token.tokenType ; left: term; right:factor}

and factor = Unary of unary 
| FactorExp of {operator: Token.tokenType; left : factor ; right : unary }

and unary = Primary of primary 
|UnaryExp of {symbol : Token.tokenType; unary : unary}

and primary = Literal of Token.Lit.l | Brackets of expression


(* The problem here is that if I want to define the expression "1" 
  (ie. the literal int)
  Then I have the construct this as 
Equalilty Comparison Term Factor Unary Literal 1 
While this is what we're actually doing in the derivation 
- this is quite tedious!? - but perhaps worth it!*)

exception TokenListExceeded


type parser = {tokens : Token.token list ; current :int}

let get_token parser = List.nth parser.tokens parser.current

let get_token_type parser = let t = get_token parser in t.token_type

let get_previous parser = List.nth parser.tokens (parser.current -1)

let at_end parser = (get_token_type parser) = Token.Eof

let advance_parser parser = if at_end parser then raise TokenListExceeded 
else {parser with current = parser.current +1 }

let check_token_type parser token_type = if at_end parser then false
else (get_token_type parser) = token_type




(*Ah - the CFG being ambiguous is simply a way of defining rigorously HOW we parse
our token string - the code implements this and returns an AST - I see.
Our aim is to create an AST from our token list.*)


let rec get_comparison_ast parser = Ast.Literal.Token.Lit.LBool true

let rec get_eq_ast parser exp = let c = get_token_type parser in 
  match c with 
  | Token.Equal_equal  | Token.Not_Equal -> 
  | _ -> 

let get_ast parser = c_equality parser

