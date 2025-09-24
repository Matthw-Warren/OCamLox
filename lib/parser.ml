(*Note that the records indeed capture our associativity rules!
 - left can  be factor -
 whereas right can only be unary
This is the left associativity of  * and / *)

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
