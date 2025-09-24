
(*We're kinda just repeating lots of these - bit tedious -
 we could just use Token.token as our types here*)

type literal = Number of float| String of string
| True | False | Nil

type unary_operator = Not| Minus

type operator = |Equal_equal | Greater | Greater_equal
|Less | Less_equal | Not_Equal | Equal | Minus | Plus| Asterix | Slash

type ast = 
| Literal of literal
| Unary of unary
| Binary of binary
| Grouping of ast
and unary = {symbol : unary_operator ; operand: ast }
and binary = {left: ast; operator : operator ; right : ast}



let rec expression_to_string e = 
    match e with
    | Literal l -> let s = 
        match l with | Number n -> string_of_float n 
        | String x -> x | True -> "true" | False -> "false" | Nil -> "nil"
    in s  
    | Unary {symbol; operand} -> 
        let s =  match symbol with | Not -> " !" | Minus -> " -" in
        "( " ^ s  ^  expression_to_string operand ^ " )"
    | Binary {left; operator; right} -> 
        let s = match operator with
        | Equal_equal -> " == " | Equal -> " = " | Greater -> " >"  
        | Greater_equal -> " >= " | Less -> " < " | Less_equal -> " <= " 
        | Not_Equal -> " != " |  Minus -> " - " | Plus -> " + " | Asterix -> " * "
        | Slash -> " / " in
         "(" ^ s ^  expression_to_string left ^ " "^ expression_to_string right ^ ")"
    | Grouping e ->  "(" ^ "group" ^  expression_to_string e ^ ")"




let a = Unary {symbol = Minus ; operand = Literal (Number 2.)}
let b = Grouping (Binary {left = Literal (Number 3.); operator = Plus ; right = Literal (Number 4.)})

let c = Binary {left = a; operator = Asterix ; right = b}

let s = expression_to_string c
let () = print_endline s


 