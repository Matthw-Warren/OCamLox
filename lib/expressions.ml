
type exp = 
| Literal of Lit.l
| Unary of {symbol : Token.tokenType ; operand: exp }
| Binary of {left: exp; operator : Token.tokenType ; right : exp}
| Grouping of exp

exception UnexpectedOperator



let rec expression_to_string e = let open Token in
    match e with
    | Literal l -> let s = 
        match l with | Lit.LInt x -> string_of_int x | Lit.LNum x -> string_of_float x
        | Lit.LString x -> x | Lit.LBool b -> string_of_bool b | Lit.LNil -> "nil"
    in s  
    | Unary {symbol; operand} -> 
        let s =  match symbol with | Not -> " !" | Minus -> " -" | _ -> raise UnexpectedOperator in
        "( " ^ s  ^  expression_to_string operand ^ " )"
    | Binary {left; operator; right} -> 
        let s = match operator with
        | Equal_equal -> " == " | Equal -> " = " | Greater -> " > "  
        | Greater_equal -> " >= " | Less -> " < " | Less_equal -> " <= " 
        | Not_Equal -> " != " |  Minus -> " - " | Plus -> " + " | Asterix -> " * "
        | Slash -> " / " | _ -> raise UnexpectedOperator in
         "(" ^ s ^  expression_to_string left ^ " "^ expression_to_string right ^ ")"
    | Grouping e ->  "(" ^ "group" ^  expression_to_string e ^ ")"




 