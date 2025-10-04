
(*We're kinda just repeating lots of these - bit tedious -
 we could just use Token.token as our types here*)

type ast = 
| Literal of Lit.l
| Unary of {symbol : Token.tokenType ; operand: ast }
| Binary of {left: ast; operator : Token.tokenType ; right : ast}
| Grouping of ast

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
        | Equal_equal -> " == " | Equal -> " = " | Greater -> " >"  
        | Greater_equal -> " >= " | Less -> " < " | Less_equal -> " <= " 
        | Not_Equal -> " != " |  Minus -> " - " | Plus -> " + " | Asterix -> " * "
        | Slash -> " / " | _ -> raise UnexpectedOperator in
         "(" ^ s ^  expression_to_string left ^ " "^ expression_to_string right ^ ")"
    | Grouping e ->  "(" ^ "group" ^  expression_to_string e ^ ")"



(* 
let a = Unary {symbol = Minus ; operand = Literal (Lit.LNum 2.)}
let b = Grouping (Binary {left = Literal (Lit.LNum 3.); operator = Token.Plus ; right = Literal (Lit.LNum 4.)})

let c = Binary {left = a; operator = Token.Asterix ; right = b}

let s = expression_to_string c
let () = print_endline s *)


 