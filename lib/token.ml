
type tokenType = (*Begin with single char ones*) |Left_bracket
|Right_bracket | Left_curly | Right_curly
|Comma | Dot | Minus | Plus | Semicolon | Slash | Asterix 
(*Then single or double chars*) |Not | Not_Equal | Equal 
|Equal_equal | Greater | Greater_equal
|Less | Less_equal
(*Literals*) |Identifier  | String | Number 
(*Keywords*)|And | Class | Else | False | For | Fun| If | Nil | Or 
|Print | Return | Super | This | True | Var | While |Eof 
[@@deriving show]

type token = {
  token_type : tokenType; 
  lexeme : string;
  line : int;
  literal : Lit.l
}

let keywords = 
  ["and" , And ; "class ",  Class ; "else",  Else ;
  "false" ,  False ; "fun" ,  Fun; "if" ,  If ; "nil" ,  Nil ;
    "or" ,  Or  ; "print" , Print ; "return" ,  Return ;
  "super" ,  Super ; "this" ,  This ; "true" ,  True ;
  "var" ,  Var ; "while" ,  While  ]

let map_keyword s = 
let opt = List.find_opt (fun (str, _ ) -> (s = str) ) keywords in
  match opt with
  |None -> Identifier 
  |Some (_, token_type) -> token_type



let token_to_string tok = 
  "Type: " ^ show_tokenType tok.token_type ^ " Lexeme: " ^ tok.lexeme ^ " on line: " ^ string_of_int tok.line 


let print_tokens tokens = let string_list = List.map token_to_string tokens in 
  let rec print_list l  = 
  match l with
  | [] -> print_endline ""
  | s::tl -> print_endline (s ^ "\n"); print_list tl;
  in print_list string_list




let compare operator left right exn = let open Lit in 
  match operator with
  | Less -> let res = 
    match (left,right) with
    | (LInt x, LInt y) -> LBool (x<y)
    | (LNum x, LNum y) -> LBool (x<y)
    | (LInt x, LNum y) -> LBool (float_of_int x < y)
    | (LNum x, LInt y) -> LBool (x < float_of_int y)
    | _ -> raise exn
    in res
  | Less_equal -> let res = 
    match (left,right) with
    | (LInt x, LInt y) -> LBool (x <= y)
    | (LNum x, LNum y) -> LBool (x <= y)
    | (LInt x, LNum y) -> LBool (float_of_int x <= y)
    | (LNum x, LInt y) -> LBool (x <= float_of_int y)
    | _ -> raise exn
    in res
  | Greater -> let res = 
    match (left,right) with
    | (LInt x, LInt y) -> LBool (x> y)
    | (LNum x, LNum y) -> LBool (x> y)
    | (LInt x, LNum y) -> LBool (float_of_int x > y)
    | (LNum x, LInt y) -> LBool (x > float_of_int y)
    | _ -> raise exn
    in res
  | Greater_equal -> let res = 
    match (left,right) with
    | (LInt x, LInt y) -> LBool (x >= y)
    | (LNum x, LNum y) -> LBool (x >= y)
    | (LInt x, LNum y) -> LBool (float_of_int x >= y)
    | (LNum x, LInt y) -> LBool (x >= float_of_int y)
    | _ -> raise exn
    in res
  | _ -> raise exn

