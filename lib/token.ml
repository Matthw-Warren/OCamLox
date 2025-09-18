module Lit : sig
  type l
  val lit_to_string : l -> string
  val create_LNil : unit -> l
  val create_LInt : int -> l
  val create_LBool : bool -> l
  val create_LNum: float -> l
  val create_LString : string -> l
end = struct
  type l = 
  |LBool of bool 
  |LInt of int
  |LNum of float
  |LString of string
  |LNil
  
  let create_LBool b = LBool b
  let create_LInt x = LInt x
  let create_LNum x = LNum x
  let create_LString x = LString x
  let create_LNil () = LNil 

  let lit_to_string l = 
  match l with
  |LBool b -> string_of_bool b
  |LInt x -> string_of_int x
  |LNum x -> string_of_float x
  |LString s -> s
  |LNil -> "NIL"
end

type tokenType = (*Begin with single char ones*) |Left_bracket
| Right_bracket | Left_curly | Right_curly
|Comma | Dot | Minus | Plus | Semicolon | Slash | Asterix 
(*Then single or double chars*) |Not | Not_Equal | Equal 
|Equal_equal | Greater | Greater_equal
|Less | Less_equal
(*Literals*) |Identifier  | String | Number 
(*Keywords*)|And | Class | Else | False | Fun| If | Nil | Or 
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
