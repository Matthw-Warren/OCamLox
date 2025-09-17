(* 
module Lit : sig
  type l
  val lit_to_string : l -> string
  val create_literal : unit -> l
end = struct
  type l = 
  |LBool of bool 
  |LInt of int
  |LNum of float
  |LString of string
  |LNil
  
  let create_literal () = LNil 

  let lit_to_string l = 
  match l with
  |LBool b -> string_of_bool b
  |LInt x -> string_of_int x
  |LNum x -> string_of_float x
  |LString s -> s
  |LNil -> "NIL"
end *)

type tokenType = (*Begin with single char ones*) |Left_bracket
| Right_bracket | Left_curly | Right_curly
|Comma | Dot | Minus | Plus | Semicolon | Slash | Asterix 
(*Then single or double chars*) |Not | Not_Equal | Equal 
|Equal_equal | Greater | Greater_equal
|Less | Less_equal
(*Literals*) |Identifier of string | String of string | Number of int 
(*Keywords*)|And | Class | Else | False | Fun| If | Nil | Or 
|Print | Return | Super | This | True | Var | While |Eof
[@@deriving show]

type token = {
  token_type : tokenType; 
  lexeme : string;
  line : int;
  (* literal : Lit.l *)
}

let token_to_string tok = 
  "Type: " ^ show_tokenType tok.token_type ^ " Lexeme: " ^ tok.lexeme ^ " on line: " ^ string_of_int tok.line