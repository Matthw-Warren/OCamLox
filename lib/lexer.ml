(**This is essentially just an enum - can we be more clever about this?*)
type tokenType = (**Begin with the single character tokens*) 
|Left_bracket| Right_bracket | Left_curly | Right_curly
|Comma | Dot | Minus | Plus | Semicolon | Slash | Asterix
(**Then one or two character tokens*)
|Bang | Bang_equal | Equal | Equal_equal | Greater | Greater_equal
|Less | Less_equal

(**Our literals - do we not have bools - they are treated as keywords?*)
|Identifier | String | Number 

(**Kwords*)
|And | Class | Else | False | Fun| If | Nil | Or 
|Print | Return | Super | This | True | Var | While |Eof



type 'a token = {
  token_type : tokenType; 
  lexem : string;
  literal : 'a;
  line : int
}



module Scanner : sig
  type t
  val scan : string-> string list
end = struct
  type t = string
  let scan s = [s]
end













let run source = print_endline source



let runFile path = 
  let inchannel = open_in path in
  let source = really_input_string inchannel (in_channel_length inchannel)
  in run source 



let rec repl s = 
  print_string "> "; 
  flush stdout; 
  let inc = stdin in 
  let line = input_line inc in 
  if 
    String.length line = 0 then s 
  else  if  String.ends_with ~suffix:";;" line then
    s^ "\n" ^ line
  else
    repl (s ^ "\n" ^ line);; 

let runPromt () = 
  let text = repl "" in
  run text





exception ArgError


let runLexer args = 
  match args with
  | None -> runPromt ()
  | Some x -> runFile x

