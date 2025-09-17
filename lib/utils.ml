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

