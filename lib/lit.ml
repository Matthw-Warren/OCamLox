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



  (* FUCKK *)
  
let add left right exn =
  match left with
  |LInt x -> let result = 
    match right with 
    |LInt y -> LInt (x+y)
    |LNum y -> LNum (float_of_int x +. y)
    | _ -> raise exn
  in result
  |LNum x -> let result = 
    match right with 
    |LNum y -> LNum (x +. y)
    |LInt y -> LNum (x +. float_of_int y) 
    |_ -> raise exn
  in result 
  | LString x -> let result  = 
  match right with 
  | LString y -> LString( x^y)
  | _ ->  raise exn
  in result
  | _ -> raise exn
    
let minus left right exn = 
  match left with 
  | LInt x -> let res = 
    match right with 
    |LInt y -> LInt (x-y)
    |LNum y -> LNum (float_of_int x -. y)
    | _ -> raise exn
  in res
  | LNum x -> let result = 
    match right with 
    |LNum y -> LNum (x -. y)
    |LInt y -> LNum (x -. float_of_int y) 
    |_ -> raise exn
  in result 
  | _ -> raise exn

let times left right exn = 
  match left with 
  | LInt x -> let res = 
    match right with 
    |LInt y -> LInt (x* y)
    |LNum y -> LNum (float_of_int x *. y)
    | _ -> raise exn
  in res
  | LNum x -> let result = 
    match right with 
    |LNum y -> LNum (x *. y)
    |LInt y -> LNum (x *. float_of_int y) 
    |_ -> raise exn
  in result 
  | _ -> raise exn

  
let slash left right exn = 
  match left with 
  | LInt x -> let res = 
    match right with 
    |LInt y -> LInt (x / y)
    |LNum y -> LNum (float_of_int x /. y)
    | _ -> raise exn
  in res
  | LNum x -> let result = 
    match right with 
    |LNum y -> LNum (x /. y)
    |LInt y -> LNum (x /. float_of_int y) 
    |_ -> raise exn
  in result 
  | _ -> raise exn


let isTrue l =
  match l with
  | LBool _ as x -> x
  | LInt x -> if x =0 then LBool false else LBool true
  | LNum x -> if x = 0. then LBool false else LBool true
  | LString _ -> LBool true
  | LNil -> LBool false

let equal left right = 
  match (left, right) with 
  | (LNum x, LInt y )-> LBool (x = (float_of_int y))
  | (LInt x, LNum y )-> LBool (y = (float_of_int x)) 
  | (l1,l2) -> LBool (l1 =l2)

(* Note - there is a problem here in int and float  *)



let lnot l = 
  match isTrue l with
  |LBool x -> LBool (not x)
  | _ -> raise End_of_file 



(*Now - also want to do comparisons - but can just use a switch statement 
(ie. pattern matching) here with the TokenType type in Token.ml - cant
do this here due to circular imports so have exported over there.


Have stuck in here for now - need to sort.

Also can't import ErrorHandling!*)