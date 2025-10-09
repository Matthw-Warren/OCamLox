# Lexer Notes

in_channel in ocaml is an abstract type (ie. we dont know the concrete implementation) - handles inputs.

I think the basic implementation is having something like an mutable variable : int which stores where in the file (or more generally - kinda the stream) we are. 
    This can be accessed using pos_in, and modified using seek_in

input_line, input_char, and so on all do what one would expect.

(read_line and such are functions which are called kinda through the standard input stdin)

If I set some variable a = stdin, this has type in_channel - but when initialised it has no content. I assume that the underlying types for stdin and if I construct an in_channel using, say, open_in (so reading from a file) are different, because when something like input_char is called, with stdin we are prompted to write this!


Yes - the stdin is special - has some associated special functions cf. https://ocaml.org/manual/5.2/api/Stdlib.html#2_Inputfunctionsonstandardinput



out_channels have a Buffer - eg. If I'm trying to write to a file (or command line) then the buffer is a store of what is going to be written. 



# Parser Notes

Ahhhhh - so rather than creating a type that is true to the CFG - what we really want a parser to do is to create an AST from a string of tokens USING the rules of that grammar to decide how to do so.
- Ok I think I understand now.

So I had created the expression type - which was essentially meant to be an element of the Language of the CFG that we were using to define the syntax:

(*Note that the records indeed capture our associativity rules!
 - left can  be factor -
 whereas right can only be unary
This is the left associativity of  * and / *)

(*So this type is just faithfully representing what is going on in the CFG.
We actually implement these rules using our parser functions below (just as we 
implement the ideas of Regular languages and such with our lexer - we dont explicitly 
need a class for the possible strings in the langauge defined by a Regex.)*)

```
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
```

(* The problem here is that if I want to define the expression "1" 
  (ie. the literal int)
  Then I have the construct this as 
Equalilty Comparison Term Factor Unary Literal 1 
While this is what we're actually doing in the derivation 
- this is quite tedious!? - but perhaps worth it!*)


Think is okay at the moment - should we be advancing when we have seen the semicolon/Eof - no I think we already have!




# Interpreting

We seem to run into a problem. A literal is a syntactic/textual representation of a value  - so '2' is a literal, while the actual value 2 is the number 2 that the computer works with. 

I suppose at our level - because we're using ocaml to do all the underlying computation and memory allocation for us - there is essentially no difference - so I can just replace the idea of a value by a literal and not worry. 

A problem we run across is that to 'evaluate a literal' - say had a function lit_to_val would have to allow for different types in the output. 

I don't think we need to worry about this low level here - again, ocaml is doing the actual evaluation for us and such.

Java apparently also has these problems- but he just uses the Object class to kinda cheat. We could do the same with OCaml? But seeing as we already have made the type Lit in the Token module - there is no need!

Moreover - its cleaner due to the pattern matching. Rather than using 'instanceof' or something of this vein - we can just pattern match the constructors for Token.Lit.l!

Super janky at the moment for evaluation of these expressions. 
