# OCamLox
Implementing a tree walk interpreter following https://craftinginterpreters.com.


## Lexer
Current things to fix
- Accepts floats of the form  NUM.NUM.NUm. etc - need to limit to <=1 decimal point
- No multiline comments
- There was another thing that I cant remember at the moment.
- Nesting strings...  -we need a stack - is this regex? - I suppose that we can just double check that a \ is prepended!
- ALso - some things like the input 3+2 should be lexed as 3,+,2, but currently throws an error because we rely on whitespace and line ends etc to end a number. This must change. Also with a variable name we would want this, but not with some keywords - this makes thing a bit complex - so for now we shall just require the whitespace!

## Parser 

-Add in comma operators for expression blocks



## Interpreter

