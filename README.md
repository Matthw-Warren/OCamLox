# OCamLox
Implementing a tree walk interpreter following https://craftinginterpreters.com.


## Lexer
Current things to fix
- Accepts floats of the form  NUM.NUM.NUm. etc - need to limit to <=1 decimal point
- No multiline comments
- There was another thing that I cant remember at the moment.
- Nesting strings...  -we need a stack - is this regex? - I suppose that we can just double check that a \ is prepended!