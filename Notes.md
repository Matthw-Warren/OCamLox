# Lexer Notes

in_channel in ocaml is an abstract type (ie. we dont know the concrete implementation) - handles inputs.

I think the basic implementation is having something like an mutable variable : int which stores where in the file (or more generally - kinda the stream) we are. 
    This can be accessed using pos_in, and modified using seek_in

input_line, input_char, and so on all do what one would expect.

(read_line and such are functions which are called kinda through the standard input stdin)

If I set some variable a = stdin, this has type in_channel - but when initialised it has no content. I assume that the underlying types for stdin and if I construct an in_channel using, say, open_in (so reading from a file) are different, because when something like input_char is called, with stdin we are prompted to write this!


Yes - the stdin is special - has some associated special functions cf. https://ocaml.org/manual/5.2/api/Stdlib.html#2_Inputfunctionsonstandardinput



out_channels have a Buffer - eg. If I'm trying to write to a file (or command line) then the buffer is a store of what is going to be written. 