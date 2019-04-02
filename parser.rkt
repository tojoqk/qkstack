#lang brag

%%qkstack    : %%expression*
%%expression : (%%form | %%word | %%datum | %%block)
%%form       : "(" (%%if | %%define | %%require | %%provide) ")"
%%if         : "if" %%block %%block?
%%define     : "define" IDENTIFIER %%block
%%require    : "require" %%sexp+
%%provide    : "provide" %%sexp+
%%block      : "[" %%expression* "]"
%%word       : IDENTIFIER
%%datum      : STRING | NUMBER | TRUE | FALSE | "'" %%sexp
%%sexp       : "'" %%sexp | STRING | NUMBER | IDENTIFIER | "(" %%sexp* ")"
