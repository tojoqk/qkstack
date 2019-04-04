#lang brag

%%qkstack    : %%expression*
%%expression : (%%form | %%word | %%datum | %%block)
%%form       : "(" (%%if | %%define | %%require | %%provide | %%let | %%named-let | %%let-cc) ")"
%%if         : "if" %%block %%block?
%%define     : "define" IDENTIFIER %%block
%%let        : "let" "(" %%binding* ")" %%block
%%named-let  : "let" IDENTIFIER "(" %%binding* ")" %%block
%%binding    : "(" IDENTIFIER %%block ")"
%%let-cc      : "let/cc" IDENTIFIER %%block
%%require    : "require" (IDENTIFIER | STRING)+
%%provide    : "provide" (IDENTIFIER | STRING)+
%%block      : "[" %%expression* "]"
%%word       : IDENTIFIER
%%datum      : STRING | NUMBER | TRUE | FALSE | "'" %%sexp
%%sexp       : "'" %%sexp | STRING | NUMBER | IDENTIFIER | "(" %%sexp* ")"
