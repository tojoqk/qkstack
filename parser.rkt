#lang brag

%%qkstack        : (%%top-level-form | %%expression)*
%%top-level-form : %%define | %%require | %%provide
%%define         : "(" "define" IDENTIFIER %%expression* ")"
%%require        : "(" "require" (IDENTIFIER | STRING)+ ")"
%%provide        : "(" "provide" IDENTIFIER+ ")"
%%expression     : %%if | %%let | %%named-let | %%let-cc | %%begin
                 | %%quote | %%word | %%datum
%%if             : "(" "if" %%expression %%expression? ")"
%%then           : "(" "then" %%expression* ")"
%%else           : "(" "else" %%expression* ")"
%%when           : "(" "when" %%expression* ")"
%%let            : "(" "let" %%bindings %%expression* ")"
%%named-let      : "(" "let" IDENTIFIER %%bindings %%expression* ")"
%%bindings       : "(" IDENTIFIER* ")"
%%let-cc         : "(" "let/cc" IDENTIFIER %%expression* ")"
%%begin          : "(" "begin" %%expression* ")"
%%quote          : "," %%expression
%%word           : IDENTIFIER
%%datum          : STRING | NUMBER | TRUE | FALSE | "'" %%sexp
%%sexp           : STRING | NUMBER | IDENTIFIER | TRUE | FALSE
                 | "(" %%sexp* ")"
