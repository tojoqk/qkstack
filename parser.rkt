#lang brag

%%qkstack        : (%%top-level-form | %%operator)*
%%top-level-form : %%define | %%require | %%provide
%%define         : "(" "define" IDENTIFIER %%operator* ")"
%%require        : "(" "require" (IDENTIFIER | STRING)+ ")"
%%provide        : "(" "provide" IDENTIFIER+ ")"
%%operator       : %%if | %%let | %%named-let | %%let-cc | %%begin
                 | %%quote | %%word | %%datum
%%if             : "(" "if" %%operator %%operator? ")"
%%then           : "(" "then" %%operator* ")"
%%else           : "(" "else" %%operator* ")"
%%when           : "(" "when" %%operator* ")"
%%let            : "(" "let" %%bindings %%operator* ")"
%%named-let      : "(" "let" IDENTIFIER %%bindings %%operator* ")"
%%bindings       : "(" IDENTIFIER* ")"
%%let-cc         : "(" "let/cc" IDENTIFIER %%operator* ")"
%%begin          : "(" "begin" %%operator* ")"
%%quote          : "," %%operator
%%word           : IDENTIFIER
%%datum          : STRING | NUMBER | TRUE | FALSE | "'" %%sexp
%%sexp           : STRING | NUMBER | IDENTIFIER | TRUE | FALSE
                 | "(" %%sexp* ")"
