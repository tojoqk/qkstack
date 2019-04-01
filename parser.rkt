#lang brag

%%qkstack      : %%block*
%%block        : %%if-block | %%define-block | (%%operator | %%datum)+
%%if-block     : IF-BEGIN %%block* (END | IF-ELSE %%block* END)
%%define-block : DEFINE-BEGIN IDENTIFIER BEGIN %%block* END
%%operator     : IDENTIFIER
%%datum        : STRING | NUMBER | QUOTE %%sexp
%%sexp         : QUOTE %%sexp | STRING | NUMBER | IDENTIFIER | LEFT-PARENCE %%sexp RIGHT-PARENCE
