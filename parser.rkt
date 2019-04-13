#lang brag

%qkstack  : (%operator | %form)*
%operator : %datum | %word | %if | %begin | %let-cc
%word     : ID
%datum    : NUMBER | TRUE | FALSE | STRING
%if       : LP "if" %operator %operator? RP
%begin    : LP "begin" %operator* RP
%let-cc   : LP "let/cc" ID %operator* RP
%form     : %require | %define
%require  : LP "require" (ID | STRING)+ RP
%define   : LP "define" ID %comment? %operator+ RP
%comment  : LB (ID | %datum | RP | LP)* RB
