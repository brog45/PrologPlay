% 3.03 (**) Truth tables for logical expressions (3).
%   Generalize problem 3.02 in such a way that the logical expression may
%   contain any number of logical variables. Define table/2 in a way that
%   table(List,Expr) prints the truth table for the expression Expr, which
%   contains the logical variables enumerated in List.
%
%   Example:
%   ?- table([A,B,C], A and (B or C) equ A and B or A and C).
%   true true true true
%   true true fail true
%   true fail true true
%   true fail fail true
%   fail true true true
%   fail true fail true
%   fail fail true true
%   fail fail fail true
:- ensure_loaded('3_02.pro').

bindlist([]).
bindlist([X|Xs]) :- bind(X), bindlist(Xs).

writelist([]).
writelist([X|Xs]) :- write(X), write("\t"), writelist(Xs).

table([X|Xs], Expr) :- bindlist([X|Xs]), do([X|Xs], Expr), fail.
table(_, _).

do([X|Xs], _) :- writelist([X|Xs]), fail.
do(_, Expr) :- Expr, !, writeln(true).
do(_, _) :- writeln(false).
