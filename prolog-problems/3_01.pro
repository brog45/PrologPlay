% 3.01 (**) Truth tables for logical expressions.
%     Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
%     (for logical equivalence) which succeed or fail according to the result
%     of their respective operations; e.g. and(A,B) will succeed, if and only
%     if both A and B succeed. Note that A and B can be Prolog goals (not only
%     the constants true and fail).
%
%     A logical expression in two variables can then be written in prefix
%     notation, as in the following example: and(or(A,B),nand(A,B)).
%
%     Now, write a predicate table/3 which prints the truth table of a given
%     logical expression in two variables.
%
%     Example:
%     ?- table(A,B,and(A,or(A,B))).
%     true true true
%     true fail true
%     fail true fail
%     fail fail fail
and(A,B) :- A, B.
or(A,B) :- A; B.
nand(A,B) :- not(and(A,B)).
nor(A,B) :- not(or(A,B)).
xor(A,B) :- A, \+ B; B, \+ A.
impl(A,B) :- not(A); B.
equ(A,B) :- and(A,B); nor(A,B).
bind(false).
bind(true).
table(A,B,Expr) :- bind(A), bind(B), do(A,B,Expr), fail.
table(_,_,_).
do(A,B,_) :- write(A), write("\t"), write(B), write("\t"), fail.
do(_,_,Expr) :- Expr, !, write(true), nl.
do(_,_,_) :- write(false), nl.
