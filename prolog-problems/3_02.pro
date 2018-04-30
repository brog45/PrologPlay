% 3.02 (*) Truth tables for logical expressions (2).
%     Continue problem 3.01 by defining and/2, or/2, etc as being operators.
%     This allows to write the logical expression in the more natural way, as
%     in the example: A and (A or not B). Define operator precedence as usual;
%     i.e. as in Java.
%
%     Example:
%     ?- table(A,B, A and (A or not B)).
%     true true true
%     true fail true
%     fail true fail
%     fail fail fail
:- ensure_loaded('3_01.pro').

:- op(900, fy, not).
:- op(910, yfx, nand).
:- op(910, yfx, and).
:- op(920, yfx, nor).
:- op(920, yfx, or).
:- op(930, yfx, equ).
:- op(930, yfx, impl).
:- op(930, yfx, xor).
