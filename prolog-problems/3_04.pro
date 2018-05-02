% 3.04 (**) Gray code.
%   An n-bit Gray code is a sequence of n-bit strings constructed according 
%   to certain rules. For example,
%   n = 1: C(1) = ['0','1'].
%   n = 2: C(2) = ['00','01','11','10'].
%   n = 3: C(3) = ['000','001','011','010','110','111','101','100'].
%
%   Find out the construction rules and write a predicate with the following 
%   specification:
%
%   % gray(N,C) :- C is the N-bit Gray code
%
%   Can you apply the method of "result caching" in order to make the 
%   predicate more efficient, when it is to be used repeatedly? 
:- module('3_04',[gray/2]).

dec_bin(0,'0') :- !.
dec_bin(1,'1') :- !.
dec_bin(N, B) :- 
    N > 1, 
    X is N mod 2,
    Y is N // 2,
    dec_bin(Y, B1),
    atom_concat(B1, X, B).

atom_padleft(A, N, A) :-
    atom_length(A, Length),
    Length >= N, !.
atom_padleft(A, N, B) :-
    atom_concat('0', A, A1),
    atom_padleft(A1, N, B).

int_to_gray(N, G) :-
    Y is N // 2, 
    Z is N xor Y,
    dec_bin(Z, G).
int_to_gray(N, Length, G) :-
    int_to_gray(N, G1),
    atom_padleft(G1, Length, G).

in_range(Min, Max, _) :- 
    Min > Max, !, fail.
in_range(Min, _, Min).
in_range(Min, Max, N) :- 
    succ(Min, Next),
    in_range(Next, Max, N).

each_gray(Power, G) :-
    Max is (2 ** Power) - 1,
    in_range(0, Max, N), 
    int_to_gray(N, Power, G).

gray(N, L) :-
    findall(G, each_gray(N,G), L).
