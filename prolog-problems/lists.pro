my_last(X, [X]).
my_last(X, [_|T]) :- my_last(X, T).

second_last(X, [X,_]).
second_last(X, [_|T]) :- second_last(X, T).

element_at(_, _, I) :- I < 1, !, fail.
element_at(_, [], _) :- !, fail.
element_at(_, L, I) :- length(L, LL), I > LL, !, fail.
element_at(X, [X|_], 1).
element_at(X, [_|T], I) :- J is I - 1, element_at(X, T, J).

elements_in(0,[]).
elements_in(X,[_|T]) :- elements_in(N,T), X is N + 1.

rev([], []) :- !.
rev([H|T],L) :- rev(T,T2), append([T2, [H]], L), !.
rev(L,[H|T]) :- rev(T,T2), append([T2, [H]], L).

palindrome(L) :- reverse(L, L).

my_flatten([], []).
my_flatten([H|T], L) :- is_list(H), my_flatten(H, H2), my_flatten(T, T2), lists:append(H2, T2, L).
my_flatten([H|T], [H|T2]) :-  not(is_list(H)), my_flatten(T, T2).

compress([], []).
compress([X], [X]).
compress([H,H|T], L) :- compress([H|T], L).
compress([X,Y|T], [X|L]) :- X \= Y, compress([Y|T], L).

% 1.09 (**) Pack consecutive duplicates of list elements into sublists. 
%   If a list contains repeated elements they should be placed in separate sublists.
% Example:
%   ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
wrap([],[]).
wrap([X|Xs],[[X]|Ys]) :- wrap(Xs,Ys).
condense([Xs],[Xs]) :- is_list(Xs).
condense([[X|Xs],[X]|Ys], Zs) :- condense([[X,X|Xs]|Ys], Zs).
condense([[X|Xs],[Y]|Ys], [[X|Xs]|Zs]) :- X \= Y, condense([[Y]|Ys],Zs).
pack([],[]).
pack(Xs,L) :- wrap(Xs,Ys), condense(Ys,L).
