:- module(native,[menuask/3,ask/2]).

% :- dynamic multivalued/1.
:- dynamic known/3.

%!	ask(+Attribute, +Value)
%
%
% ask(A, V):-
%     \+ multivalued(A),
%     known(yes, A, V2),
%     V \== V2,
%     !, fail.
ask(A, V):-
    known(X, A, V),
    !, X = yes.
ask(A, V):-
    format('~w ~w? ', [A, V]), nl,
    read(Y),
    asserta(known(Y, A, V)),
    !, Y = yes.

%!	menuask(+Attribute, +Value, +MenuList)
%
%
menuask(A, V, MenuList):-
    write('What is the value for'),
    write(A), write('?'), nl,
    write(MenuList), nl,
    read(X),
    check_val(X, A, V, MenuList).

check_val(X, A, V, MenuList):-
    member(X, MenuList), !,
    asserta( known(yes, A, V) ),
    X == V.
check_val(X, A, V, MenuList):-
    write(X), write('is not a legal value. Try again.'), nl,
    menuask(A, V, MenuList).
