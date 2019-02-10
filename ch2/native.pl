:- module(native,[go/0,menuask/3,ask/2]).
:- use_module(library(dialect/sicstus)).

% :- dynamic multivalued/1.
:- dynamic top_goal/1, known/3.

%!	go
%
%   Run the Native shell's command loop.
go :-
    greeting,
    repeat, 
    read_line(Codes),
    atom_codes(X, Codes),
    do(X),
    X == quit.

greeting :-
    writeln('This is the Native Prolog shell.'),
    writeln('Enter load, consult, or quit at the prompt.').

do(load) :- load_kb, !.
do(consult) :- solve, !.
do(quit).
do(X) :-
    format('~w is not a legal command.~n', [X]),
    fail.

load_kb :- 
    writeln('Enter filename:'),
    read_line(Codes),
    atom_codes(F, Codes),
    consult(F),
    format('Loaded ~w~n', [F]).

solve :-
    retractall(known(_,_,_)),
    top_goal(X),
    format('The answer is ~w.~n', [X]).
solve :-
    writeln('No answer found.').

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
    read_line(Codes),
    atom_codes(Y, Codes),
    asserta(known(Y, A, V)),
    !, Y == yes.

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
