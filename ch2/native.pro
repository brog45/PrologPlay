define(multivalued, 1).
define(known, 3).

ask(A, V):-
    % not multivalued(A),
    known(yes, A, V2),
    V \== V2,
    !, fail.
ask(A, V):-
    known(yes, A, V),
    !.
ask(A, V):-
    known(_, A, V),
    !, fail.
ask(A, V):-
    write(A:V),
    write('? : '),
    read(Y),
    asserta(known(Y, A, V)),
    Y == yes.

menuask(A, V, MenuList):-
    write('What is the value for'),
    write(A), write('?'), nl,
    write(MenuList), nl,
    read(X),
    check_val(X, A, V, MenuList),
    asserta( known(yes, A, V) ),
    X == V.

check_val(X, A, V, MenuList):-
    member(X, MenuList), !.
check_val(X, A, V, MenuList):-
    write(X), write('is not a legal value. Try again.'), nl,
    menuask(A, V, MenuList).
