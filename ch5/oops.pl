:- module(oops,[main/0, run/1]).

:- dynamic initial_data/1, fact/2, rule/1, instantiation/1.

% load oops operator definitions
:- consult(oops_ops).

main :- welcome, supervisor.

run(F) :- reconsult(F), initialize, go.

welcome :- 
    write("OOPS - a toy production system"), nl, nl,
    write("The => prompt accepts three commands:"), nl,nl,
    write("    load. - prompts for name of rules file"), nl,
    write("    exit. - does what you'd expect"), nl,
    write("    go.   - starts the inference"), nl, nl.

% the supervisor uses a repeat fail loop to read and process commands from the user

supervisor :-
    repeat,
    writeln("=>"),
    read(X),
    doit(X),
    X = exit.

doit(X) :- do(X).

% actions to take based on commands

do(exit) :- !.
do(go) :- initialize, go, !.
do(load) :- load, !.
do(list) :- lst, !. % list all of working storage
do(list(X)) :- lst(X), !. % lists match the pattern
do(_) :- write("invalid command"), nl.

% loads the rules (Prolog terms) into the Prolog database

load :-
    writeln('File name? '),
    read(F),
    reconsult(F).

% assert each of the initial conditions into working storage

initialize :- 
    initial_data(X),
    assert_list(X).

% working storage is represented by database terms stored under the key "fact"

assert_list([]) :- !.
assert_list([H|T]) :- 
    assertz(fact(H, 0)),
    !, assert_list(T).

/* the main inference loop, find a rule and try it.
   if it fired, say so and repeat the process.
   if no, go back and try the next rule.
   when no rules succeed, stop the inference.
   */

go :- 
    conflict_set(CS),
    select_rule(CS, r(Inst, ID, LHS, RHS)),
    process(RHS, LHS),
    asserta( instantiation(Inst) ),
    write("Rule fired "), write(ID), nl,
    !, go.
go.

conflict_set(ConflictSet) :-
    findall(r(Inst, ID, LHS, RHS),
            conflict(Inst, ID, LHS, RHS),
            ConflictSet).

conflict(Inst, ID, LHS, RHS) :-
    rule ID: LHS ==> RHS, 
    match(LHS, Inst).

select_rule(CS, R) :-
    refract(CS, CS1),
    lex_sort(CS1, [R|_]).

lex_sort(L, L1) :-
    build_keys(L, LK),
    keysort(LK, X),
    reverse(X,Y),
    strip_keys(Y, L1).

build_keys([],[]).
build_keys([r(Inst,A,B,C)|T],[Key-r(Inst,A,B,C)|TK]) :-
    build_chlist(Inst,ChronList),
    sort(ChronList, X),
    reverse(X, Key),
    build_keys(T,TK).

build_chlist([],[]).
build_chlist([_/Chron|T], [Chron|TC]) :-
    build_chlist(T,TC).

strip_keys([],[]).
strip_keys([_-X|Y],[X|Z]) :-
    strip_keys(Y,Z).

refract([], []).
refract([r(Instance,_,_,_)|T], TR) :-
    instantiation(Instance),
    !, refract(T, TR).
refract([H|T], [H|TR]) :-
    refract(T, TR).

% recursively go through the LHS list, matching conditions against working storage

match([], []) :- !.
match([_:Premise|Rest], [Premise/Time|IRest]) :-
    !,
    (fact(Premise, Time); 
     test(Premise), Time = 0), % comparison, rather than fact
    match(Rest, IRest).
match([Premise|Rest], [Premise/Time|IRest]) :-
    % condition number not specified
    !, 
    (fact(Premise, Time); 
     test(Premise), Time = 0),
    match(Rest, IRest).

% various tests allowed on the LHS

test(not(X)) :- !, \+ fact(X, _Chron).
test(X#Y)    :- !, X=Y.
test(X>Y)    :- !, X>Y.
test(X>=Y)   :- !, X>=Y.
test(X<Y)    :- !, X<Y.
test(X=<Y)   :- !, X=<Y.
test(X=Y)    :- !, X is Y.
test(member(X,Y)) :- !, member(X,Y).

% recurively execute each action in the RHS

process([],_) :- !.
process([Action|Rest], LHS) :- 
    take(Action, LHS),
    !, process(Rest, LHS).

% If it's retract, use the reference number stored in the LRefs list; 
% otherwise, just take the action.

getchron(N) :-
    ( retract(chron(N)) ; N = 1 ),
    NN is N + 1,
    asserta(chron(NN)).

assert_ws(fact(X, T)) :-
    getchron(T),
    asserta(fact(X, T)).

take(retract(N), LHS) :-
    (N == all; integer(N)),
    !, retr(N, LHS).
take(A, _) :- take(A), !.

take(retract(X)) :- !, retract(fact(X, _)).
take(assert(X)) :- 
    !, assert_ws(fact(X, _)),
    write(adding-X), nl.
take(X#Y) :- !, X=Y.
take(X=Y) :- !, X is Y.
take(write(X)) :- !, write(X).
take(nl) :- !, nl.
take(read(X)) :- !, read(X).
take(prompt(X,Y)) :- !, nl, write(X), read(Y).
take(cls) :- !, tty_clear.
take(member(X,Y)) :- !, member(X, Y).
take(list(X)) :- !, lst(X).

% logic for retraction

retr(all, LHS) :- !, retrall(LHS).
retr(N, []) :- !, format("retract error, no~w~n", [N]).
retr(N, [N:Premise|_]) :- !, retract(fact(Premise, _)).
retr(N, [_|Rest]) :- !, retr(N,Rest).

retrall([]).
retrall([_:Premise|Rest]) :- 
    retract(fact(Premise, _)),
    !, retrall(Rest).
retrall([Premise|Rest]) :- 
    retract(fact(Premise, _)),
    !, retrall(Rest).
retrall([_|Rest]) :-
    !, retrall(Rest).

% list all terms in working storage

lst :-
    fact(X, _Chron),
    write(X), nl,
    fail.
lst.

% list all terms that match the pattern

lst(X) :-
    fact(X, _Chron),
    write(X), nl, 
    fail.
lst(_).
