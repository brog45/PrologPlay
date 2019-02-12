:- module(oops,[main/0, run/1]).

:- dynamic initial_data/1, fact/1, rule/1.

% operator definitions

:- op(800,xfx,==>). % used to separate LHS and RHS
:- op(500,xfy,#).   % used to separate attributes and values
:- op(810,fx,rule). % used to define rules
:- op(700,xfy,#).   % used for unification instead of =

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
    assertz(fact(H)),
    !, assert_list(T).

/* the main inference loop, find a rule and try it.
   if it fired, say so and repeat the process.
   if no, go back and try the next rule.
   when no rules succeed, stop the inference.
   */

go :- 
    call(rule ID # LHS ==> RHS),
    try(LHS, RHS),
    write("Rule fired "), write(ID), nl,
    !, go.
go.

try(LHS, RHS) :-
    match(LHS),
    process(RHS, LHS).

% recursively go through the LHS list, matching conditions against working storage

match([]) :- !.
match([_#Premise|Rest]) :-
    !,
    (fact(Premise); 
     test(Premise)), % comparison, rather than fact
    match(Rest).
match([Premise|Rest]) :-
    % condition number not specified
    !, 
    (fact(Premise); 
     test(Premise)),
    match(Rest).

% various tests allowed on the LHS

test(not(X)) :- !, \+ fact(X).
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

take(retract(N), LHS) :-
    (N == all; integer(N)),
    !, retr(N, LHS).
take(A, _) :- take(A), !.

take(retract(X)) :- !, retract(fact(X)).
take(assert(X)) :- 
    !, asserta(fact(X)),
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
retr(N, [N#Premise|_]) :- !, retract(fact(Premise)).
retr(N, [_|Rest]) :- !, retr(N,Rest).

retrall([]).
retrall([_#Premise|Rest]) :- 
    retract(fact(Premise)),
    !, retrall(Rest).
retrall([Premise|Rest]) :- 
    retract(fact(Premise)),
    !, retrall(Rest).
retrall([_|Rest]) :-
    !, retrall(Rest).

% list all terms in working storage

lst :-
    fact(X),
    write(X), nl,
    fail.
lst.

% list all terms that match the pattern

lst(X) :-
    fact(X),
    write(X), nl, 
    fail.
lst(_).

