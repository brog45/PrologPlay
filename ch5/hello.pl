% hello.pl: a trivial OOPS example for simple testing
% This trivial script simply prints "Hello, Prolog!"

% load oops operator definitions
:- consult(oops_ops).

% initial_data/1 will be used to initialize working storage
initial_data([
        hello
    ]).

rule 1#
    [ 1# hello ]
    ==>
    [ write("Hello, Prolog!")
    , nl
    , retract(1) % removes hello from working storage
    ].
