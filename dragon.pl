% [Describing a Dragon Curve](https://youtu.be/DMdiPC1ZckI)

dragon([r]).
dragon(Ds) :-
    dragon(Ds0),
    phrase((seq(Ds0), [r], inv(Ds0)), Ds).

inv([]) --> [].
inv([F|Fs]) --> inv(Fs), swap(F).

swap(r) --> [l].
swap(l) --> [r].

%% seq(Seq)//
% 
% Describes a sequence
seq(Xs, Cs0,Cs) :-
    var(Xs),
    Cs0 == [],
    !,
    Xs = [],
    Cs0 = Cs.
 seq([]) --> [].
 seq([E|Es]) --> [E], seq(Es).
