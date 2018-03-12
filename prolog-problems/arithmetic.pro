% 2.01 (**) Determine whether a given integer number is prime.
%     Example:
%     ?- is_prime(7).
%     Yes
in_range(From, To, _) :- From > To, !, fail.
in_range(From, To, From) :- From =< To.
in_range(From, To, X) :-
    Next is From + 1,
    in_range(Next, To, X).
is_composite(N) :-
    S is sqrt(N),
    in_range(2, S, Factor),
    Remainder is N mod Factor,
    Remainder = 0,
    !.
is_prime(N) :-
    integer(N),
    N > 1,
    \+ is_composite(N).

% 2.02 (**) Determine the prime factors of a given positive integer.
%     Construct a flat list containing the prime factors in ascending order.
%     Example:
%     ?- prime_factors(315, L).
%     L = [3,3,5,7]
prime_factors(N, [N]) :- is_prime(N).
prime_factors(N, [F|Fs]) :-
    integer(N),
    N > 0,
    S is sqrt(N),
    in_range(2, S, F),
    is_prime(F),
    Q is N / F,
    integer(Q),
    !,
    prime_factors(Q, Fs).
