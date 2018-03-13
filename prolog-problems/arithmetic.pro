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

% 2.03 (**) Determine the prime factors of a given positive integer (2).
%     Construct a list containing the prime factors and their multiplicity.
%     Example:
%     ?- prime_factors_mult(315, L).
%     L = [[3,2],[5,1],[7,1]]
%
%     Hint: The solution of problem 1.10 may be helpful.
:- ensure_loaded('lists.pro').
flip_encoded([],[]).
flip_encoded([[A,B]|Xs], [[B,A]|Ys]) :- flip_encoded(Xs, Ys).
prime_factors_mult(N, L) :-
    prime_factors(N, Fs),
    encode(Fs, Gs),
    flip_encoded(Gs, L).

% 2.04 (*) A list of prime numbers.
%     Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
prime_in_range(From, To, N) :-
    in_range(From, To, N),
    is_prime(N).
primes_in_range(From, To, L) :-
    findall(N, prime_in_range(From, To, N), L).

% 2.05 (**) Goldbach's conjecture.
%     Goldbach's conjecture says that every positive even number greater than 2
%     is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the
%     most famous facts in number theory that has not been proved to be correct
%     in the general case. It has been numerically confirmed up to very large
%     numbers (much larger than we can go with our Prolog system). Write a
%     predicate to find the two prime numbers that sum up to a given even
%     integer.
%
%     Example:
%     ?- goldbach(28, L).
%     L = [5,23]
is_even(N) :- integer(N), R is N mod 2, R = 0.
goldbach(N, [A,B]) :-
    is_even(N), 
    N > 2,
    Half is N / 2,
    in_range(2, Half, A),
    is_prime(A),
    B is N - A,
    is_prime(B).
