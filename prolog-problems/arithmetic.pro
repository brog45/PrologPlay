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
    is_prime(B),
    !.

% 2.06 (**) A list of Goldbach compositions.
%     Given a range of integers by its lower and upper limit, print a list of
%     all even numbers and their Goldbach composition.
%
%     Example:
%     ?- goldbach_list(9,20).
%     10 = 3 + 7
%     12 = 5 + 7
%     14 = 3 + 11
%     16 = 3 + 13
%     18 = 5 + 13
%     20 = 3 + 17
%
%     In most cases, if an even number is written as the sum of two prime
%     numbers, one of them is very small. Very rarely, the primes are both
%     bigger than say 50. Try to find out how many such cases there are in the
%     range 2..3000.
%
%     Example (for a print limit of 50):
%     ?- goldbach_list(1,2000,50).
%     992 = 73 + 919
%     1382 = 61 + 1321
%     1856 = 67 + 1789
%     1928 = 61 + 1867
goldbach_list(From, To, Minimum) :-
    in_range(From,To,N), 
    is_even(N), 
    goldbach(N, [A,B]),
    A >= Minimum,
    B >= Minimum,
    writeln(N = A + B),
    fail.
goldbach_list(_,_,_).
goldbach_list(From, To) :- 
    goldbach_list(From, To, 2).

% 2.07 (**) Determine the greatest common divisor of two positive integer numbers.
%     Use Euclid's algorithm.
%     Example:
%     ?- gcd(36, 63, G).
%     G = 9
%
%     Define gcd as an arithmetic function; so you can use it like this:
%     ?- G is gcd(36,63).
%     G = 9
gcd(A, 0, A) :- !.
gcd(A, B, Y) :- B \= 0, X is A mod B, gcd(B, X, Y).
gcd(A, B) := G :- gcd(A, B, G).

% 2.08 (*) Determine whether two positive integer numbers are coprime.
%     Two numbers are coprime if their greatest common divisor equals 1.
%     Example:
%     ?- coprime(35, 64).
%     Yes
coprime(A, B) :- gcd(A, B, 1).

% 2.09 (**) Calculate Euler's totient function phi(m).  Euler's so-called totient
%     function phi(m) is defined as the number of positive integers r (1 <= r < m)
%     that are coprime to m.
%
%     Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
%
%     ?- Phi is totient_phi(10).
%     Phi = 4
%
%     Find out what the value of p, Rhi(m) is if m is a prime number. Euler's
%     totient function plays an important role in one of the most widely used
%     public key cryptography methods (RSA). In this exercise you should use the
%     most primitive method to calculate this function. There is a smarter way
%     that we shall use in 2.10.
in_r_range(From, To, _) :- From >= To, !, fail.
in_r_range(From, _, From).
in_r_range(From, To, X) :-
    Next is From + 1,
    in_range(Next, To, X).
r(M, R) :-
    in_r_range(1, M, R),
    coprime(M, R).
totient_phi(1, 1) :- !.
totient_phi(M, N) :-
    findall(R, r(M, R), Rs),
    length(Rs, N).

% 2.10 (**) Calculate Euler's totient function phi(m) (2).
%     See problem 2.09 for the definition of Euler's totient function. If the
%     list of the prime factors of a number m is known in the form of problem
%     2.03 then the function phi(m) can be efficiently calculated as follows:
%     Let [[p1,m1],[p2,m2],[p3,m3],...] be the list of prime factors (and their
%     multiplicities) of a given number m. Then phi(m) can be calculated with
%     the following formula:
% 
%     phi(m) = (p1 - 1) * p1**(m1 - 1) * (p2 - 1) * p2**(m2 - 1) * (p3 - 1) * p3**(m3 - 1) * ...
% 
%     Note that a**b stands for the b'th power of a.
%
totient_phi2_loop(Accumulator, [[P|M]], N) :-
    !, N is Accumulator * (P - 1) * (P ** (M - 1)).
totient_phi2_loop(Accumulator, [[P|M]|Tail], N) :-
    !, Next is Accumulator * (P - 1) * (P ** (M - 1)),
    totient_phi2_loop(Next, Tail, N).
totient_phi2(1, 1) :- !.
totient_phi2(M, N) :-
    prime_factors_mult(M, L),
    !, totient_phi2_loop(1, L, N).

% 2.11 (*) Compare the two methods of calculating Euler's totient function.
%     Use the solutions of problems 2.09 and 2.10 to compare the algorithms.
%     Take the number of logical inferences as a measure for efficiency. Try
%     to calculate phi(10090) as an example.
compare_totients :-
    time(totient_phi(10090, Phi)),
    write("totient_phi(10090, Phi) -> Phi = "), write(Phi), nl,
    time(totient_phi2(10090, Phi2)),
    write("totient_phi2(10090, Phi2) -> Phi2 = "), write(Phi2), nl.
