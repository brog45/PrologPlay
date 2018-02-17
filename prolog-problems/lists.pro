my_last(X, [X]).
my_last(X, [_|T]) :- my_last(X, T).

second_last(X, [X,_]).
second_last(X, [_|T]) :- second_last(X, T).

element_at(_, _, I) :- I < 1, !, fail.
element_at(_, [], _) :- !, fail.
element_at(_, L, I) :- length(L, LL), I > LL, !, fail.
element_at(X, [X|_], 1).
element_at(X, [_|T], I) :- J is I - 1, element_at(X, T, J).

elements_in(0,[]).
elements_in(X,[_|T]) :- elements_in(N,T), X is N + 1.

rev([], []) :- !.
rev([H|T],L) :- rev(T,T2), append([T2, [H]], L), !.
rev(L,[H|T]) :- rev(T,T2), append([T2, [H]], L).

palindrome(L) :- reverse(L, L).

my_flatten([], []).
my_flatten([H|T], L) :- is_list(H), my_flatten(H, H2), my_flatten(T, T2), lists:append(H2, T2, L).
my_flatten([H|T], [H|T2]) :-  not(is_list(H)), my_flatten(T, T2).

compress([], []).
compress([X], [X]).
compress([H,H|T], L) :- compress([H|T], L).
compress([X,Y|T], [X|L]) :- X \= Y, compress([Y|T], L).

% 1.09 (**) Pack consecutive duplicates of list elements into sublists. 
%   If a list contains repeated elements they should be placed in separate sublists.
% Example:
%   ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
wrap([],[]).
wrap([X|Xs],[[X]|Ys]) :- wrap(Xs,Ys).
condense([Xs],[Xs]) :- is_list(Xs).
condense([[X|Xs],[X]|Ys], Zs) :- condense([[X,X|Xs]|Ys], Zs).
condense([[X|Xs],[Y]|Ys], [[X|Xs]|Zs]) :- X \= Y, condense([[Y]|Ys],Zs).
pack([],[]).
pack(Xs,L) :- wrap(Xs,Ys), condense(Ys,L).

% 1.10 (*) Run-length encoding of a list.
%   Use the result of problem 1.09 to implement the so-called run-length 
%   encoding data compression method. Consecutive duplicates of elements are 
%   encoded as terms [N,E] where N is the number of duplicates of the element 
%   E.
% Example:
%   ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[4,a],[1,b],[2,c],[2,a],[1,d],[4,e]]
encode_item([X|Xs],[Nx,X]) :- length([X|Xs], Nx).
encode_packed([],[]).
encode_packed([X|Xs], [Y|Ys]) :- encode_item(X,Y), encode_packed(Xs,Ys).
encode([],[]).
encode([X|Xs],Zs) :- pack([X|Xs], Ys), encode_packed(Ys,Zs).

% 1.11 (*) Modified run-length encoding.
%   Modify the result of problem 1.10 in such a way that if an element has no
%   duplicates it is simply copied into the result list. Only elements with 
%   duplicates are transferred as [N,E] terms.
% Example:
%   ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[4,a],b,[2,c],[2,a],d,[4,e]]
simplify([],[]).
simplify([[1,X]|Xs], [X|Ys]) :- simplify(Xs,Ys).
simplify([[N,X]|Xs], [[N,X]|Ys]) :- N > 1, simplify(Xs,Ys).
encode_modified([], []).
encode_modified([X|Xs], Zs) :- encode([X|Xs], Ys), simplify(Ys,Zs).

% 1.12 (**) Decode a run-length encoded list.
%   Given a run-length code list generated as specified in problem 1.11. 
%   Construct its uncompressed version.
decode([], []).
decode([[1,X]|Xs], [X|Ys]) :- decode(Xs, Ys).
decode([[N,X]|Xs], [X|Ys]) :- N > 1, N2 is N - 1, decode([[N2,X]|Xs], Ys).
decode([X|Xs], [X|Ys]) :- \+ is_list(X), decode(Xs, Ys).

% 1.13 (**) Run-length encoding of a list (direct solution).
%   Implement the so-called run-length encoding data compression method 
%   directly. I.e. don't explicitly create the sublists containing the 
%   duplicates, as in problem 1.09, but only count them. As in problem 1.11, 
%   simplify the result list by replacing the singleton terms [1,X] by X.
% Example:
%   ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_direct([],[]).
encode_direct([X],[X]) :- \+ is_list(X).
encode_direct([X,X|Xs],L) :- \+ is_list(X), encode_direct([[2,X]|Xs],L).
encode_direct([X,Y|Xs],[X|Zs]) :- X \= Y, \+ is_list(X), \+ is_list(Y), encode_direct([Y|Xs],Zs).
encode_direct([[N,X]],[[N,X]]) :- N > 0.
encode_direct([[N,X],X|Xs],L) :- N2 is N + 1, encode_direct([[N2,X]|Xs],L).
encode_direct([[N,X],Y|Xs],[[N,X]|L]) :- X \= Y, encode_direct([Y|Xs],L).

% 1.14 (*) Duplicate the elements of a list.
% Example:
%     ?- dupli([a,b,c,c,d],X).
%     X = [a,a,b,b,c,c,c,c,d,d]
dupli([],[]).
dupli([X|Xs],[X,X|Ys]) :- dupli(Xs,Ys).

% 1.15 (**) Duplicate the elements of a list a given number of times.
% Example:
%     ?- dupli([a,b,c],3,X).
%     X = [a,a,a,b,b,b,c,c,c]
dupli([X],1,[X]).
dupli([X],N,[X|Xs]) :- N > 1, N2 is N - 1, dupli([X],N2,Xs).
dupli([X,Y|Ys], N, L) :- dupli([X], N, Xs), dupli([Y|Ys], N, Zs), append(Xs, Zs, L).

% 1.16 (**) Drop every N'th element from a list.
% Example:
%     ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
%     X = [a,b,d,e,g,h,k]
drop([],_,[]).
drop([_|_],1,[]).
drop([X],N,[X]) :- N > 1.
drop([X1,X2|Xs], N, L2) :- N > 1, drop([X1,X2|Xs], N, L2, N).
drop([_|Xs],1,Ys,N) :- drop(Xs,N,Ys).
drop([X|Xs],Nx,[X|Ys],N) :- Nx > 1, Ny is Nx - 1, drop(Xs,Ny,Ys,N).

% 1.17 (*) Split a list into two parts; the length of the first part is given.
%     Do not use any predefined predicates.
% Example:
%     ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
%     L1 = [a,b,c]
%     L2 = [d,e,f,g,h,i,k]
split([X|Xs],1,[X],Xs).
split([X|Xs],N,[X|Ys],Zs) :- N > 1, N2 is N - 1, split(Xs, N2, Ys, Zs).

% 1.18 (**) Extract a slice from a list.
%   Given two indices, I and K, the slice is the list containing the elements
%   between the I'th and K'th element of the original list (both limits 
%   included). Start counting the elements with 1.
% Example:
%   ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
%   L = [c,d,e,f,g]
slice(Xs,1,K,Ys) :- split(Xs,K,Ys,_).
slice([_|Xs],I,K,Ys) :- I > 1, I2 is I - 1, K2 is K - 1, slice(Xs,I2,K2,Ys).

% 1.19 (**) Rotate a list N places to the left.
% Examples:
%     ?- rotate([a,b,c,d,e,f,g,h],3,X).
%     X = [d,e,f,g,h,a,b,c]
%
%     ?- rotate([a,b,c,d,e,f,g,h],-2,X).
%     X = [g,h,a,b,c,d,e,f]
%
%     Hint: Use the predefined predicates length/2 and append/3, as well as the result of problem 1.17.
rotate(L,0,L).
rotate(L1,N,L2) :- N < 0, length(L1, Length), N2 is Length - (-N mod Length), rotate(L1,N2,L2).
rotate(L1,N,L2) :- N > 0, length(L1, Length), N >= Length, N2 is N mod Length, rotate(L1,N2,L2).
rotate(L1,N,L4) :- N > 0, length(L1, Length), N < Length, split(L1,N,L2,L3), append(L3,L2,L4).
