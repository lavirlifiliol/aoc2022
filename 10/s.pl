:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lambda)).

digit(X) :- member(X, "0123456789").

ints_a([X|Xs]) --> [X], {digit(X)}, ints_a(Xs).
ints_a([X]) --> [X], {digit(X)}.

ints(N) --> (("-",{S="-"})|([],{S=[]})),ints_a(Ns), {append(S, Ns, U), number_chars(N, U)}.

eol --> "\n".

inp(X, [X,X,NX|Xs]) --> "addx ", ints(N), {NX#=N+X}, eol, inp(NX, [NX|Xs]).
inp(X, [X|Xs]) --> "noop", eol, inp(X, Xs).
inp(_, []) --> [].

p2(_, []) --> [].
p2(C, [X|Xs])  -->
	{abs(C-X) #=< 1}, "#", (({C#=39, C1=0}, eol); ({C#\=39,C1#=C+1})),p2(C1, Xs).
p2(C, [X|Xs])  -->
	{abs(C-X) #> 1}, ".", (({C#=39, C1=0}, eol); ({C#\=39,C1#=C+1})),p2(C1, Xs).

solve(A, B):-
	phrase_from_file(inp(1, Xs), 'input'),
	maplist(\I^V^(nth1(I, Xs, J), V #= J * I), [20, 60, 100, 140, 180, 220], As),
	sum(As, #=, A),
	phrase(p2(0, Xs), B).
