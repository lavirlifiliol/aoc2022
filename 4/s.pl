:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).

digit(X) :- member(X, "0123456789").

ints_a([X]) --> [X], {digit(X)}.
ints_a([X|Xs]) --> [X], {digit(X)}, ints_a(Xs).

ints(N) --> ints_a(Ns), {number_chars(N, Ns)}.

line(p(A-B,C-D)) --> ints(A), "-", ints(B), ",", ints(C), "-", ints(D).
lines([]) --> [].
lines([E|Es]) --> line(E), eol, lines(Es).

is_subs(p(A-B, C-D)) :-
	(A #>= C, B #=< D) ; (C #>= A, D #=< B).

is_subs2(p(A-B, C-D)) :-
	(A #>= C, A #=< D) ; (C #>= A, C #=< B).

value(U, N) :- is_subs(U) -> N=1; N=0.
value2(U, N) :- is_subs2(U) -> N=1; N=0.

eol --> "\n".

solve(P1, P2) :-
	phrase_from_file(lines(Ls), 'input'),
	maplist(value, Ls, Rs),
	maplist(value2, Ls, Rs2),
	sum(Rs, #=, P1),
	sum(Rs2, #=, P2).
