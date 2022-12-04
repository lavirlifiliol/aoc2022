:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).

digit(X) :- member(X, "0123456789").

ints_a([X]) --> [X], {digit(X)}.
ints_a([X|Xs]) --> [X], {digit(X)}, ints_a(Xs).

ints(N) --> ints_a(Ns), {number_chars(N, Ns)}.

eol --> "\n".

group([X]) --> ints(X), eol.
group([X|Xs]) --> ints(X), eol, group(Xs).

groups([X]) --> group(X).
groups([X|Xs]) --> group(X), eol, groups(Xs).

suma(T, U) :- sum(T, #=, U).

highest_r([], M, M).
highest_r([X|Xs], M, M1) :-
	M2 #= max(M, X),
	highest_r(Xs, M2, M1).

solve(A, B) :-
	phrase_from_file(groups(Gs), 'input'),
	maplist(suma, Gs, Fs),
	sort(Fs, Rs),
	append(_, [D,C,A], Rs),
	B #= A+C+D.

