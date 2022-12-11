:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lambda)).
:- use_module(library(debug)).

digit(X) :- member(X, "0123456789").

ints_a([X]) --> [X], {digit(X)}.
ints_a([X|Xs]) --> [X], {digit(X)}, ints_a(Xs).

ints(N) --> ints_a(Ns), {number_chars(N, Ns)}.

eol --> "\n".

items([X|Xs]) --> ints(X), ", ", items(Xs).
items([X]) --> ints(X), eol.

eval(Op, Old, R) :- 
	(
		Op =.. [O, old, R1],
		number(R1),
		C =.. [O, Old, R1],
		R #= C
	) ; (
		Op =.. [O, old, old],
		C =.. [O, Old, Old],
		R #= C
	).

operation(E) -->
	((ints(B), " ");("old ", {B=old})),
	[A], " ",
	(ints(C);("old", {C=old})),
	{E =.. [A, B, C]}.

monkey(m(Id, Is, Op, Te, It, If, 0)) -->
	"Monkey ", ints(Id), ":", eol,
	"  Starting items: ", items(Is),
	"  Operation: new = ", operation(Op), eol,
	"  Test: divisible by ", ints(Te), eol,
	"    If true: throw to monkey ", ints(It), eol,
	"    If false: throw to monkey ", ints(If), eol.

monkeys([E|Es]) --> monkey(E), eol, monkeys(Es).
monkeys([E]) --> monkey(E).

mid(Id, m(Id, _, _, _, _, _, _)).

throwto(Id, I, MsB, MsA) :-
	mid(Id, M),
	select(M, MsB, MsA1),
	M = m(Id, Is, T, U, V, W, X),
	append(Is, [I], Is1),
	MsA = [m(Id, Is1, T, U, V, W, X)|MsA1].

step_m(M, MsB, [M|MsB]):-arg(2, M, []).
step_m(m(Id, [X|Xs], Op, Te, MIdT, MIdF, N), MsB, MsA) :-
	eval(Op, X, X1),
	X2 #= div(X1, 3),
	R #= mod(X2, Te),
	N1 #= N + 1,
	(R#=0 -> throwto(MIdT, X2, MsB, MsA1); throwto(MIdF, X2, MsB, MsA1)),
	step_m(m(Id, Xs, Op, Te, MIdT, MIdF, N1), MsA1, MsA).
step_all(N, MsB, MsA) :-
	length(MsB, L),
	N #< L,
	mid(N, M),
	select(M, MsB, MsB1),
	N1 #= N + 1,
	step_m(M, MsB1, MsA1),
	step_all(N1, MsA1, MsA).

step_all(N, A, A) :-
	length(A, N).

do_nt(_, 0, A, A).
do_nt(F, N, A, B) :-
	N #> 0,
	call(F, A, B1),
	N1 #= N - 1,
	do_nt(F, N1, B1, B).

prod(A, B, C) :- C #= B*A.
prod(A, B) :- foldl(prod, A, 1, B).

step_i(Mod, s(I, Id, MsB), s(NI, NId, MsA)) :-
	%write(s(Id, I)), nl,
	select(m(Id,Is,Op,Te,MIdT,MIdF,N), MsB, MsA1),
	N1 #= N + 1,
	M1 = m(Id,Is,Op,Te,MIdT,MIdF,N1),
	eval(Op, I, NI1),
	NI #= mod(NI1, Mod),
	R #= mod(NI, Te),
	(R#=0 -> NId = MIdT ; NId = MIdF),
	MsA = [M1|MsA1].
	%(R#=0 -> step_i(Mod, I1, MIdT, [M1|MsA1], MsA); step_i(Mod, I1, MIdF, [M1|MsA1], MsA))


step_in(0, _, SB, SB).
step_in(N, Mod, SB, SA) :-
	N #> 0,
	step_i(Mod, SB, SA1),
	arg(2, SB, Id),
	arg(2, SA1, NId),
	N1 #= N - 1,
	(NId #> Id -> step_in(N, Mod, SA1, SA) ; step_in(N1, Mod, SA1, SA)).
proc_m(_, _, [], MsB, MsB).

proc_m(Mod, Id, [I|Is], MsB, MsA) :-
	write(s(Id, I)), nl,
	step_in(10000, Mod, s(I, Id, MsB), s(_, _, MsA1)),
	proc_m(Mod, Id, Is, MsA1, MsA).

proc_ms(_, [], MsB, MsB).
proc_ms(Mod, [M|Ms], MsB, MsA) :-
	arg(1, M, Id),
	arg(2, M, Is),
	proc_m(Mod, Id, Is, MsB, MsA1),
	proc_ms(Mod, Ms, MsA1, MsA).
p2(Ms, B) :-
	maplist(arg(4), Ms, Is),
	prod(Is, Mod),
	proc_ms(Mod, Ms, Ms, NMs),
	maplist(arg(7), NMs, Sc),
	sort(Sc, Scs),
	append(_, [T, U], Scs),
	B #= T * U.

solve(A, B) :-
	phrase_from_file(monkeys(Ms), 'input'),
	do_nt(step_all(0), 20, Ms, Aft),
	maplist(arg(7), Aft, Sc),
	sort(Sc, Scs),
	append(_, [T, U], Scs),
	A #= T * U,
	p2(Ms, B).
