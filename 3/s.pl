:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).

eol --> "\n".

lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

eos([], []).

sack(L, r(A, B)) :- length(L, N2), N #= N2 // 2, length(A, N), length(B, N), append(A, B, L).

com(r(A, B), U) :- sort(A, As), sort(B, Bs), ord_intersect(As, Bs, [U]).
com([A,B,C], U) :- sort(A, As), sort(B, Bs), sort(C, Cs),  ord_intersect(As, Bs, Ds), ord_intersect(Cs, Ds, [U]).

value(C, V) :- char_type(C, upper), atom_codes(C, [N]), V #= N - 64 + 26.
value(C, V) :- char_type(C, lower), atom_codes(C, [N]), V #= N - 96.

groups([[A,B,C]|Rs]) --> [A],[B],[C],groups(Rs).
groups([]) --> [].

solve(P1, P2) :-
	phrase_from_file(lines(Ls), 'input'),
	maplist(sack, Ls, Rs),
	maplist(com, Rs, Cs),
	maplist(value, Cs, Vs),
	sum(Vs, #=, P1),
	phrase(groups(Gs), Ls),
	maplist(com, Gs, Cs2),
	maplist(value, Cs2, Vs2),
	sum(Vs2, #=, P2).
