:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).

inp(Xs) --> seq(X), eol, {maplist(char_code, X, Xs)}.
eol --> "\n".
is_solution(As) :-
	all_distinct(As).
solve(A, N, Xs) :-
	length(W, N),
	append(_, W, L),
	append(L, _, Xs),
	is_solution(W),
	length(L, A).
solve(A, B) :-
	phrase_from_file(inp(Xs), 'input'),
	solve(A, 4, Xs),
	solve(B, 14, Xs).
