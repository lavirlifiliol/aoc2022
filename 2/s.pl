:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(debug)).

line(X, Y) --> [X], " ", [Y].
lines([g(X, Y)|Gs]) --> line(X, Y), eol, lines(Gs).
lines([], [], []).
eol --> "\n".

score(g('A', 'X'), 4). 
score(g('B', 'X'), 1). 
score(g('C', 'X'), 7). 

score(g('A', 'Y'), 8). 
score(g('B', 'Y'), 5). 
score(g('C', 'Y'), 2). 

score(g('A', 'Z'), 3). 
score(g('B', 'Z'), 9). 
score(g('C', 'Z'), 6). 

same('A', 'X').
same('B', 'Y').
same('C', 'Z').

beat('A', 'C').
beat('B', 'A').
beat('C', 'B').

score2(g(P, 'X'), R) :-
	beat(P, Y),
	same(Y, MY),
	score(g(P, MY), R).
score2(g(P, 'Y'), R) :-
	same(P, Y),
	score(g(P, Y), R).

score2(g(P, 'Z'), R) :-
	beat(Y, P),
	same(Y, MY),
	score(g(P, MY), R).

solve(R, R2) :-
	phrase_from_file(lines(Vs), 'input'),
	maplist(score, Vs, Ss),
	maplist(score2, Vs, Ss2),
	sum(Ss, #=, R),
	sum(Ss2, #=, R2).
