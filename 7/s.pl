:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(pairs)).
:- use_module(library(debug)).

digit(X) :- member(X, "0123456789").

ints_a([X]) --> [X], {digit(X)}.
ints_a([X|Xs]) --> [X], {digit(X)}, ints_a(Xs).

ints(N) --> ints_a(Ns), {number_chars(N, Ns)}.

seq1([E]) --> [E], {char_type(E, alpha)}.
seq1([E|Es]) --> [E], {char_type(E, alpha);E='.'}, seq1(Es).

eol --> "\n".

ls_data([], [U|Xs], [U|Xs]).
ls_data([], [], []).
ls_data([e(S, N)|Es]) --> ints(S), " ", seq1(N), eol, ls_data(Es).
ls_data([dir(N, _)|Es]) --> "dir ", seq1(N), eol, ls_data(Es).

trace(A, A) :- write(A), nl.

line(cdup) --> "$ cd ..", eol.
line(cd(U)) --> "$ cd ", seq1(U), eol.
line(ls(Cs)) --> "$ ls", eol, ls_data(Cs).

inp([]) --> [].
inp([X|Xs]) --> line(X), inp(Xs).

mktree(dir(P, Xs)) --> [cd(N)], {member(dir(N, Es), Xs)}, mktree(dir(N, Es)), mktree(dir(P, Xs)).
mktree(dir(N, Xs)) --> [ls(Xs)], mktree(dir(N, Xs)).
mktree(_) --> [cdup].
mktree(_) --> [].

each_entry([], []) --> [].
each_entry([E|Es], [Size|Sizes]) --> comp_sizes(E, Size), each_entry(Es, Sizes).

comp_sizes(e(S, _), S) --> [].
comp_sizes(dir(N, E), S) -->
	each_entry(E, Vs),
	{sum(Vs, #=, S)}, [dir(N, S)].

sol1(dir(_, N), N) :- N #=< 100000.
sol1(dir(_, N), 0) :- N #> 100000.

sol2(_, TD, dir(I, N), N-I) :- N #>= TD.
sol2(Rs, TD, dir(I, N), Rs-I) :- N #< TD.

solve(A, B) :-
	phrase_from_file(inp(Cmds), 'input'),
	Tree = dir("/", _),
	phrase(mktree(Tree), Cmds),
	phrase(comp_sizes(Tree, Rs), Ss),
	maplist(sol1, Ss, As),
	sum(As, #=, A),
	ToRm #= Rs - 40000000,
	maplist(sol2(Rs, ToRm), Ss, Bs),
	keysort(Bs, Bs2),
	[B-_|_] = Bs2.
