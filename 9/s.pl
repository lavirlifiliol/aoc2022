:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).
:- use_module(library(debug)).
:- use_module(library(dcgs)).
:- use_module(library(lambda)).

digit(X) :- member(X, "0123456789").

ints_a([X]) --> [X], {digit(X)}.
ints_a([X|Xs]) --> [X], {digit(X)}, ints_a(Xs).

ints(N) --> ints_a(Ns), {number_chars(N, Ns)}. 

eol --> "\n".

trace(X, A,A) :- write(X), write(A), nl.
trace(A, A) :- trace(d, A, A).

cmd(l(N)) -->  "L ", ints(N).
cmd(r(N)) -->  "R ", ints(N).
cmd(u(N)) -->  "U ", ints(N).
cmd(d(N)) -->  "D ", ints(N).
inp([], [], []).
inp([X|Xs]) --> cmd(X), eol, inp(Xs).

sign(A, -1) :- A #< -1.
sign(A, 1) :- A #> 1.
signe(A, -1) :- A #< 0.
signe(A, 1) :- A #> 0.

adj(A, B) :- A - B #= 1; A - B #= -1 ; A #= B.

upt(Tx0-Ty0, s(Hx-Hy, Tx0-Ty0)) :-
	%	(adj(Tx0, Hx), Ty0#=Hy); (adj(Ty0, Hy), Tx0#=Hx).
	adj(Tx0, Hx), adj(Ty0, Hy).

upt(Tx0-Ty0, s(Hx-Hy, Tx-Ty0)) :-
	Ty0 #= Hy,
	U #= Hx - Tx0,
	sign(U, S),
	Tx #= Tx0 + S.

upt(Tx0-Ty0, s(Hx-Hy, Tx0-Ty)) :-
	Tx0 #= Hx,
	U #= Hy - Ty0,
	sign(U, S),
	Ty #= Ty0 + S.

upt(Tx0-Ty0, s(Hx-Hy, Tx-Ty)) :-
	(sign(Hx-Tx0, _); sign(Hy-Ty0, _)),
	signe(Hx-Tx0, U),
	signe(Hy-Ty0, V),
	Tx #= Tx0 + U,
	Ty #= Ty0 + V.

uph(l(_), Hx0-Hy0, Hx-Hy0) :- Hx #= Hx0 - 1.
uph(r(_), Hx0-Hy0, Hx-Hy0) :- Hx #= Hx0 + 1.
uph(u(_), Hx0-Hy0, Hx0-Hy) :- Hy #= Hy0 - 1.
uph(d(_), Hx0-Hy0, Hx0-Hy) :- Hy #= Hy0 + 1.

uprope([A], []).
uprope([Hx-Hy,Tx-Ty|Us], [Tx1-Ty1|Rs]) :-
	upt(Tx-Ty, s(Hx-Hy,Tx1-Ty1)),
	uprope([Tx1-Ty1|Us], Rs).

move(M, I, R) :- 
	M =.. [U, N],
	N#=0 -> R=I;
	(
		I = [[Hx-Hy|Us]|_],
		uph(M, Hx-Hy, Hx1-Hy1),
		uprope([Hx1-Hy1|Us], NR),
		M =.. [U,N],
		N1 #= N - 1,
		Ms1 =.. [U,N1],
		Our = [[Hx1-Hy1|NR]|I],
		move(Ms1, Our, R)
	).

app([], S, S).
app([X|Xs], I, R) :- move(X, I, R1), app(Xs, R1, R).

start(0-0).

solve(A, B) :-
	phrase_from_file(inp(Cmds), 'input'),
	app(Cmds, [[0-0,0-0]], Sts),
	maplist(nth1(2), Sts, Tls),
	sort(Tls, R),
	length(R, A),
	length(Rb, 10),
	maplist(start, Rb),
	app(Cmds, [Rb], Sts2),
	maplist(nth1(10), Sts2, Tls2),
	sort(Tls2, R2),
	length(R2, B).
