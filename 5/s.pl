:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(debug)).

digit(X) :- member(X, "0123456789").

ints_a([X]) --> [X], {digit(X)}.
ints_a([X|Xs]) --> [X], {digit(X)}, ints_a(Xs).

ints(N) --> ints_a(Ns), {number_chars(N, Ns)}.

eol --> "\n".

crate([]) --> "   ".
crate([C]) --> "[",[C],"]".
crate_level([C|Cs]) --> crate(C), (" ", crate_level(Cs) ; {Cs=[]}, eol).
all_levels([C|Cs]) --> crate_level(C), all_levels(Cs).
all_levels([]) --> !, " 1 ", seq(_), eol, eol.
append_levels([Level], Level).
append_levels([Level|Ls], L) :-
	append_levels(Ls, Later),
	maplist(append, Level, Later, L).

moves([m(N, F, T)|Ms]) --> "move ", ints(N), " from ", ints(F1), {F#=F1-1}, " to ", ints(T1), {T#=T1-1}, eol, (moves(Ms) ; {Ms=[]}).

inp(Stack, Moves) --> all_levels(Ss), {append_levels(Ss, Stack)}, moves(Moves).

reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

id(A, A, []).

move(R, m(N, F, T), B, A) :-
	length(Fb, F),
	append(Fb, [Fcr|Frs], B),
	length(Tb, T),
	append(Tb, [Tcr|_], B),
	length(Mv, N),
	append(Mv, NF, Fcr),
	C =.. [R, Mv, Mvr, []],
	call(C),
	append(Mvr, Tcr, NT),
	append(Fb, [NF|Frs], Ap),
	length(Tb2, T),
	append(Tb2, [_|Trs], Ap),
	append(Tb2, [NT|Trs], A),
	true.

ap([M|Ms]) --> move(reverse, M), ap(Ms).
ap([]) --> [].

ap2([M|Ms]) --> move(id, M), ap2(Ms).
ap2([]) --> [].

fst([A|_], A).

solve(A, B) :-
	phrase_from_file(inp(Crates, Cmds), 'input'),
	ap(Cmds, Crates, After),
	ap2(Cmds, Crates, After2),
	maplist(fst, After, A),
	maplist(fst, After2, B).
