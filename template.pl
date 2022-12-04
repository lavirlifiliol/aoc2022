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
