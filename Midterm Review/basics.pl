:- use_module(library(clpb)).
:- use_module(library(clpfd)).

elem(X, [X|_]).
elem(X, [_|Xs]) :- elem(X, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Binary trees
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nothing().
just(_).

isJust(just(_)).
isNothing(nothing()).

ifthenelse(true, T, _, T).
ifthenelse(false, _, F, F).

fromJust(Dflt, X, Dflt) :- isNothing(X).
fromJust(_,    just(Z), Z).

:- isNothing(nothing()).
:- \+ isNothing(just(1)).
:- isJust(just(10)).
:- \+ isJust(nothing()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Binary trees
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leaf(_).
branch(_, _).

is_tree(leaf(X)) :- X in inf..sup.
is_tree(branch(L, R)) :- is_tree(L), is_tree(R).

hasLeaf(leaf(X), X).
hasLeaf(branch(L, R), X) :- is_tree(L), is_tree(R), (hasLeaf(L, X) ; hasLeaf(R, X)).

:- hasLeaf(leaf(20), 20).
:- \+ hasLeaf(leaf(20), 10).
:- hasLeaf(branch(leaf(1), leaf(10)), 10).
:- \+ hasLeaf(branch(leaf(1), leaf(2)), 10).
:- hasLeaf(branch(leaf(1), branch(leaf(2), leaf(10))), 10).

sumLeaves(leaf(X), X) :- X in inf..sup.
sumLeaves(branch(L, R), Z) :-
    sumLeaves(L, LZ),
    sumLeaves(R, RZ),
    Z #= LZ + RZ.

:-    sumLeaves(leaf(10), Z), Z is 10.
:-    sumLeaves(branch(leaf(5), leaf(5)), Z), Z is 10.
:-    sumLeaves(branch(leaf(5), branch(leaf(3), leaf(2))), Z), Z is 10.
:- \+ sumLeaves(branch(leaf(5), branch(leaf(3), leaf(3))), 10).
