:- use_module(library(clpb)).
:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Problem 1

elem(X, [X|_]).
elem(X, [_|Xs]) :- elem(X,Xs).

:- elem(1,[1,2,3]).
:- elem(1,[2,1,3]).
:- elem(1,[2,1,3,1]).
:- \+ elem(1,[2,3,2]).
:- \+ elem(1,[]).

pick(X,[X|Xs],Xs).
pick(X,[Y|Ys],[Y|Zs]) :- pick(X,Ys,Zs).

:- pick(1,[1,2,3],[2,3]).          %% [2,3] is [1,2,3] with 1 removed
:- \+ pick(1,[2,3],[2,3]).         %% 1 is not in [2,3], so this is false.
:- pick(1,[2,1,2,1,3],[2,2,1,3]).  %% We only remove 1 from [2,1,2,1,3] one time.
:- pick(1,[2,1,2,1,3],[2,1,2,3]).  %% Furthermore, we can remove either 1.
:- \+ pick(1,[2,1,2,1,3],[2,2,3]). %% However, we should only remove a single 1.

permutation([],[]).
permutation([X|Xs],Ys) :-
    pick(X,Ys,Rest),
    permutation(Xs,Rest).

:- permutation([],[]).
:- permutation([1,2],[2,1]).
:- permutation([1,2,3,3],[3,2,1,3]).
:- \+ permutation([1,2,1,3,3],[3,2,1,3]).
:- permutation([1,2,1,3,3],[1,3,2,1,3]).

sorted([]).
sorted([_]).
sorted([X,Y|Xs]) :- X =< Y, sorted([Y|Xs]).

:- sorted([]).
:- sorted([3]).
:- sorted([1,2,4,9]).
:- sorted([1,1,1,2,4,4,9]).
:- \+ sorted([1,1,2,1,4,4,9]).

naive_sort(Xs,Ys) :-
    sorted(Ys),
    permutation(Xs,Ys).

:- naive_sort([],[]).
:- naive_sort([1],[1]).
:- naive_sort([2,1,9,3],[1,2,3,9]).
:- naive_sort([2,1,3,1,2],[1,1,2,2,3]).
:- \+ naive_sort([1,3,2,4],[4,3,2,1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Problem 2

%% Modified a bit from the assignment; needed for testing.
is_creature(X) :- var(X).

is_statement(gnome(X)) :- is_creature(X).
is_statement(goblin(X)) :- is_creature(X).
is_statement(and(X,Y)) :-
    is_statement(X),
    is_statement(Y).
is_statement(or(X,Y)) :-
    is_statement(X),
    is_statement(Y).

clue_2(A,B) :-
    sat(A =:= ~ A * ~ B).

:- clue_2(0,1).

riddle_1(A,B) :-
    sat(A =:= ~ B),
    sat(B =:= A * B).

:- riddle_1(1,0).

riddle_2(A,B,C,D) :-
    sat(A =:= D),
    sat(B =:= ~ C * A),
    sat(C =:= ~ C + C),
    sat(D =:= card([2],[A, ~ A, ~ B * ~ C])).

:- riddle_2(0,0,1,0).

gnomes_or_goblins_to_sat(gnome(N), N).
gnomes_or_goblins_to_sat(goblin(N), ~ N).
gnomes_or_goblins_to_sat(and(X,Y), T1 * T2) :-
    gnomes_or_goblins_to_sat(X, T1),
    gnomes_or_goblins_to_sat(Y, T2).
gnomes_or_goblins_to_sat(or(X,Y), T1 + T2) :-
    gnomes_or_goblins_to_sat(X, T1),
    gnomes_or_goblins_to_sat(Y, T2).

gnomes_or_goblins(_, []).
gnomes_or_goblins([G|Gs], [E|Es]) :-
    gnomes_or_goblins_to_sat(E, T),
    sat(G =:= T),
    gnomes_or_goblins(Gs, Es).

:- gnomes_or_goblins([A,B], [and(goblin(A),goblin(B))]),
   A is 0,
   B is 1.

:- gnomes_or_goblins([A,B],[gnome(A),and(goblin(A),goblin(B))]),
   A is 1,
   B is 0.

:- gnomes_or_goblins([A,B], [and(gnome(A), gnome(B))]),
    A is 1,
    B is 1.

:- gnomes_or_goblins([A,B], [or(gnome(A), goblin(B)), gnome(B)]),
   A is 0,
   B is 1.

:- gnomes_or_goblins([A,B], [or(gnome(A), goblin(B)), gnome(B)]),
   A is 0,
   B is 1.

:- gnomes_or_goblins([A,B,C], [and(goblin(C), gnome(C)), and(goblin(A), gnome(A)), or(goblin(B), gnome(C))]),
  A is 0,
  B is 0,
  C is 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Question 3

boolean(true).
boolean(false).


is_expr(int(V)) :- V in inf..sup.
is_expr(bool(B)) :- boolean(B).
is_expr(add(X, Y)) :- is_expr(X), is_expr(Y).
is_expr(mul(X, Y)) :- is_expr(X), is_expr(Y).
is_expr(neg(X)) :- is_expr(X).
is_expr(and(X,Y)) :- is_expr(X), is_expr(Y).
is_expr(xor(X,Y)) :- is_expr(X), is_expr(Y).
is_expr(if(B,X,Y)) :- is_expr(B), is_expr(X), is_expr(Y).

and(true,true,true).
and(_,_,false).

xor(true,false,true).
xor(false,false,true).
xor(_,_,false).

if(true,X,_,X).
if(false,_,Y,Y).

eval_expr(add(X,Y),V) :-
    eval_expr(X,VX),
    eval_expr(Y,VY),
    V #= VX + VY.
eval_expr(mul(X,Y),V) :-
    eval_expr(X,VX),
    eval_expr(Y,VY),
    V #= VX * VY.
eval_expr(neg(X),V) :-
    eval_expr(X,VX),
    V #= - VX.
eval_expr(and(X,Y),V) :-
    eval_expr(X,VX),
    eval_expr(Y,VY),
    and(VX,VY,V).
eval_expr(xor(X,Y),V) :-
    eval_expr(X,VX),
    eval_expr(Y,VY),
    xor(VX,VY,V).
eval_expr(if(B,X,Y),V) :-
    eval_expr(B,VB),
    eval_expr(X,VX),
    eval_expr(Y,VY),
    if(VB,VX,VY,V).
eval_expr(int(V),V) :- V in inf..sup.
eval_expr(bool(V),V) :- boolean(V).

:- eval_expr(add(int(1),int(2)),3).
:- eval_expr(mul(int(2),int(2)),4).
:- eval_expr(add(mul(int(2),int(2)),int(3)),7).
:- eval_expr(neg(neg(int(7))),7).
:- eval_expr(add(neg(int(10)),int(10)),0).
:- eval_expr(mul(add(int(3),int(3)),int(10)),60).
:- eval_expr(and(bool(true),bool(true)), false).
:- eval_expr(xor(bool(true),bool(false)), true).
:- eval_expr(if(and(bool(true),bool(true)), add(int(1),int(2)), mul(int(3),int(3))), 3).
:- eval_expr(if(xor(bool(true),bool(true)), add(int(1),int(2)), mul(int(3),int(3))), 9).
:- eval_expr(add(int(400549390), int(7)), 400549397).
:- eval_expr(add(int(400549400), neg(int(3))), 400549397).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Bonus 2: Program Search

steps(add(X,Y), Steps) :-
    XSteps #>= 0,
    YSteps #>= 0,
    Steps #= 1 + XSteps + YSteps,
    steps(X,XSteps),
    steps(Y,YSteps).
steps(mul(X,Y), Steps) :-
    XSteps #>= 0,
    YSteps #>= 0,
    Steps #= 1 + XSteps + YSteps,
    steps(X,XSteps),
    steps(Y,YSteps).
steps(neg(X), Steps) :-
    XSteps #>= 0,
    Steps #= 1 + XSteps,
    steps(X,XSteps).
steps(and(X,Y), Steps) :-
    XSteps #>= 0,
    YSteps #>= 0,
    Steps #= 1 + XSteps + YSteps,
    steps(X,XSteps),
    steps(Y,YSteps).
steps(xor(X,Y), Steps) :-
    XSteps #>= 0,
    YSteps #>= 0,
    Steps #= 1 + XSteps + YSteps,
    steps(X,XSteps),
    steps(Y,YSteps).
steps(if(B,X,Y), Steps) :-
    BSteps #>= 0,
    XSteps #>= 0,
    YSteps #>= 0,
    Steps #= 1 + BSteps + XSteps + YSteps,
    steps(B,BSteps),
    steps(X,XSteps),
    steps(Y,YSteps).
steps(int(V), 0) :- V in inf..sup.
steps(bool(V), 0) :- boolean(V).


is_type(int).
is_type(boolean).

val_type(V,int) :- integer(V), V in inf..sup.
val_type(V,boolean) :- boolean(V).
    
typecheck(add(X,Y), int) :- typecheck(X,int), typecheck(Y,int).
typecheck(mul(X,Y), int) :- typecheck(X,int), typecheck(Y,int).
typecheck(neg(X), int) :- typecheck(X,int).
typecheck(and(X,Y), boolean) :- typecheck(X,boolean), typecheck(Y,boolean).
typecheck(xor(X,Y), boolean) :- typecheck(X,boolean), typecheck(Y,boolean).
typecheck(if(B,X,Y), A) :- typecheck(B,boolean), typecheck(X,A), typecheck(Y,A).
typecheck(int(V), int) :- V in inf..sup.
typecheck(bool(V), boolean) :- boolean(V).

expr_search(E,N,V) :-
    steps(E,N),
    typecheck(E,T),
    val_type(V,T),
    eval_expr(E,V).
