% this is a comment
member(X,[X|_]).
member(X,[_|Xs]) :- member(X,Xs).

islist([]).
islist([_ | Tail]) :- islist(Tail).

% delete anywhere
del(X, [X|L1], L1).
del(X, [Y|L1], [Y|L2]) :- del(X, L1, L2).

% delete first
del_first(X, [X|L1], L1).
del_first(X, [Y|L1], [Y|L2]) :-
  X \= Y , del_first(X, L1, L2).

app([], L, L).
app([X|Xs], Ys, [X|Zs]) :- app(Xs,Ys,Zs).

pref(P,L) :- app(P, _, L).

subl(S, L) :- append(_, S, Left), append(Left, _, L).


qs([],[]).
qs([X|Xs], Ys) :-
  integer(X) ,
  part(Xs, X, Left, Right) ,
  qs(Left, Ls),
  qs(Right, Rs),
  append(Ls, [X|Rs], Ys) ,

part([], _, [], []).
part([X|Xs], Y, [X|Ls], Rs) :-
  integer(X) ,
  X < Y , part(Xs, Y, Ls, Rs).
part([X|Xs], Y, Ls, [X|Rs]) :-
  integer(X) ,
  X >= Y , part(Xs, Y, Ls, Rs).

% list length
listlen([],0).
listlen([X|Xs], N) :- listlen(Xs,M) , N is M+1.

% list length via an accumulator
listlen2(L,N) :- listacc(L, 0, N).

listacc([], S, S).
listacc([X|Xs], S, N) :- L is S+1 , listacc(Xs, L, N).

listsum([],0).
listsum([X|Xs], N) :- listsum(Xs, M), N is M+X.

listsum2(L,N) :- sumacc(L, 0, N).
sumacc([], S, S).
sumacc([X|Xs], S, N) :- T is S+X , sumacc(Xs, T, N).
