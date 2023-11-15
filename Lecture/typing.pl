%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(clpb)).
:- use_module(library(clpfd)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expression Language
expr(true_).
expr(false_).
expr(if_then_else(B,T,E)) :- expr(B), expr(T), expr(E).
expr(zero).
expr(suc(X)) :- expr(X).
expr(iszero(X)) :- expr(X).
expr(pred(X)) :- expr(X).

nv(zero).
nv(suc(X)) :- nv(X).

value(true_).
value(false_).
value(X) :- nv(X).

type(bool).
type(nat).

typed(true_, bool).
typed(false_, bool).
typed(zero, nat).
typed(suc(X), nat) :- typed(X, nat).
typed(pred(X), nat) :- typed(X, nat).
typed(iszero(X), bool) :- typed(X, nat).
typed(if_then_else(X,Y,Z), T) :-
  typed(X,bool), typed(Y, T), typed(Z, T).

sstep(if_then_else(true_, X, _), X).
sstep(if_then_else(false_, _, Y), Y).
sstep(if_then_else(Z, X, Y), if_then_else(W, X, Y)) :- sstep(Z,W).
sstep(suc(X), suc(Y)) :- sstep(X,Y).
sstep(pred(zero), zero).
sstep(pred(suc(X)), X) :- nv(X).
sstep(pred(X), pred(Y)) :- sstep(X,Y).
sstep(izero(zero), true_).
sstep(iszero(suc(X)), false_) :- nv(X).
sstep(iszero(X), iszero(Y)) :- sstep(X,Y).

%% big-step
eval(true_, true_).
eval(false_, false_).
eval(zero, zero).
eval(suc(X), suc(Y)) :- eval(X, Y).
eval(iszero(zero), true_).
eval(iszero(suc(X)), false_) :- eval(X,Y), nv(Y).
eval(pred(zero), zero).
eval(pred(suc(X)),Y) :- eval(X,Y), nv(Y). 
eval(if_then_else(X, Y, _), W) :- eval(X, true_), eval(Y, W).
eval(if_then_else(X, _, Z), W) :- eval(X, false_), eval(Z, W).

%% multi-step
mstep(X, X) :- value(X).
mstep(X, Y) :- sstep(X, Z), mstep(Z, Y).

%% tracing single-step evaluator
tsstep(if_then_else(true_, X, _), X, e_IfTrue).
tsstep(if_then_else(false_, _, Y), Y, e_IfFalse).
tsstep(if_then_else(Z, X, Y), if_then_else(W, X, Y), e_If(T)) :- tsstep(Z,W,T).
tsstep(suc(X), suc(Y), e_Succ(T)) :- tsstep(X,Y,T).
tsstep(pred(zero), zero, e_PredZero).
tsstep(pred(suc(X)), X, e_PredSucc) :- nv(X).
tsstep(pred(X), pred(Y), e_Pred(T)) :- tsstep(X,Y,T).
tsstep(iszero(zero), true_, e_IsZeroZero).
tsstep(iszero(suc(X)), false_, e_IsZeroSucc(X)) :- nv(X).
tsstep(iszero(X), iszero(Y), e_IsZero(T)) :- tsstep(X,Y,T).

%% t-multi
tmstep(X, X, [e_Val(X)]) :- value(X).
tmstep(X, Y, [W | Ws]) :- tsstep(X, Z, W), tmstep(Z, Y, Ws).

%% typing derivation
typederiv(true_, bool, t_True).
typederiv(false_, bool, t_False).
typederiv(zero, nat, t_Zero).
typederiv(suc(X), nat, t_Succ(T)) :- typederiv(X, nat, T).
typederiv(pred(X), nat, t_Pred(T)) :- typederiv(X, nat, T).
typederiv(iszero(X), bool, t_Iszero(T)) :- typederiv(X, nat, T).
typederiv(if_then_else(X,Y,Z), T, t_If(XD, YD, ZD)) :-
  typederiv(X,bool,XD), typederiv(Y, T, YD), typederiv(Z, T, ZD).

%% can we trivially use Prolog as a theorem prover? 
typedvalue(V, T) :- value(V), typederiv(V, T, _).
progress(V, T) :- typederiv(V, T, _), (value(V); sstep(V, _)).
