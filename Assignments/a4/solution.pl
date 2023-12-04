%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Syntax

% What are the expressions?
expr(var(X)) :- string(X).
expr(lam(X,E)) :- string(X), expr(E).
expr(app(E1,E2)) :- expr(E1), expr(E2).
expr(pair(E1,E2)) :- expr(E1), expr(E2).
expr(fst(E)) :- expr(E).
expr(snd(E)) :- expr(E).
expr(true).
expr(false).
expr(and(E1,E2)) :- expr(E1), expr(E2).
expr(if(E1,E2,E3)) :- expr(E1), expr(E2), expr(E3).
expr(let(X,E1,E2)) :- string(X), expr(E1), expr(E2).

% What are the values?
value(true).
value(false).
value(pair(V1, V2)) :- value(V1), value(V2).
value(lam(_,_)).

% What are the types?
type(bool).
type(T1 * T2) :- type(T1), type(T2).
type(T1 => T2) :- type(T1), type(T2).

% [NOTE: Context Ordering]
% Normally we read contexts right-to-left, but programming languages
% tend to only offer left-to-right lists. To avoid re-implementing
% a bunch of list functions, we will use lists. Just note that
% contexts will be read backwards!
context([]).
context([X : T | Gamma]) :-
    string(X),
    type(T),
    context(Gamma).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Free Variables

% Take the union of a list of sets, represented as lists.
unions([],[]).
unions([X|Xs],Us) :-
    unions(Xs,UXs),
    union(X,UXs,Us).

% Compute the set of free variables for an expression.
fv(var(X), [X]).
fv(lam(X,E), Vs) :-
    % Lambdas bind variables, so we remove the bound var from the
    % free variable set.
    fv(E, EVs),
    delete(EVs, X, Vs).
fv(app(E1,E2), Vs) :-
    fv(E1, E1Vs),
    fv(E2, E2Vs),
    union(E1Vs, E2Vs, Vs).
fv(pair(E1,E2), Vs) :-
    fv(E1, E1Vs),
    fv(E2, E2Vs),
    union(E1Vs, E2Vs, Vs).
fv(fst(E), Vs) :-
    fv(E, Vs).
fv(snd(E), Vs) :-
    fv(E, Vs).
fv(true, []).
fv(false, []).
fv(and(E1,E2), Vs) :-
    fv(E1, E1Vs),
    fv(E2, E2Vs),
    union(E1Vs, E2Vs, Vs).
fv(if(E1,E2,E3), Vs) :-
    fv(E1, E1Vs),
    fv(E2, E2Vs),
    fv(E3, E3Vs),
    unions([E1Vs,E2Vs,E3Vs], Vs).
fv(let(X,E1,E2), Vs) :-
    fv(E1, E1Vs),
    fv(E2, E2Vs),
    % Let bindings only bind the variable in the second expression.
    % To account for this, we only remove X from the free-variable set
    % of the second expression.
    delete(E2Vs, X, E2VsBound),
    union(E1Vs, E2VsBound, Vs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Renaming
%%
%% Different, but equivalent renaming strategy.
%% This time we encode renaming as a data structure that maps variables to variables.
%% To perform a rename, we traverse the syntax tree, and replace every
%% variable with the corresponding variable in the map. If the variable
%% is not in the map, then we do not rename it.


% A renaming will be a map of variables to variables.
renaming([]).
renaming([X --> R|Rs]) :-
  string(X),
  string(R),
  renaming(Rs).

% Pick a new name for X that is not in the set AVOID.
freshen(X, Avoid, R) :-
    (member(X, Avoid) ->
	 string_concat(X, "'", X1),
	 freshen(X1, Avoid, R));
    (X = R).

% Apply a renamning to a variable.
rename_var(X, [], X).
rename_var(X, [X --> R|_], R) :-
    % Cut to avoid multiple renames.
    !.
rename_var(X, [_|Rs], R) :-
    rename_var(X, Rs, R).

% Apply a renaming to a term.
% Note that this will rename both bound and free variables.
rename(var(X), Rn, var(R)) :-
    rename_var(X, Rn, R).
rename(lam(X,E), Rn, lam(R,RE)) :-
    rename_var(X, Rn, R),
    rename(E, Rn, RE).
rename(app(E1,E2), Rn, app(RE1,RE2)) :-
    rename(E1, Rn, RE1),
    rename(E2, Rn, RE2).
rename(pair(E1,E2), Rn, pair(RE1,RE2)) :-
    rename(E1, Rn, RE1),
    rename(E2, Rn, RE2).
rename(fst(E), Rn, fst(RE)) :-
    rename(E, Rn, RE).
rename(snd(E), Rn, snd(RE)) :-
    rename(E, Rn, RE).
rename(true, _, true).
rename(false, _, false).
rename(and(E1,E2), Rn, and(RE1,RE2)) :-
    rename(E1, Rn, RE1),
    rename(E2, Rn, RE2).
rename(if(E1,E2,E3), Rn, if(RE1,RE2,RE3)) :-
    rename(E1, Rn, RE1),
    rename(E2, Rn, RE2),
    rename(E3, Rn, RE3).
rename(let(X,E1,E2), Rn, let(R,RE1,RE2)) :-
    rename_var(X, Rn, R),
    rename(E1, Rn, RE1),
    rename(E2, Rn, RE2).

:- rename(var("x"), ["x" --> "a"], var("a")).
:- rename(lam("x",var("x")), ["x" --> "a"], lam("a",var("a"))).
:- rename(lam("y",var("x")), ["x" --> "a"], lam("y",var("a"))).
:- rename(let("x", lam("y", var("x")),var("z")), ["x" --> "a", "y" --> "b"], let("a", lam("b", var("a")), var("z"))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Substitution
%%
%% We follow a slightly tweaked version of the substitution
%% algorithm. Instead of performing a big batch rename
%% at the start of substitution, we perform renames on an
%% as-needed basis. This involves slightly less code, but
%% is less efficient.

% Substitute x for e2 in e1.
subst(var(V), X, E2, ES) :-
    % Check to see if 'V' is the variable we are substituting for.
    (V = X -> ES = E2) ; (ES = var(V)).
subst(lam(V,E1), X, E2, lam(R, E1S)) :-
    (V = X -> V = R, E1S = E1);
    (fv(E2, Vs),
     % Construct a renaming that avoids all the free variables of E2
     freshen(V, Vs, R),
     % Apply the renaming to E1.
     rename(E1, [V --> R], RE1),
     % Substitute underneath the lambda.
     subst(RE1, X, E2, E1S)).
subst(app(EF,EA), X, E2, app(EFS,EAS)) :-
    subst(EF, X, E2, EFS),
    subst(EA, X, E2, EAS).
subst(pair(EL,ER), X, E2, pair(ELS,ERS)) :-
    subst(EL, X, E2, ELS),
    subst(ER, X, E2, ERS).
subst(fst(E), X, E2, fst(ES)) :-
    subst(E, X, E2, ES).
subst(snd(E), X, E2, snd(ES)) :-
    subst(E, X, E2, ES).
subst(true, _, _, true).
subst(false, _, _, false).
subst(and(EL,ER), X, E2, and(ELS,ERS)) :-
    subst(EL, X, E2, ELS),
    subst(ER, X, E2, ERS).
subst(if(EB,ET,EF), X, E2, if(EBS,ETS,EFS)) :-
    subst(EB, X, E2, EBS),
    subst(ET, X, E2, ETS),
    subst(EF, X, E2, EFS).
subst(let(V,E1,EB), X, E2, let(R,E1S,EBS)) :-
    % Push the substitution underneath the binding; don't need to worry about capture here.
    subst(E1,X,E2,E1S),
    % Check if the variable we are substituting for is bound.
    % If it is, we stop.
    (V = X -> V = R, EBS = EB);
    (fv(E2,Vs),
     % Construct a renaming that avoids all the free variables of E2
     freshen(V,Vs, R),
     % Apply the renaming to the body of the let binding.
     rename(EB, [V --> R], REB),
     % Perform the substitution.
     subst(REB, X, E2, EBS)).

:- subst(lam("x", app(var("y"), var("x"))), "y", var("x"), lam("x'", app(var("x"), var("x'")))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Evaluation

tsstep(app(E1,E2), app(SE1, E2), step_app_l(R)) :-
    tsstep(E1,SE1, R).
tsstep(app(V1,E2), app(V1, SE2), step_app_r(R)) :-
    value(V1),
    tsstep(E2,SE2, R).
tsstep(app(lam(X,E1), E2), ES, step_app_beta) :-
    value(E2),
    subst(E1,X,E2, ES).
tsstep(pair(E1,E2), pair(SE1, E2), step_pair_l(R)) :-
    tsstep(E1, SE1, R).
tsstep(pair(V1,E2), pair(V1, SE2), step_pair_r(R)) :-
    value(V1),
    tsstep(E2, SE2, R).
tsstep(fst(E), fst(SE), step_fst(R)) :-
    tsstep(E,SE, R).
tsstep(fst(pair(V1,V2)), V1, step_fst_beta) :-
    value(V1),
    value(V2).
tsstep(snd(E), snd(SE), step_snd(R)) :-
    tsstep(E,SE, R).
tsstep(snd(pair(V1,V2)), V2, step_snd_beta) :-
    value(V1),
    value(V2).
tsstep(and(E1,E2), and(SE1, E2), step_and_l(R)) :-
    tssetp(E1, SE1, R).
tsstep(and(true,_), true, step_and_true_l).
tsstep(and(false,E2), E2, step_and_false_l).
tsstep(if(EB,ET,EF), if(EBS,ET,EF), step_if(R)) :-
    tsstep(EB, EBS, R).
tsstep(if(true,ET,_), ET, step_if_true).
tsstep(if(false,_,EF), EF, step_if_false).
tsstep(let(X,E1,E2), let(X,SE1,E2), step_let(R)) :-
    tsstep(E1, SE1, R).
tsstep(let(X,E1,E2), ES, step_let_zeta) :-
    value(E1),
    subst(E2, X, E1, ES).

sstep(E, SE) :- tsstep(E,SE,_).

mstep(V,V).
mstep(E,V) :-
    sstep(E,SE),
    mstep(SE,V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Typing

% Look up an element of a context.
lookup(X,T,[X : T|_]) :-
    % Use a cut to ensure that we only get the first element.
    % See [NOTE: Context Ordering] for more information on
    % why we do this.
    !.
lookup(X,T,[_|Gamma]) :-
    lookup(X,T,Gamma).

typederiv(Gamma,var(X), T, tp_var) :-
    lookup(X,T,Gamma).
typederiv(Gamma,lam(X,E), T1 => T2, tp_fn_intro(R)) :-
    typederiv([X : T1|Gamma], E, T2, R).
typederiv(Gamma, app(E1,E2), T, tp_fn_elim(R1,R2)) :-
    typederiv(Gamma, E1, TI => T, R1),
    typederiv(Gamma, E2, TI, R2).
typederiv(Gamma, pair(E1,E2), T1 * T2, tp_pair_intro(R1,R2)) :-
    typederiv(Gamma, E1, T1, R1),
    typederiv(Gamma, E2, T2, R2).
typederiv(Gamma, fst(E), T, tp_pair_elim_l(R)) :-
    typederiv(Gamma, E, T * _, R).
typederiv(Gamma, snd(E), T, tp_pair_elim_r(R)) :-
    typederiv(Gamma, E, _ * T, R).
typederiv(_, true, bool, tp_bool_intro_true).
typederiv(_, false, bool, tp_bool_intro_false).
typederiv(Gamma, and(E1,E2), bool, tp_bool_elim_and(R1,R2)) :-
    typederiv(Gamma, E1, bool, R1),
    typederiv(Gamma, E2, bool, R2).
typederiv(Gamma, if(E1,E2,E3), T, tp_bool_elim_if(R1,R2,R3)) :-
    typederiv(Gamma, E1, bool, R1),
    typederiv(Gamma, E2, T, R2),
    typederiv(Gamma, E3, T, R3).
typederiv(Gamma, let(X,E1,E2), T, tp_let(R1, R2)) :-
    typederiv(Gamma, E1, TX, R1),
    typederiv([X : TX|Gamma], E2, T, R2).

typed(Gamma, E, T) :- typederiv(Gamma, E, T, _).
						

    
