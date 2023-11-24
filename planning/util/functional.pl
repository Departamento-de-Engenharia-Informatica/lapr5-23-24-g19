:- module(util_functional, [
    map/3,
    compare_on/4
]).

map(_, [], []).
map(Func, [X|Xs], [Y|Ys]) :-
    call(Func, X, Y),
    map(Func, Xs, Ys).


compare_on(Func, X, Y, Sign) :-
    call(Func, X, X1),
    call(Func, Y, Y1),
    compare(Sign, X1, Y1).


% vim: ft=prolog
