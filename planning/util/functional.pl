:- module(util_functional, [
    map/3,
    compare_on/4
]).

map(_, [], []).
map(Func, [X|Xs], [Y|Ys]) :-
    call(Func, X, Y),
    map(Func, Xs, Ys).


% SEE:
% - https://stackoverflow.com/a/23285455
% - https://stackoverflow.com/a/8780144
compare_on(Func, X, Y, Sign) :-
    call(Func, X, X1),
    call(Func, Y, Y1),
    (
        (X1 == Y1, !, Sign = (<));
        compare(Sign, X1, Y1)
    ).

% vim: ft=prolog
