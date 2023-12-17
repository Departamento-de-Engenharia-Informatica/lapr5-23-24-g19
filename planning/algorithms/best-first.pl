:- module(algorithms_best_first, [
    bestfs/5
]).

bestfs(Orig, Dest, EdgePred, Path, Cost):-
    % write('hi'),nl,
    bestfs_aux(Dest, [Orig], EdgePred, Path, 0, Cost),
    % write('bye'),nl.

bestfs_aux(Dest, [Dest|T], _, Path, Acc, Acc):-
    reverse([Dest|T], Path).

bestfs_aux(Dest, LA, EdgePred, Path, Acc, Cost):-
    % write('comecou bestfs_aux'), nl,
	LA = [Act|_],
	findall((EstX, [X|LA], CostX),
		(
            % format('encontrar edge de ~w ~n', [Act]),
            call(EdgePred, Act, X, CostX),
            % format('encontrada edge: ~w ~n', [X]),
            \+ member(X, LA),
            estimate(X, Dest, EstX)
        ),
        Novos
    ),

    sort(Novos, NovosOrd),
    NovosOrd = [(_, Best, CostX)|_],

    NewAcc is Acc + CostX,
    bestfs_aux(Dest, Best, EdgePred, Path, NewAcc, Cost).

estimate(Node1, Node2, Estimate) :-
    Node1 = cell(X1, Y1),
    Node2 = cell(X2, Y2),
    Estimate is sqrt((X1-X2)^2 + (Y1-Y2)^2).

% vim: ft=prolog
