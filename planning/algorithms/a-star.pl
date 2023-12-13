:- module(algorithms_astar, [
    a_star/5
]).

a_star(Orig, Dest, EdgePred, Cam, Custo):-
    a_star2(Dest, [(_, 0, [Orig])], EdgePred, Cam, Custo).

a_star2(Dest, [(_, Custo, [Dest|T])|_], _, Cam, Custo):-
	reverse([Dest|T], Cam).

a_star2(Dest, [(_, Ca, LA)|Outros], EdgePred, Cam, Custo):-
	LA = [Act|_],
	findall((CEX, CaX, [X|LA]),
		(
            Dest \== Act, call(EdgePred, Act, X, CustoX), \+ member(X, LA),
            CaX is CustoX + Ca, estimate(X, Dest, EstX),
            CEX is CaX + EstX
        ),
        Novos
    ),
	append(Outros, Novos, Todos),
	sort(Todos, TodosOrd),
	a_star2(Dest, TodosOrd, EdgePred, Cam, Custo).



estimate(Node1, Node2, Estimate) :-
    Node1 = cell(X1, Y1),
    Node2 = cell(X2, Y2),
    Estimate is sqrt((X1-X2)^2 + (Y1-Y2)^2).

% vim: ft=prolog
