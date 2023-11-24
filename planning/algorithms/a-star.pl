:- module(algorithms_astar, [
    a_star/4
]).

a_star(Orig, Dest, Cam, Custo):-
    a_star2(Dest, [(_, 0, [Orig])], Cam, Custo).

a_star2(Dest, [(_, Custo, [Dest|T])|_], Cam, Custo):-
	reverse([Dest|T], Cam).

a_star2(Dest, [(_, Ca, LA)|Outros], Cam, Custo):-
	LA = [Act|_],
	findall((CEX, CaX, [X|LA]),
		(
            Dest \== Act, edge(Act, X, CustoX), \+ member(X, LA),
            CaX is CustoX + Ca, estimativa(X, Dest, EstX),
            CEX is CaX + EstX
        ),
        Novos),
	append(Outros, Novos, Todos),
	sort(Todos, TodosOrd),
	a_star2(Dest, TodosOrd, Cam, Custo).

% substituir a chamada edge(Act,X,CustoX)
% por (edge(Act,X,CustoX);edge(X,Act,CustoX))
% se quiser ligacoes bidirecionais


estimativa(Nodo1, Nodo2, Estimativa) :-
	node(Nodo1, X1, Y1),
	node(Nodo2, X2, Y2),
	Estimativa is sqrt((X1-X2)^2 + (Y1-Y2)^2).




% vim: ft=prolog
