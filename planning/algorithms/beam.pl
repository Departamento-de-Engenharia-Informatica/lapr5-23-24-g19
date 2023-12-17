:- module(algorithms_beam, [
    beam/5
]).

:- use_module('util/functional', [ retain_n/3 ]).

beam(Orig, Dest, EdgePred, Cam, Custo):-
    beam_aux(Dest, [(_, 0, [Orig])], EdgePred, Cam, Custo).

beam_aux(Dest, [(_, Custo, [Dest|T])|_], _, Cam, Custo):-
	reverse([Dest|T], Cam).

beam_aux(Dest, [(_, Ca, LA)|Outros], EdgePred, Cam, Custo):-
    % write('beam start'), nl,
    LA = [Act|_],
    findall((CEX, CaX, [X|LA]),
		(
            Dest \== Act,
            call(EdgePred, Act, X, CustoX),
            \+ member(X, LA),

            CaX is CustoX + Ca,
            estimate(X, Dest, EstX),
            CEX is CaX + EstX
        ),
        Novos
    ),
    append(Outros, Novos, Todos),
    sort(Todos, TodosOrd),

    paths_to_retain(N),
    retain_n(N, TodosOrd, AlgunsOrd),

	beam_aux(Dest, AlgunsOrd, EdgePred, Cam, Custo).


paths_to_retain(90).

estimate(Node1, Node2, Estimate) :-
    Node1 = cell(X1, Y1),
    Node2 = cell(X2, Y2),
    Estimate is sqrt((X1-X2)^2 + (Y1-Y2)^2).

% vim: ft=prolog
