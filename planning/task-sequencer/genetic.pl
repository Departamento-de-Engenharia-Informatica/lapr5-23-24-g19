:-dynamic geracoes/1.
:-dynamic populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.

% parameterizacao
inicializa:-
    write('\33\[2J'), %limpar o terminal
    write('Numero de novas Geracoes: '),read(NG),
    (retract(geracoes(_));true), asserta(geracoes(NG)),
	write('Dimensao da Populacao: '),read(DP),
	(retract(populacao(_));true), asserta(populacao(DP)),
	write('Probabilidade de Cruzamento (%):'), read(P1),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%):'), read(P2),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).

%TODO: Change to return task quantity
%tasks(N):- N is 5
tasks(5).

gera:-
	inicializa,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAvaliada='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	gera_geracao(0,NG,PopOrd).

gera_populacao(Pop):-
	populacao(TamPop),
	tasks(NumT),
    %TODO: FIND ALL TASKS
	findall(Task,task(Task,_,_,_),TasksList),
	gera_populacao(TamPop,TasksList,NumT,Pop).

gera_populacao(0,_,_,[]):-!.

gera_populacao(TamPop,TasksList,NumT,[Ind|Rest]):-
	TamPop1 is TamPop-1,
	gera_populacao(TamPop1,TasksList,NumT,Rest),
	gera_individuo(TasksList,NumT,Ind),
	not(member(Ind,Rest)).
gera_populacao(TamPop,TasksList,NumT,L):-
	gera_populacao(TamPop,TasksList,NumT,L).

gera_individuo([G],1,[G]):-!.

gera_individuo(TasksList,NumT,[G|Rest]):-
	NumTemp is NumT + 1, % To use with random
	random(1,NumTemp,N),
	retira(N,TasksList,G,NewList),
	NumT1 is NumT-1,
	gera_individuo(NewList,NumT1,Rest).

retira(1,[G|Rest],G,Rest).
retira(N,[G1|Rest],G,[G1|Rest1]):-
	N1 is N-1,
	retira(N1,Rest,G,Rest1).

avalia_populacao([],[]).
avalia_populacao([Ind|Rest],[Ind*V|Rest1]):-
	avalia(Ind,V),
	avalia_populacao(Rest,Rest1).

avalia(Seq,V):-
	avalia(Seq,0,V).

avalia([],_,0).


%Exemplo de ALGAV
% avalia([T|Rest],Inst,V):-
% 	tarefa(T,Dur,Prazo,Pen),
% 	InstFim is Inst+Dur,
% 	avalia(Rest,InstFim,VRest),
% 	(
% 		(InstFim =< Prazo,!, VT is 0)
%   ;
% 		(VT is (InstFim-Prazo)*Pen)
% 	),
% 	V is VT+VRest.


% % TODO: ADAPTAR PARA AVALIAR DE ACORDO COM O TEMPO ENTRE TAREFAS
% % TODO: Criar os factos que indiquem os tempos entre os pontos finais e iniciais das tarefas
avalia([T|Rest], Inst, V):-
    %TODO:  Orig: T: final position
    %TODO:  Dest: [T|[X|Rest]] X: inital position
    compute_path(Orig, Dest, _, PathCost),  % Assuming compute_path/4 returns the cost of the path
    avalia(Rest, InstFim, VRest),
    V is PathCost + VRest.

ordena_populacao(PopAv,PopAvOrd):-
	bsort(PopAv,PopAvOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
	bsort(Xs,Zs),
	btroca([X|Zs],Ys).


btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
	VX>VY,!,
	btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).


gera_geracao(G,G,Pop):-!,
	write('Geracao '), write(G), write(':'), nl, write(Pop), nl.

gera_geracao(N,G,Pop):-
	write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	cruzamento(Pop,NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	N1 is N+1,
	gera_geracao(N1,G,NPopOrd).

gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	tasks(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);(P1=P21,P2=P11)).
gerar_pontos_cruzamento1(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).


cruzamento([],[]).
cruzamento([Ind*_],[Ind]).
cruzamento([Ind1*_,Ind2*_|Rest],[NInd1,NInd2|Rest1]):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(Pcruz),random(0.0,1.0,Pc),
	((Pc =< Pcruz,!,
        cruzar(Ind1,Ind2,P1,P2,NInd1),
	  cruzar(Ind2,Ind1,P1,P2,NInd2))
	;
	(NInd1=Ind1,NInd2=Ind2)),
	cruzamento(Rest,Rest1).

preencheh([],[]).

preencheh([_|R1],[h|R2]):-
	preencheh(R1,R2).


sublista(L1,I1,I2,L):-
	I1 < I2,!,
	sublista1(L1,I1,I2,L).

sublista(L1,I1,I2,L):-
	sublista1(L1,I2,I1,L).

sublista1([X|R1],1,1,[X|H]):-!,
	preencheh(R1,H).

sublista1([X|R1],1,N2,[X|R2]):-!,
	N3 is N2 - 1,
	sublista1(R1,1,N3,R2).

sublista1([_|R1],N1,N2,[h|R2]):-
	N3 is N1 - 1,
	N4 is N2 - 1,
	sublista1(R1,N3,N4,R2).

rotate_right(L,K,L1):-
	tasks(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).


elimina([],_,[]):-!.

elimina([X|R1],L,[X|R2]):-
	not(member(X,L)),!,
	elimina(R1,L,R2).

elimina([_|R1],L,R2):-
	elimina(R1,L,R2).

insere([],L,_,L):-!.
insere([X|R],L,N,L2):-
	tasks(T),
	((N>T,!,N1 is N mod T);N1 = N),
	insere1(X,N1,L,L1),
	N2 is N + 1,
	insere(R,L1,N2,L2).


insere1(X,1,L,[X|L]):-!.
insere1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insere1(X,N1,L,L1).

cruzar(Ind1,Ind2,P1,P2,NInd11):-
	sublista(Ind1,P1,P2,Sub1),
	tasks(NumT),
	R is NumT-P2,
	rotate_right(Ind2,R,Ind21),
	elimina(Ind21,Sub1,Sub2),
	P3 is P2 + 1,
	insere(Sub2,Sub1,P3,NInd1),
	eliminah(NInd1,NInd11).


eliminah([],[]).

eliminah([h|R1],R2):-!,
	eliminah(R1,R2).

eliminah([X|R1],[X|R2]):-
	eliminah(R1,R2).

mutacao([],[]).
mutacao([Ind|Rest],[NInd|Rest1]):-
	prob_mutacao(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutacao(Rest,Rest1).

mutacao1(Ind,NInd):-
	gerar_pontos_cruzamento(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).