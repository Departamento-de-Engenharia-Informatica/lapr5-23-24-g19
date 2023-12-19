:- module('genetic', [ gera/2 ]).

:- use_module('env', [ time_limit/1, num_gen/1, prob_mut/1, prob_cruz/1, dim_pop/1 ]).
:- use_module('task-sequencer/tasks_cost', [ generate_tasks/1,tasks_cost/3]).

:-dynamic geracoes/1.
:-dynamic populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.
%time wich start computing
:-dynamic start_time/1.
% tasks number
:-dynamic tasks/1.


inicializa_env:-
	num_gen(NG),
    (retract(geracoes(_));true), asserta(geracoes(NG)),

	dim_pop(DP),
	(retract(populacao(_));true), asserta(populacao(DP)),

	prob_cruz(P1),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), asserta(prob_cruzamento(PC)),

	prob_mut(P2),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).

update_start_limit :-
	get_time(CurrentTime),
    (retract(start_time(_)),!;true),
    asserta(start_time(CurrentTime)).


gera(TasksList,Res) :-
	% inicializa as variaveis necessarias
	inicializa_env,
	% incializa o nº de tasks
	length(TasksList,NumT),
	(retract(tasks(_)),!;true),
    asserta(tasks(NumT)),
	% gera os factos tasks_cost(T1,T2,Cost)
	generate_tasks(TasksList),
	% marcar o tempo incial em que o programa irá começar a computar a solução
    update_start_limit,
	% gera a primeira população
	gera_populacao(TasksList, Pop),
	% associa um valor a cada individuo da populacao
	avalia_populacao(Pop,PopAv),
	% associa a populacao de acordo com o valor de cada individuo
	ordena_populacao(PopAv, PopOrd),!,
	% verifca o numero de populacoes necessárias
	geracoes(NG),
	% gerar NG geracoes
	gera_geracao(0,NG,PopOrd,R),
	reverse(R,L),
	[H|_]= L,
	[Res|_] = H.
	% TODO: pass cost to a variable instead of *V
	% write_res(Res).

% gera_geracao
gera_geracao(G,G,_,[]):-!.

gera_geracao(N, G, Pop, [NextGen|Res]) :-

	% nl,write('Geracao'),write(N),nl,nl,

	start_time(StartTime),
	time_limit(TimeLimit),

	% TODO: CHECK, stop conditions:/  Terminar o programa se retornar verdadeiro??
	verifica_tempo_limite(StartTime, TimeLimit),

	% Permutação aleatória da população, para o cruzamento ser entre individuos diferentes
	random_permutation(Pop, PermutedPop),
	% cruzamento e mutação de acordo com o que foi fornecido em ALGAV
    cruzamento(PermutedPop, NPop1),
    mutacao(NPop1, NPop),
	% nl,write('PopMUTACAO'),write(NPop),nl,nl,

    % % Avalia e combina populações atual e nova
    avalia_populacao(NPop, NPopAv),
	% nl,write('Pop'),write(Pop),nl,nl,
	% nl,write('Av'),write(NPopAv),nl,nl,

    append(Pop, NPopAv, CombinedPop),

    ordena_populacao(CombinedPop, CombinedPopOrd),
	
	% nl,write('Append'),write(CombinedPopOrd),nl,nl,


    % % Seleção elitista : preserva os 20% melhores da população antiga
	% length(Pop,L),
    % P is round(0.3 *L),  % Exemplo: 30% de N
    % take_best(P, CombinedPopOrd, ElitistSelection),

	selecao_elitista(CombinedPopOrd,NPop1,ElitistSelection),

	selecao_nao_elitista(CombinedPopOrd,ElitistSelection,PermutedPop,NonElitistSelection),

    % % Formar a próxima geração
    append(ElitistSelection, NonElitistSelection, NextGen),

    N1 is N + 1,
	% nl,write('NextGen'),write(NextGen),nl,nl,

	
    gera_geracao(N1, G, NextGen,Res).


write_res([]).
write_res([X|Res]):-
	nl,write('Res= '),write(X),nl,nl,
	write_res(Res).

%CHECKED
gera_populacao(TasksList, Pop):-
	populacao(TamPop),
	length(TasksList,NumT),
	gera_populacao(TamPop,TasksList,NumT,Pop).


% gerar a população de acordo com as tarefas fornecidas
gera_populacao(0,_,_,[]):-!.

gera_populacao(TamPop,TasksList,NumT,[Ind|Rest]):-
	%tamanho população
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


% caso base: existe apenas 1 elemento na lista
avalia([_],0).

avalia([T,H |Rest], V):-
	tasks_cost(T,H,V1),
    avalia([H | Rest], VRest),
    V is V1 + VRest.

% FIX: returing multiple orders
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



selecao_elitista(CombinedPopOrd, Pop, ElitistSelection) :-
	length(Pop,L),
    P is round(0.6 * L),  % Exemplo: 30% de N
    take_best(P, CombinedPopOrd, ElitistSelection).

% retirar os primeiros N elementos
take_best(0, _, []).
take_best(N, [Ind | Rest], [Ind | Next]) :-
    N1 is N - 1,
    take_best(N1, Rest, Next).

% escolha random de um elemento
selecao_nao_elitista(CombinedPopOrd, ElitistSelection, Pop, NonElitistSelection) :-
    subtract(CombinedPopOrd, ElitistSelection, Remaining2),

	% clean tasks cost
	extract_elements(Remaining2,Remaining),

	% apply random value
    maplist(apply_random_factor, Remaining, RandomizedRemaining),

	% order with random value
    ordena_populacao(RandomizedRemaining, SortedRemaining2),

	% clean ordered list
	extract_elements(SortedRemaining2,SortedRemainingN),

	% restore tasks cost
	avalia_populacao(SortedRemainingN,SortedRemaining),
    length(ElitistSelection, ElitistCount),
	length(Pop,L),
    NP is  L - ElitistCount,
    take_best(NP, SortedRemaining, NonElitistSelection).


% Extract only elements
extract_elements([], []).
extract_elements([Element*_|T], [Element|ElementsTail]) :-
    extract_elements(T, ElementsTail).


apply_random_factor(Ind, Ind*Rand) :-
    random(0.0, 1.0, Rand).

verifica_tempo_limite(StartTime, TimeLimit) :-
    get_time(CurrentTime),
    ElapsedTime is CurrentTime - StartTime,
    ElapsedTime < TimeLimit.


% TODO:
% Estabilização da População: Encerrar o algoritmo se a população permanecer inalterada durante um número G de gerações consecutivas. A estabilização é mais provável em seleções puramente elitistas, mas é importante observar que mesmo populações estáveis podem sofrer alterações devido a cruzamentos e mutações diferentes.
% % Avaliar a população atual
% avalia_populacao(Pop, PopAv),
% ordena_populacao(PopAv, PopAvOrd),
% PopAvOrd = [BestInd*BestEval|_],
% % Verificar o limite de avaliação
% (BestEval =< EvalLimit -> (write('Solução com avaliação desejada encontrada.'), nl, !) ; true),
% % Verificar o limite de gerações
% (N < G -> true ; (write('Número máximo de gerações atingido.'), nl, !)),

% verifica_avaliacao_limite(Pop, EvalLimit) :-
%     avalia_populacao(Pop, PopAv),
%     ordena_populacao(PopAv, [BestInd*BestEval|_]),
%     BestEval =< EvalLimit.



%ALGAV

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



% combina dois progenitores gerando dois individuos novos
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

% facts for each cost between tasks
% tasks_cost(T1,T2,Cost)

% parameterizacao
% env variables
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

		
% gera:-
% 	inicializa,
% 	gera_populacao(Pop),
% 	write('Pop='),write(Pop),nl,
% 	avalia_populacao(Pop,PopAv),
% 	write('PopAvaliada='),write(PopAv),nl,
% 	ordena_populacao(PopAv,PopOrd),
% 	geracoes(NG),
% 	gera_geracao(0,NG,PopOrd).

% gera(TasksList):-
% 	inicializa_env,

% 	length(TasksList,NumT),
% 	(retract(tasks(_)),!;true),
%     asserta(tasks(NumT)),

% 	% initialize task-task-cost facts
% 	generate_tasks(TasksList),

% 	% set start time
%     update_start_limit,

% 	% gerar primeira populacao
% 	gera_populacao(TasksList, Pop),

% 	%avalia a primeira populacao
% 	% write('Pop='),write(Pop),nl,
% 	avalia_populacao(Pop,PopAv),
	
% 	% ordena a primeira populacao de acordo com o valor de avaliação
% 	% write('PopAvaliada='),write(PopAv),nl,
% 	ordena_populacao(PopAv,PopOrd),

% 	geracoes(NG),
%     update_start_limit,
% 	gera_geracao(0,NG,PopOrd).

% gera_geracao(G,G,Pop):-!,
% 	write('Geracao '), write(G), write(':'), nl, write(Pop), nl.

% gera_geracao(N, G, Pop) :-

% 	start_time(StartTime),
% 	time_limit(TimeLimit),
% 	%FIX: Terminar o programa se retornar verdadeiro
% 	verifica_tempo_limite(StartTime, TimeLimit),

% 	% TODO: CHECK, did not understand :/
% 	% Estabilização da População: Encerrar o algoritmo se a população permanecer inalterada durante um número G de gerações consecutivas. A estabilização é mais provável em seleções puramente elitistas, mas é importante observar que mesmo populações estáveis podem sofrer alterações devido a cruzamentos e mutações diferentes.
%     % % Avaliar a população atual
%     % avalia_populacao(Pop, PopAv),
%     % ordena_populacao(PopAv, PopAvOrd),
%     % PopAvOrd = [BestInd*BestEval|_],
%     % % Verificar o limite de avaliação
%     % (BestEval =< EvalLimit -> (write('Solução com avaliação desejada encontrada.'), nl, !) ; true),
%     % % Verificar o limite de gerações
%     % (N < G -> true ; (write('Número máximo de gerações atingido.'), nl, !)),

% 	% Permutação aleatória da população
	
% 	random_permutation(Pop, PermutedPop),
%     % Cruzar e mutar a população
% 	% combina dois progenitores gerando dois individuos novos
%     cruzamento(PermutedPop, NPop1),
%     mutacao(NPop1, NPop),

%     % % Avaliar e combina populações atual e nova
%     % avalia_populacao(Pop, PopAv),
%     % avalia_populacao(NPop, NPopAv),
%     % append(PopAv, NPopAv, CombinedPop),
%     % ordena_populacao(CombinedPop, CombinedPopOrd),
% 	avalia_e_combina_populacoes(PermutedPop,NPop,CombinedPopOrd),

%     % Seleção elitista : preserva os 20% melhores da população antiga
%     % P is round(0.3 * length(Pop)),  % Exemplo: 30% de N
%     % take_best(P, CombinedPopOrd, ElitistSelection),

% 	selecao_elitista(CombinedPopOrd,PermutedPop,ElitistSelection),

%     % % Preparar para seleção não-elitista
%     % subtract(CombinedPopOrd, ElitistSelection, Remaining),
%     % maplist(apply_random_factor, Remaining, RandomizedRemaining),
%     % ordena_populacao(RandomizedRemaining, SortedRemaining),

%     % % Seleção não-elitista
%     % length(ElitistSelection, ElitistCount),
%     % NP is length(Pop) - ElitistCount,
%     % take_best(NP, SortedRemaining, NonElitistSelection),
% 	selecao_nao_elitista(CombinedPopOrd,ElitistSelection,PermutedPop,NonElitistSelection),

% % Formar a próxima geração
% append(ElitistSelection, NonElitistSelection, NextGen),
% N1 is N + 1,
% gera_geracao(N1, G, NextGen).

