% vim: ft=prolog
:-consult('BC_Viagens').

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).

% Bibliotecas JSON
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% Inicialização dos valores necessários para começar a gerar solução
%P/Ex: Prob. Cruz, Prob. mutação etc
inicializa:-
    write('\33\[2J'), %limpar o terminal
    write('Numero de novas Geracoes: '),read(NG), 			
	(retract(geracoes(_));true), asserta(geracoes(NG)),
	prompt(_,''),
	write('Dimensao da Populacao: '),read(DP),
	(retract(populacao(_));true), asserta(populacao(DP)),
	write('Probabilidade de Cruzamento (%): '), read(P1),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), 	asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%): '), read(P2),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)),
    % NOTE: Nao esta nos slides
	write('Tempo limite de execucao (S): '), read(T),
	(retract(tempo_limite(_));true), asserta(tempo_limite(T)),
	write('Avaliacao Especifica: '), read(Av),
    (retract(avaliacao_especifica());true), asserta(avaliacao_especifica(Av)),
    write('Numero de Geracoes ate Estabilizacao: '), read(G),nl,
    (retract(estabilizacao());true), asserta(estabilizacao(G)),write('\33\[2J'),!. %limpar o terminal

inicializaCloud(NG,DP,PC,PM,T,Av,G):-	
	(retract(geracoes(_));true), asserta(geracoes(NG)),
	(retract(populacao(_));true), asserta(populacao(DP)),
	(retract(prob_cruzamento(_));true), asserta(prob_cruzamento(PC)),
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)),
	(retract(tempo_limite(_));true), asserta(tempo_limite(T)),
	(retract(avaliacao_especifica());true), asserta(avaliacao_especifica(Av)),
	(retract(estabilizacao());true), asserta(estabilizacao(G)),!.


%Res(Cost,Seq)
%Seq :- [t1,t2,t3]


gera:-
    %TODO: add inicializa??
	gera_populacao(Pop),
	avalia_populacao(Pop,PopAv),
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),

    %NOTA: nao esta nos slides
	tempo_limite(Tlim),
	avaliacao_especifica(AvEspecifica),
	estabilizacao(Gest),
	get_time(Ti),

	gera_geracao(0,NG,Ti,Tlim,PopOrd,AvEspecifica,Gest,0).

geraCloud(NG,DP,PC,PM,T,Av,G):-
	inicializaCloud(NG,DP,PC,PM,T,Av,G),
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	ordena_populacao(PopAv,PopOrd),
	write('PopAv='),write(PopOrd),nl,
	geracoes(NG),
	tempo_limite(Tlim),
	avaliacao_especifica(AvEspecifica),
	estabilizacao(Gest),
	get_time(Ti),
	gera_geracao(0,NG,Ti,Tlim,PopOrd,AvEspecifica,Gest,0),
	fittest(H*V),
    postSolution(H,V).


%TODO: APAGAR?
postSolution(H,V):-
    Term = json([drivers=H,score=V]),
    http_post('https://MDR_URL/geneticSolutions', json(Term), _, []).


gera_populacao(Pop):-
	populacao(TamPop),

    %??
	vd(VD),
	lista_motoristas_nworkblocks(VD,L),
	geraListaPopulacao(L,Lista),
	length(Lista,NumT),
	(retract(numero_worblocks(_));true), asserta(numero_worblocks(NumT)),
	calcula_nPermutacoes(NumT,L,NPermutacoes),
	(
		(NPermutacoes>=TamPop,gera_populacao(TamPop,L,Lista,NumT,Pop))
	;
		(retract(populacao(_)), 
		assert(populacao(NPermutacoes)), 
		gera_populacao(NPermutacoes,L,Lista,NumT,Pop))
	),!.
    %??


gera_populacao(0,_,_,_,[]):-!.

gera_populacao(TamPop,ListaMotNWorkblocks,ListaTarefas,NumT,[Ind|Resto]):-
	TamPop1 is TamPop-1,
	gera_populacao(TamPop1,ListaMotNWorkblocks,ListaTarefas,NumT,Resto),
	gera_novo_individuo(ListaTarefas,NumT,Resto,Ind),!.

gera_novo_individuo(ListaTarefas,NumT,Resto,Ind):-
	gera_individuo(ListaTarefas,NumT,IndTemp),
	(
		(member(IndTemp,Resto), gera_novo_individuo(ListaTarefas,NumT,Resto,IndTemp1), Ind = IndTemp1)
	;
		(Ind = IndTemp)
	),!.

% calcula_nPermutacoes(NumTotal,ListaMotNWorkblocks,NPermutacoes):-
% 	fatorial(NumTotal,FatorialNumTotal),
% 	fatorialRepetidos(ListaMotNWorkblocks,Valor),
% 	NPermutacoes is FatorialNumTotal/Valor,!.

% fatorial(0,1).
% fatorial(NumTotal,FatorialNumTotal):-
% 	NumTemp is NumTotal - 1,
% 	fatorial(NumTemp,FatorialNumTotalTemp),
% 	FatorialNumTotal is FatorialNumTotalTemp*NumTotal,!.

% fatorialRepetidos([],1).
% fatorialRepetidos([(_,Repetidos)|T],Valor):-
% 	fatorialRepetidos(T,ValorTemp),
% 	fatorial(Repetidos,ValorFatorialMot),
% 	Valor is ValorFatorialMot*ValorTemp,!.

% gera_populacao_heuristicas(0,_,_,[]):-!.
% gera_populacao_heuristicas(TamPop,ListaTarefas,NumT,[Ind|Resto]):-
% 	TamPop1 is TamPop-1,
% 	gera_populacao_heuristicas(TamPop1,ListaTarefas,NumT,Resto),
% 	heuristica_quatro_horas(ListaTarefas,NumT,Ind),
% 	not(member(Ind,Resto)),!.
	
% gera_populacao_heuristicas(TamPop,ListaTarefas,NumT,L):-
% 	gera_populacao_heuristicas(TamPop,ListaTarefas,NumT,L).

% heuristica_quatro_horas(ListaTarefas,NumT,Ind):-
% 	gera_individuo(ListaTarefas,NumT,IndTemp),
% 	avalia_quatro_horas(IndTemp,Pen),
% 	(
% 		(Pen==0, Ind = IndTemp)	
% 	;	
% 		(heuristica_quatro_horas(ListaTarefas,NumT,Ind))
% 	),!.

% avalia_quatro_horas(Seq,V):-
% 	vehicleduty(_,ListWorkblocks),
% 	criarAgenda(Seq,ListWorkblocks,Agenda),
% 	avalia_quatro_horas1(Agenda,V).

% avalia_quatro_horas1([],0).
% avalia_quatro_horas1(Agenda,V):-
% 	restricaoMaximoHorasSeguidas(Agenda,PenalizacaoHorasSeguidas),
% 	V is PenalizacaoHorasSeguidas*10,!.

% heuristica_preferencia_horarios(ListaTarefas,NumT,Ind):-
% 	gera_individuo(ListaTarefas,NumT,IndTemp),
% 	avalia_preferencia_horarios(IndTemp,Pen),
% 	(
% 		(Pen==0, Ind = IndTemp)	
% 	;	
% 		(heuristica_preferencia_horarios(ListaTarefas,NumT,Ind))
% 	),!.

% avalia_preferencia_horarios(Seq,V):-
% 	vehicleduty(_,ListWorkblocks),
% 	criarAgenda(Seq,ListWorkblocks,Agenda),
% 	avalia_preferencia_horarios1(Agenda,V).

% avalia_preferencia_horarios1([],0).
% avalia_preferencia_horarios1(Agenda,V):-
% 	preferenciaHorarios(Pref),
% 	restricaoHorarios(Agenda,Pref,V),!.


gera_individuo([G],1,[G]):-!.

gera_individuo(L,NumT,[G|Resto]):-
	NumTemp is NumT + 1,
	random(1,NumTemp,N),
	retira(N,L,G,NovaLista),
	NumT1 is NumT-1,
	gera_individuo(NovaLista,NumT1,Resto).

retira(1,[G|Resto],G,Resto).
retira(N,[G1|Resto],G,[G1|Resto1]):-
	N1 is N-1,
	retira(N1,Resto,G,Resto1).

%Avalia restrições e dá penalizações
avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Ind*V|Resto1]):-
	avalia(Ind,V),
	avalia_populacao(Resto,Resto1).

% criarAgenda([],[],[]).
% criarAgenda([Mot|T1],[Workblock|T2],Agenda):-
% 	workblock(Workblock,_,Inicio,Fim),
% 	criarAgenda(T1,T2,AgendaTemp),
% 	(
% 		(member((Fim,_,Mot),AgendaTemp),mergeHora(AgendaTemp,Inicio,Fim,Mot,Agenda))
% 	;
% 		(Agenda = [(Inicio,Fim,Mot)|AgendaTemp])
% 	),!.

% mergeHora([],_,_,_,_):-!,print("Nao foi possivel criar as agendas!"),nl.

% mergeHora([(Fim,OutroFim,Mot)|Tail],Inicio,Fim,Mot,Agenda):-
% 	Agenda = [(Inicio,OutroFim,Mot)|Tail],!.

% mergeHora([H|T],Inicio,Fim,Mot,Agenda):-
% 	mergeHora(T,Inicio,Fim,Mot,AgendaTemp),
% 	Agenda = [H|AgendaTemp],!.

% criarPausas([],_,[]).
% criarPausas([(Mot,_)|T1],Agenda,Pausas):-
% 	criarPausas(T1,Agenda,PausasTemp),
% 	getHorarios(Mot,Agenda,HorarioMot),
% 	getPausas(HorarioMot,Pausa),
% 	append(Pausa,PausasTemp,Pausas),!.

% getHorarios(_,[],[]).
% getHorarios(Mot,[(Inicio,Fim,Mot)|Tail],HorarioMot):-
% 	getHorarios(Mot,Tail,HorarioMotTemp),
% 	HorarioMot = [(Inicio,Fim,Mot)|HorarioMotTemp],!.
% getHorarios(Mot,[_|Tail],HorarioMot):-
% 	getHorarios(Mot,Tail,HorarioMot),!.

% getPausas([(Inicio,Fim,Mot)|T],Pausa):-
% 	getPausas([(Inicio,Fim,Mot)|T],UltimaHora,PausaTemp),
% 	Pausa = [(0,UltimaHora,Mot)|PausaTemp],!.
	
% getPausas([],86400,[]).
% getPausas([(Inicio,Fim,Mot)|T],UltimaHora,Pausa):-
% 	getPausas(T,UltimaHoraTemp,PausaTemp),
% 	Pausa = [(Fim,UltimaHoraTemp,Mot)|PausaTemp],
%	UltimaHora is Inicio,!.

avalia(Seq,V):-
	vd(VD),
	vehicleduty(VD,ListWorkblocks),
	length(ListWorkblocks,N),
	length(Seq,N1),
	(
		(N1<N, print("Erro aqui!"),nl)
	;	
		criarAgenda(Seq,ListWorkblocks,Agenda),
		lista_motoristas_nworkblocks(VD,ListaMot),
		criarPausas(ListaMot,Agenda,Pausas),
		avalia(Agenda,Pausas,V)
	),!.

avalia([],_,0).
avalia(Agenda,Pausas,V):-
	horasLimiteContratuais(_,Contr),
	preferenciaHorarios(Pref),
	%Hard Constraints
	restricaoOitoHoras(Agenda,[],PenalizacaoOitoHoras),
	restricaoAlmocoEJantar(Pausas,PenalizacaoAlmocoJantar),
	restricaoMaximoHorasSeguidas(Agenda,PenalizacaoHorasSeguidas),
	restricaoPausaMinima(Agenda,Pausas,PenalizacaoPausaNaoCumprida),
	restricaoHorarios(Agenda,Contr,PenalizacaoHorasNaoContratuais),
	%Soft Constraits
	restricaoHorarios(Agenda,Pref,PenalizacaoForaHorasPref),
	V is (PenalizacaoOitoHoras*10)+ (PenalizacaoAlmocoJantar*8) + (PenalizacaoHorasSeguidas*10) + 
		 (PenalizacaoPausaNaoCumprida*10) + (PenalizacaoHorasNaoContratuais*8) + PenalizacaoForaHorasPref,!.

restricaoHorarios([],_,0).
restricaoHorarios([(Inicio,Fim,Mot)|Tail],L,PenalizacaoHorarios):-
	restricaoHorarios(Tail,L,PenalizacaoHorariosResto),
	(
		(not(member((_,_,Mot),L)), PenalizacaoHorarios is PenalizacaoHorariosResto)
	;	
		(verifica_horarios_mot(L,Inicio,Fim,Mot,Penalizacao),
		PenalizacaoHorarios is PenalizacaoHorariosResto+Penalizacao)
	),!.


verifica_horarios_mot(L,Inicio,Fim,Mot,Penalizacao):-
	tempo_dentro_agenda(L,Inicio,Fim,Mot,Tempo),
	DuracaoAgenda is Fim-Inicio,
	(
		(DuracaoAgenda>Tempo,Penalizacao is DuracaoAgenda-Tempo)
	;	
		(Penalizacao is 0)
	),!.


tempo_dentro_agenda([],_,_,_,0).
tempo_dentro_agenda([(InicioCont,FimCont,Mot)|T],Inicio,Fim,Mot,Tempo):-
	tempo_dentro_agenda(T,Inicio,Fim,Mot,TempoTemp),
	(
		(InicioCont<Fim,FimCont>Inicio,
			(
            	(InicioCont<Inicio,InicioTemp is Inicio)
        	;
            	(InicioTemp is InicioCont)
        	),
        	(
            	(FimCont>Fim, FimTemp is Fim)
        	;
           		(FimTemp is FimCont)
			),
		Duracao is FimTemp-InicioTemp,
		Tempo is TempoTemp+Duracao)
	;
		(Tempo is TempoTemp)
	),!.
tempo_dentro_agenda([_|T],Inicio,Fim,Mot,Tempo):-
	tempo_dentro_agenda(T,Inicio,Fim,Mot,Tempo),!.

restricaoPausaMinima([],_,0).
restricaoPausaMinima([(Inicio,Fim,Mot)|Tail],Pausas,PenalizacaoPausaNaoCumprida):-
	restricaoPausaMinima(Tail,Pausas,PenalizacaoPausaNaoCumpridaResto),
	Dur is Fim - Inicio,
	(
		(Dur>=14400, nth0(_,Pausas,(Fim,FimDaPausa,Mot)),DurPausa is FimDaPausa-Fim, 
		(
			(DurPausa<3600, Pen is 3600-DurPausa,PenalizacaoPausaNaoCumprida is PenalizacaoPausaNaoCumpridaResto + Pen)
		;
			(PenalizacaoPausaNaoCumprida is PenalizacaoPausaNaoCumpridaResto)
		)
		)
	;
		(PenalizacaoPausaNaoCumprida is PenalizacaoPausaNaoCumpridaResto)
	),!.

restricaoMaximoHorasSeguidas([],0).
restricaoMaximoHorasSeguidas([(Inicio,Fim,_)|Tail],PenalizacaoHorasSeguidas):-
	restricaoMaximoHorasSeguidas(Tail,PenalizacaoHorasSeguidasResto),
	Dur is Fim - Inicio,
	(
		(Dur>14400, Pen is Dur-14400,PenalizacaoHorasSeguidas is PenalizacaoHorasSeguidasResto + Pen)
	;
		(PenalizacaoHorasSeguidas is PenalizacaoHorasSeguidasResto)
	),!.

restricaoAlmocoEJantar(Pausas,Penalizacao):-
	vd(VD),
	lista_motoristas_nworkblocks(VD,ListaMot),
	restricaoAlmoco(Pausas,ListaMot,PenAlmoco),
	restricaoJantar(Pausas,ListaMot,PenJantar),
	Penalizacao is PenAlmoco+PenJantar,!.
	
restricaoAlmoco(_,[],0).
restricaoAlmoco(Pausas,[(Mot,_)|Tail],PenAlmoco):-
	restricaoAlmoco(Pausas,Tail,PenAlmocoResto),
	getPausasMot(Mot,Pausas,ListaPausasMot),
	getPenMot(ListaPausasMot,39600,54000,Pen),
	PenAlmoco is PenAlmocoResto + Pen,!.

restricaoJantar(_,[],0).
restricaoJantar(Pausas,[(Mot,_)|Tail],PenJantar):-
	restricaoJantar(Pausas,Tail,PenJantarResto),
	getPausasMot(Mot,Pausas,ListaPausasMot),
	getPenMot(ListaPausasMot,64800,79200,Pen),
	PenJantar is PenJantarResto + Pen,!.

getPausasMot(_,[],[]).
getPausasMot(Mot,[(Inicio,Fim,Mot)|Tail],Pausas):-
	getPausasMot(Mot,Tail,PausasTemp),
	Pausas = [(Inicio,Fim,Mot)|PausasTemp],!.
getPausasMot(Mot,[_|Tail],Pausas):-
	getPausasMot(Mot,Tail,Pausas),!.

getPenMot([],_,_,3600).
getPenMot([(Inicio,Fim,_)|Tail],LimiteInferior,LimiteSuperior,Pen):-
	(
		(Inicio>LimiteSuperior,Fim<LimiteInferior,getPenMot(Tail,LimiteInferior,LimiteSuperior,Pen))
		;
		(
			(Inicio<LimiteInferior,InicioTemp is LimiteInferior)
		;
			(InicioTemp is Inicio)
		),
		(
			(Fim>LimiteSuperior,FimTemp is LimiteSuperior)
		;	
			(FimTemp is Fim)
		),
		DurPausa is FimTemp - InicioTemp,
		(
			(DurPausa > 3600, Pen is 0)
		;	
			(getPenMot(Tail,LimiteInferior,LimiteSuperior,PenTemp),
				(
					(PenTemp>3600-DurPausa,Pen is 3600-DurPausa)
				;
					(Pen is PenTemp)
				)
			)
		)
	),!.

restricaoOitoHoras([],[],0).

restricaoOitoHoras([],[(_,Dur)|T],SegundosExtra):-
	restricaoOitoHoras([],T,SegundosExtraTemp),
	(
		(Dur>28800,Diferenca is Dur - 28800, SegundosExtra is SegundosExtraTemp + Diferenca)
	;
		(SegundosExtra is SegundosExtraTemp)
	),!.

restricaoOitoHoras([(Inicio,Fim,Mot)|T],HorasPorMot,SegundosExtra):-
	Duracao is Fim-Inicio,
	(
		(member((Mot,Dur),HorasPorMot),
		DuracaoFinal is Duracao+Dur, 
		replaceHora(HorasPorMot,Mot,DuracaoFinal,HorasLista))
	;
		(HorasLista = [(Mot,Duracao)|HorasPorMot])
	),
	restricaoOitoHoras(T,HorasLista,SegundosExtra),!.

replaceHora([(Mot,_)|Tail],Mot,Dur,HorasLista):-
	HorasLista = [(Mot,Dur)|Tail],!.
replaceHora([H|Tail],Mot,Dur,HorasLista):-
	replaceHora(Tail,Mot,Dur,ListaTemp),
	HorasLista = [H|ListaTemp],!.

%Organiza população por ordem crescente de penalização
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
%------------------------------------------------------

gera_geracao(_,_,Ti,Tlim,Pop,_,_,_):-get_time(Tf),TPassado is Tf-Ti, TPassado>=Tlim,set_fittest(Pop),!.
gera_geracao(G,G,_,_,Pop,_,_,_):-!,set_fittest(Pop).
gera_geracao(_,_,_,_,[H1*P1|T1],AvEspecifica,_,_):- P1=<AvEspecifica,set_fittest([H1*P1|T1]),!.
gera_geracao(_,_,_,_,[H1*P1|T1],_,Gest,Gest):-set_fittest([H1*P1|T1]),!.

gera_geracao(N,G,Ti,Tlim,Pop,AvEspecifica,Gest,Count):-
	random_permutation(Pop,PermList),
	cruzamento(PermList,NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	append(Pop,NPopAv,PopTot),
	ordena_populacao(PopTot,[H*P|Tail]),
	nao_elitista(Tail,PopNaoElite),
	PopComMelhorDaGeracao = [H*P|PopNaoElite],
	kill_unworthy(PopComMelhorDaGeracao,PopFinal),
	ordena_populacao(PopFinal,PopFinalOrd),
	N1 is N+1,
	((compare(D,PopFinalOrd,Pop), D == (=), Count1 is Count + 1);Count1 is 0),
	gera_geracao(N1,G,Ti,Tlim,PopFinalOrd,AvEspecifica,Gest,Count1).

set_fittest([H*P|_]):-
    (retract(fittest());true), asserta(fittest(H*P)),!.

media_populacao([],Count,(Count,0)):-!.
media_populacao([_*P|T],Count,Media):-
	Count1 is Count + 1,
	media_populacao(T,Count1,(Countf,Sum)),
	(
		(Count == 0,Sum1 is Sum + P,Media is Sum1/Countf)	
	;
		(Sum1 is Sum + P,Media = (Countf,Sum1))
	),!.

gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	numero_worblocks(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);(P1=P21,P2=P11)).
gerar_pontos_cruzamento1(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

cruzamento([],[]).
cruzamento([Ind*_],[Ind]).
cruzamento([Ind1*_,Ind2*_|Resto],[NInd1,NInd2|Resto1]):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(Pcruz),random(0.0,1.0,Pc),
	((Pc =< Pcruz,!,
		cruzar(Ind1,Ind2,P1,P2,NInd1),
		cruzar(Ind2,Ind1,P1,P2,NInd2))
	;
	(NInd1=Ind1,NInd2=Ind2)),
	cruzamento(Resto,Resto1).

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
	numero_worblocks(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).

insere([],L,_,_,L):-!.
insere([X|R],L,N,Lista,L2):-
	numero_worblocks(T),
	((N>T,!,N1 is N mod T);N1 = N),
	nth0(_,Lista,(X,NVezes)),
	getNTimesInList(X,L,TimesInList),
	(
		(NVezes>TimesInList,
		insere1(X,N1,L,L1),
		N2 is N + 1,
		insere(R,L1,N2,Lista,L2))
	;
		(insere(R,L,N,Lista,L2))
	),!.
	
insere1(X,1,L,[X|L]):-!.
insere1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insere1(X,N1,L,L1).

getNTimesInList(_,[],0).
getNTimesInList(X,[X|T],N):-
	getNTimesInList(X,T,NTemp),
	N is NTemp + 1,!.
getNTimesInList(X,[_|T],N):-
	getNTimesInList(X,T,N),!.

cruzar(Ind1,Ind2,P1,P2,NInd11):-
	vd(VD),
	lista_motoristas_nworkblocks(VD,Lista),
	sublista(Ind1,P1,P2,Sub1),
	numero_worblocks(NumT),
	R is NumT-P2,
	rotate_right(Ind2,R,Ind21),
	P3 is P2 + 1,
	insere(Ind21,Sub1,P3,Lista,NInd1),
	eliminah(NInd1,NInd11),
	length(NInd11,Length2),
	(
		(Length2<NumT)
	;
		(!)
	).
	

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

elite(PopOrd,Elite,NaoElite):-
	populacao(NG),
	NPassa is NG*0.5,
	elite2(PopOrd,[],Elite,NaoElite,0,NPassa).

elite2(Pop,EliteTemp,Elite,NaoElite,Counter,NPassa):-
	Counter>=NPassa,reverse(EliteTemp,Elite),reverse(Pop,NaoElite),!.
elite2([H|T],EliteTemp,Elite,NaoElite,Counter,NPassa):-
	C1 is Counter +1,
	elite2(T,[H|EliteTemp],Elite,NaoElite,C1,NPassa).
	
nao_elitista(Pop,PopOrg):-
	nao_elitista2(Pop,[],[],PopSorte,PopAzar),
	organizarSorte(PopSorte,PopAzar,[],PopOrg).
	
nao_elitista2([],TempSorte,TempAzar,PopSorte,PopAzar):-reverse(TempSorte,PopSorte),reverse(TempAzar,PopAzar),!.
nao_elitista2([H*P|Tail],TempSorte,TempAzar,PopSorte,PopAzar):-
	random(0.0,1.0,ProbPassar),
	(ProbPassar=<0.1,nao_elitista2(Tail,[H*P|TempSorte],TempAzar,PopSorte,PopAzar),!);
	(nao_elitista2(Tail,TempSorte,[H*P|TempAzar],PopSorte,PopAzar),!).

organizarSorte([],[],Temp,PopOrg):- reverse(Temp,PopOrg),!.
organizarSorte([],[H*P|Tail],Temp,PopOrg):-
	organizarSorte([],Tail,[H*P|Temp],PopOrg).
organizarSorte([H*P|Tail],PopAzar,Temp,PopOrg):-
	organizarSorte(Tail,PopAzar,[H*P|Temp],PopOrg).

kill_unworthy(Pop,PopFinal):-
	populacao(NP),
	kill_unworthy2(Pop,NP,0,[],PopFinal).

kill_unworthy2([],_,_,PopTemp,PopTemp):-write('Perigo, especie em extincao!'),nl.

kill_unworthy2(_,NP,NP,PopTemp,PopFinal):-reverse(PopTemp,PopFinal),!.

kill_unworthy2([H|T],NP,N,PopTemp,PopFinal):-
	\+member(H,PopTemp),
	PT1 = [H|PopTemp],
	length(PT1,N1),
	(kill_unworthy2(T,NP,N1,PT1,PopFinal),!);
	(kill_unworthy2(T,NP,N,PopTemp,PopFinal)).

geraListaPopulacao([],[]).
geraListaPopulacao([(_,0)|T],Lista):-
	geraListaPopulacao(T,Lista),!.
geraListaPopulacao([(G,N)|T],[G|Resto]):-
	NTemp is N-1,
	geraListaPopulacao([(G,NTemp)|T],Resto),!.