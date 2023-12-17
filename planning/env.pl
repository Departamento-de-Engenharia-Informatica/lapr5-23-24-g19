
:- module(env, [
    loader_ttl/1,
    mdr_url/1,
    elev_cost/1,
    passage_cost/1,
    task_sequence_alg/1,
    num_gen/1,
    dim_pop/1,
    prob_cruz/1,
    prob_mut/1
]).


% 2 min long cache; adjust as needed
loader_ttl(120).

mdr_url(Url) :-
    getenv('MDR_PREFIX', Url), !;
    Url = 'http://localhost:4000/api'.
    % Url = 'http://34.231.14.154:4000/api'.

passage_cost(Cost) :-
    getenv('PASSAGE_COST', Cost), !;
    Cost is 5.

elev_cost(Cost) :-
    getenv('ELEV_COST', Cost), !;
    Cost is 30.

task_sequence_alg(Alg) :-
    getenv('TASK_SEQ_ALGORITHM', Alg), !;
    Alg = 'permtutations'.

time_limit(Time) :-
    getenv('TIME_LIMIT', Time), !;
    Time = 60.

    
num_gen(NG) :-
    NG = 6.
    
dim_pop(DP) :-
    DP = 4.

prob_cruz(PC) :-
    PC =50.

prob_mut(PM) :-
    PM = 50.

% vim: ft=prolog
