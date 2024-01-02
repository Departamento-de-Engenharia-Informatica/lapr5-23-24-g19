:- module('tasks_cost',[generate_tasks/2,tasks_cost/3,robot/1]).
:- use_module('algorithms',[compute_path/4]).

% Define a dynamic predicate to store task costs
:- dynamic tasks_cost/3.
:- dynamic robot/1.

% Calculate and assert task costs
generate_tasks(Robot,List):-
	% inicializa o robot
	(retract(robot(_)),!;true),
    asserta(robot(Robot)),
    calculate_and_assert_task_costs(List,List).

calculate_and_assert_task_costs([],_).
calculate_and_assert_task_costs([Task|Rest],List) :-

    % calcular cada uma das sequencias Task-[lista total]
    calculate_task_cost(Task, List, Costs),

    %a ssert cada uma das sequencias Task-[lista total]
    assert_task_costs(Task, List, Costs),
    
    robot(Robot),
    calculate_assert_robot_task(Robot,Task),
    calculate_assert_task_robot(Robot,Task),

    % continuar a calcular para o resto das tarefas
    calculate_and_assert_task_costs(Rest,List).

calculate_task_cost(_, [], []).
    
calculate_task_cost(Task, [Other | Rest], [Cost | Costs]) :-
    compute_cost(Task, Other, Cost),
    calculate_task_cost(Task, Rest, Costs).

compute_cost(Task1, Task1, 0).
compute_cost(Task1, Task2, Cost) :-
    (_, end(B1, F1, X1, Y1), _) = Task1,
    (start(B2, F2, X2, Y2), _, _) = Task2,
    compute_path((B1, F1, X1, Y1), (B2, F2, X2, Y2), _, Cost), !.

% criar os factos para as combinações Task-{res}
assert_task_costs(_, [], []).
assert_task_costs(Task, [Task | Rest], [_ | Costs]) :-
    assert_task_costs(Task, Rest, Costs).

assert_task_costs(Task, [Other | Rest], [Cost | Costs]) :-
    assertz(tasks_cost(Task, Other, Cost)),
    assert_task_costs(Task, Rest, Costs).

calculate_assert_robot_task(Robot,Task) :-
    % (pos(B,F,X,Y), _) = Robot,
    (B,F,X,Y) = Robot,
    (start(B2, F2, X2, Y2), _, _) = Task,
    compute_path((B, F, X, Y), (B2, F2, X2, Y2), _, Cost), !,
    assertz(tasks_cost(Robot, Task, Cost)).

calculate_assert_task_robot(Robot,Task) :-
    % (pos(B,F,X,Y), _) = Robot,
    (B,F,X,Y) = Robot,
    (_, end(B2, F2, X2, Y2), _) = Task,
    compute_path((B2, F2, X2, Y2),(B, F, X, Y), _, Cost), !,
    assertz(tasks_cost(Task, Robot, Cost)).
