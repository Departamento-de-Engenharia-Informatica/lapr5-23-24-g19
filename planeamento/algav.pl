%Following the documents provided in ALGAV
%Plant acpassageding to campus ISEP

% 1.1- List of connections between buildings

connect(a,h).
connect(b,g).
connect(b,i).
connect(g,h).
connect(h,i).
connect(i,j).

% 1.2 - The first argument is the building indication, and the second is the list of floors in the building

floors(a,[a1]).
floors(b,[b1,b2,b3,b4]).
floors(g,[g2,g3,g4]).
floors(h,[h1,h2,h3,h4]).
floors(i,[i1,i2,i3,i4]).
floors(j,[j1,j2,j3,j4]).

% 1.3 - The building indication in the first argument and the list of floors served by the elevator in the second

elevator(b,[b1,b2,b3,b4]).
elevator(g,[g2,g3,g4]).
elevator(i,[i1,i2,i3,i4]).
elevator(j,[j1,j2,j3,j4]).

% 1.4 - They have, in the first two arguments, the indication of the two buildings connected by a passage,
%       and in the last two arguments, the indication of the floors between which this passage connection is established

passage(a,h,a1,h2).
passage(b,g,b2,g2).
passage(b,g,b3,g3).
passage(b,i,b3,i3).
passage(g,h,g2,h2).
passage(g,h,g3,h3).
passage(h,i,h2,i2).
passage(i,j,i1,j1).
passage(i,j,i2,j2).
passage(i,j,i3,j3).




% 2 - List of paths between building of origin(BuildingOR) and building of destination(BuildingDest)
%     The method to be applied may be based on depth-first search(DFS) with backtracking
%     example: ?- path_between_buildings(j,a,LBuildingPath).


path_between_buildings(BuildingOr,BuildingDest,LBuildingPath):-
    path_between_buildings2(BuildingOr,BuildingDest,[BuildingOr],LBuildingPath).

path_between_buildings2(BuildingX,BuildingX,LBuildingInv,LBuildingPath):- !, reverse(LBuildingInv,LBuildingPath).

path_between_buildings2(BuildingAct,BuildingDest,LBuildingPassed,LBuildingPath):-
        (connect(BuildingAct,BuildingInt) ; connect(BuildingInt,BuildingAct)),
        \+member(BuildingInt,LBuildingPassed),
        path_between_buildings2(BuildingInt,BuildingDest,[BuildingInt|LBuildingPassed],LBuildingPath).




% 3 - List of ALL paths between building of origin(BuildingOR) and building of destination(BuildingDest)
%     LTPathsBuildings where each internal element of the list is a list with
%                      a possible path between the origin and destination buildings
%     example: ?- all_paths_between_buildings(i,j,LTPathsBuildings).
%
%
% all_paths_between_buildings(BuildingOr,BuildingDest,LTPathsBuildings):-
%                  findall(LBuildingPath,
%                         path_between_buildings(BuildingOr,BuildingDest,LBuildingPath),
%                         LTPathsBuildings).



% ------------------------------------------------------------------------------------
% -----------------------USER STORY 510-----------------------------------------------
% ------------------------------------------------------------------------------------
% To find paths between buildings that optimize a given criterion,
% such as passing through the smallest number of buildings.
% Using a variable to keep track of the length of the path and update it as we explore different paths.


% 4 - Find the shortest path between building of origin(BuildingOr) and building of destination(BuildingDest)
% example: ?- shortest_path_between_buildings(i, j, ShortestPath).


shortest_path_between_buildings(BuildingOr, BuildingDest, ShortestPath):-
        findall(LBuildingPath,
        path_between_buildings(BuildingOr, BuildingDest, LBuildingPath),
        AllPaths),
        find_shortest_path(AllPaths, ShortestPath).

find_shortest_path([Path|Paths], ShortestPath) :-
        length(Path, Len),
        find_shortest_path(Paths, ShortestPath, Path, Len).

find_shortest_path([], ShortestPath, ShortestPath, _).

find_shortest_path([Path|Paths], ShortestPath, CurrentShortestPath, MinLen) :-
            length(Path, Len),
            Len < MinLen,
            find_shortest_path(Paths, ShortestPath, Path, Len).

find_shortest_path([Path|Paths], ShortestPath, CurrentShortestPath, MinLen) :-
            length(Path, Len),
            Len >= MinLen,
            find_shortest_path(Paths, ShortestPath, CurrentShortestPath, MinLen).

% ------------------------------------------------------------------------------------
% -------------------------------------END FIRST PART---------------------------------
% ------------------------------------------------------------------------------------


% 5 - Finding a path between floors of buildings using passages and elevators.
% example : floors_path(j2,g4,LBuildingPath,LConnection).

floors_path(FloorOr,FloorDest,LBuildingPath,LConnection):-
            floors(BuildingOr,LFloorsOr),
            member(FloorOr,LFloorsOr),
            floors(BuildingDest,LFloorsDest),
            member(FloorDest,LFloorsDest),
            path_between_buildings(BuildingOr,BuildingDest,LBuildingPath),
            follow_floors(FloorOr,FloorDest,LBuildingPath,LConnection).

follow_floors(FloorDest,FloorDest,_,[]).

follow_floors(FloorDest1,FloorDest,[BuildingDest],[elev(FloorDest1,FloorDest)]):-
                FloorDest\==FloorDest1,
                elevator(BuildingDest,LFloors),
                member(FloorDest1,LFloors),
                member(FloorDest,LFloors).

follow_floors(CurrentFloor,FloorDest,[CurrentBuilding,SecBuilding|LOthersBuilding],[passag(CurrentFloor,SecFloor)|LOthersConnections]):-
            (passage(CurrentBuilding,SecBuilding,CurrentFloor,SecFloor) ; passage(SecBuilding,CurrentBuilding,SecFloor,CurrentFloor)),
            follow_floors(SecFloor,FloorDest,[SecBuilding|LOthersBuilding],LOthersConnections).

follow_floors(CurrentFloor,FloorDest,[CurrentBuilding,SecBuilding|LOthersBuilding],[elev(CurrentFloor,CurrentFloor1),passag(CurrentFloor1,SecFloor)|LOthersConnections]):-
            (passage(CurrentBuilding,SecBuilding,CurrentFloor1,SecFloor) ; passage(SecBuilding,CurrentBuilding,SecFloor,CurrentFloor1)),
            CurrentFloor1\==CurrentFloor,
            elevator(CurrentBuilding,LFloors),
            member(CurrentFloor,LFloors),
            member(CurrentFloor1,LFloors),
            follow_floors(SecFloor,FloorDest,[SecBuilding|LOthersBuilding],LOthersConnections).

% ------------------------------------------------------------------------------------
% -----------------------USER STORY 510-----------------------------------------------
% ------------------------------------------------------------------------------------
% To find paths between buildings that optimize a given criterion,
% such as minimize the number of elevator uses.
% If a path has fewer elevators or the same number of elevators but fewer passages, it will be selected as the best path.

% 6- Select the path that involves fewer elevator uses, and in case of a tie, less use of passages, fewer segments
% example: best_floors_path(j2,g4,LBestConnection).

best_floors_path(FloorOr,FloorDest,LBestConnection):-
                findall(LConnection,floors_path(FloorOr,FloorDest,_,LConnection),LLConnection),
                less_elevators(LLConnection,LBestConnection,_,_).

less_elevators([LConnection],LConnection,NElev,NPassage):-
                 counter(LConnection,NElev,NPassage).


less_elevators([LConnection|OthersLConnection],LConnectionR,NElevR,NPassageR):-
                less_elevators(OthersLConnection,LConnectionM,NElev,NPassage),
                counter(LConnection,NElev1,NPassage1),
                (((NElev1<NElev; (NElev1==NElev,NPassage1<NPassage)),
                !,
                NElevR is NElev1, NPassageR is NPassage1,LConnectionR=LConnection);
                (NElevR is NElev,NPassageR is NPassage,LConnectionR=LConnectionM)).

counter([],0,0).

counter([elev(_,_)|L],NElev,NPassage):-
    counter(L,NElevL,NPassage),
    NElev is NElevL+1.

counter([passag(_,_)|L],NElev,NPassage):-
    counter(L,NElev,NPassageL),
    NPassage is NPassageL+1.


% ------------------------------------------------------------------------------------
% -------------------------------------END SECOND PART--------------------------------
% ------------------------------------------------------------------------------------

% 7- We will represent a portion of the surface, for example, the 3rd floor of building B,
%   using a matrix of cells, where 0 indicates that the robot can occupy the cell and 1 indicates an obstacle.
%   We can assume that these cells are square

%m(col,lin,valor)
m(1,1,1).
m(2,1,1).
m(3,1,1).
m(4,1,1).
m(5,1,1).
m(6,1,1).
m(7,1,1).
m(8,1,1).
m(1,2,0).
m(2,2,0).
m(3,2,0).
m(4,2,0).
m(5,2,0).
m(6,2,0).
m(7,2,0).
m(8,2,1).
m(1,3,0).
m(2,3,0).
m(3,3,0).
m(4,3,0).
m(5,3,0).
m(6,3,0).
m(7,3,0).
m(8,3,1).
m(1,4,0).
m(2,4,0).
m(3,4,0).
m(4,4,0).
m(5,4,0).
m(6,4,0).
m(7,4,0).
m(8,4,1).
m(1,5,1).
m(2,5,1).
m(3,5,1).
m(4,5,1).
m(5,5,0).
m(6,5,0).
m(7,5,0).
m(8,5,1).
m(1,6,1).
m(2,6,1).
m(3,6,1).
m(4,6,1).
m(5,6,0).
m(6,6,0).
m(7,6,0).
m(8,6,1).
m(1,7,1).
m(2,7,1).
m(3,7,1).
m(4,7,1).
m(5,7,0).
m(6,7,0).
m(7,7,0).
m(8,7,1)

% 8- CREATE GRAPH
% example: ?- create_graph(8,7).
% example: ?- ligacel(A,B)

:-dynamic ligacel/2.
create_graph(_,0):-!.

create_graph(Col,Lin):-
    create_graph_lin(Col,Lin),
    Lin1 is Lin-1,
    create_graph(Col,Lin1).

create_graph_lin(0,_):-!.

create_graph_lin(Col,Lin):-m(Col,Lin,0),
    !,
    ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,
    ((m(ColS,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColS,Lin)));true)),
    ((m(ColA,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColA,Lin)));true)),
    ((m(Col,LinS,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinS)));true)),
    ((m(Col,LinA,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinA)));true)),
    Col1 is Col-1,
    create_graph_lin(Col1,Lin).

create_graph_lin(Col,Lin):-
    Col1 is Col-1,
    create_graph_lin(Col1,Lin).


% 9- All possible paths inside floor
% ?- findall(_,ligacel(_,_),L),length(L,Length).


% 10- Find solutions to go from cell(x,y) to cell(x,y) using DFS
% example: ?- dfs(cel(1,3),cel(6,7),L).

dfs(Orig,Dest,Path):-
dfs2(Orig,Dest,[Orig],Path).

dfs2(Dest,Dest,LA,Path):-
reverse(LA,Path).

dfs2(Act,Dest,LA,Path):-ligacel(Act,X),\+ member(X,LA),
dfs2(X,Dest,[X|LA],Path).


% 11- We will also try to count the number of solutions by generating all of them into a list and then observing the size of the list.
% ?- all_dfs(cel(1,3),cel(6,7),L),length(L,Length).

all_dfs(Orig,Dest,LPath):-findall(Path,dfs(Orig,Dest,Path),LPath).


% 12- To find out the best solution, that is, the solution (or one of the solutions) with the fewest number of steps
% ?- better_dfs(cel(1,3),cel(6,7),L).

better_dfs(Orig,Dest,Path):-all_dfs(Orig,Dest,LPath), shortlist(LPath,Path,_).

shortlist([L],L,N):-!,length(L,N).

shortlist([L|LL],Lm,Nm):-shortlist(LL,Lm1,Nm1),
        length(L,NL),
        ((NL<Nm1,!,Lm=L,Nm is NL);(Lm=Lm1,Nm is Nm1)).


% 13- There are several other solutions with 10 elements; this will be easier to see using the Breadth-First Search(BFS)
% example: ?- bfs(cel(1,3),cel(6,7),L).

bfs(Orig,Dest,Path):-
    bfs2(Dest,[[Orig]],Path).

bfs2(Dest,[[Dest|T]|_],Path):-
    reverse([Dest|T],Path).

bfs2(Dest,[LA|Others],Path):-
    LA=[Act|_],
    findall([X|LA],
        (Dest\==Act,ligacel(Act,X),\+ member(X,LA)),
        News),
    append(Others,News,All),
    bfs2(Dest,All,Path).

% ------------------------------------------------------------------------------------
% -----------------------USER STORY 510-----------------------------------------------
% ------------------------------------------------------------------------------------

%Connect what was covered in the previous class with what was presented in this class.

% In the previous class, we only discussed connections between buildings
%    and floors using external linking passages or elevators.

% Now, for each internal floor, it should be possible to reference rooms and offices, the access point to the elevator,
%    and the access points to external linking passages that connect buildings.
%    Note that a matrix will be required for each floor of the building.

% Each of these references will be in a cell cel(Column, Row), with positions in front of the doors to access rooms and offices,
%    the elevator, and access to the external linking passages.

% We will have a matrix of cells for each floor and building
%    (it is necessary to differentiate them; for example, facts like m/3 and ligacel/2 will require additional arguments).

% Include diagonal movements. For example, a diagonal movement between neighboring cells cel(i, j) and cel(i+1, j+1),
%    both with a value of 0, can only be made if cel(i, j+1) and cel(i+1, j) are also set to 0.

% A movement in the horizontal or vertical direction counts as 1 unit, but a diagonal movement has a weight of sqrt(2)
%    compared to the unit used for horizontal and vertical movements.


% Example floor layout:
% 1 1 1 1 1 1 1 1
% 0 0 0 0 0 0 0 1
% 0 0 0 0 0 0 0 1
% 0 0 0 0 0 0 0 1
% 1 1 1 1 0 0 0 1
% 1 1 1 1 0 0 0 1
% 1 1 1 1 0 0 0 1

% Define dynamic predicates
:- dynamic grid/1.
:- dynamic access_point/1.


assert(grid([[1, 1, 1, 1, 1, 1, 1, 1],
             [0, 0, 0, 0, 0, 0, 0, 1],
             [0, 0, 0, 0, 0, 0, 0, 1],
             [0, 0, 0, 0, 0, 0, 0, 1],
             [1, 1, 1, 1, 0, 0, 0, 1],
             [1, 1, 1, 1, 0, 0, 0, 1],
             [1, 1, 1, 1, 0, 1, 0, 1]])).

% Define dynamic predicates
:- dynamic grid/1.
:- dynamic access_point/1.

% Define the grid
grid([
    [1, 1, 1, 1, 1, 1, 1, 1],
    [0, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 1],
    [1, 1, 1, 1, 0, 0, 0, 1],
    [1, 1, 1, 1, 0, 0, 0, 1],
    [1, 1, 1, 1, 0, 1, 0, 1]
]).

% Define access points
access_point(cell(6, 7)).  % Access point between buildings.
access_point(cell(1, 3)).  % Example: Access point between buildings.
access_point(cell(3, 5)).  % Access point to rooms.
access_point(cell(7, 2)).  % Access point to the elevator.

% Define the rules for movement
can_move(X/Y, X1/Y1) :-
    grid(Grid),
    nth0(X, Grid, Row),
    nth0(X1, Grid, Row1),
    element(Y, Row, 0),
    element(Y1, Row1, 0).

element(N, List, Elem) :- nth0(N, List, Elem).

% Define DFS for pathfinding with access points
dfst(Start, End, Path) :- dfst(Start, End, [], Path).

dfst(End, End, Visited, [End|Visited]).
dfst(Start, End, Visited, Path) :-
    can_move(Start, Next),
    \+ member(Next, Visited),
    (
        access_point(Next),
        Path = [Next|Visited]
        ;
        dfst(Next, End, [Start|Visited], Path)
    ).

% Define a predicate to find a path
find_path(Start, End, Path) :- dfst(Start, End, [], Path).

% Example usage:
% ?- find_path(cell(0, 0), cell(4, 4), Path).

% ------------------------------------------------------------------------------------
% -------------------------------------END THIRD PART--------------------------------
% ------------------------------------------------------------------------------------

