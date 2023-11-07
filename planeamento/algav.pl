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


