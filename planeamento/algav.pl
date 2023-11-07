%Following the documents provided in ALGAV
%Plant according to campus ISEP

% 1- List of connections between buildings

connect(a,h).
connect(b,g).
connect(b,i).
connect(g,h).
connect(h,i).
connect(i,j).


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
% ------------------------------------------------------------------------------------
% ------------------------------------------------------------------------------------
