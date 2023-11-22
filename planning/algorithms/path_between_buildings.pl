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


