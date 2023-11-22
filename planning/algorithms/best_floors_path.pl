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
