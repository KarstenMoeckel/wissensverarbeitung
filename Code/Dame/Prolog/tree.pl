%author Karsten Möckel
%date 07.12.2015

% Return opposite player of the color.
getEnemy(Color, Enemy) :-
   Color == white ->
      Enemy = black
   ;
   Color == black ->
      Enemy = white.

% Player: current player.
% Depth: Depth of the search.
createTree(Player, Depth) :-
    createStoneList(World),
    assertz(node(_, 0, World, Player)),
    depthSearch_1(Depth, node(_, 0, World, Player), Player).

% Check if depth is reached.
%
% For each stone of the current Player
%     call depthSearch_1
depthSearch_1(Depth, ParentNode, Player) :-
    (
        arg(3, ParentNode, World),
        Depth > 0,
        NewDepth is Depth - 1,
        getEnemy(Player, Enemy),
        subtract2(World, stone(_, Enemy, _), PlayerStones),
        maplist(depthSearch_2(World, Player, NewDepth, ParentNode),
            PlayerStones)
    )
    ;
    (
        true
    ).

% For each Possible Move
%     call depthSearch_3.
%
% For each Possible Attack
%     call depthSearch_4.
depthSearch_2(World, Player, Depth, ParentNode, Stone) :-
    member(Stone, World),
    (
        %% Get all possible Moves
        findall(X, moveDirections(Stone, X), Relations),
        maplist(depthSearch_3(World, Player, ParentNode, Stone, Depth),
            Relations)

        %% Get all possible HitChances
        %hasNeighbours(Stone, World, ListNeighbours),
        %writeln("3 Neighbours:\t" + ListNeighbours)
        %%maplist(depthSearch_4(World, Player, ParentNode, Stone, Depth),
        %%    ListNeighbours)
    ).

depthSearch_3(World, Player, ParentNode, Stone, Depth, Relation) :-
    arg(1, Stone, Field),
    validMove(stone(Field,_,_), Relation, Player, World),
    doMove(Stone, World, Relation, NewList),
    valueOfGame(NewList, Player, Value),
    assertz(node(ParentNode, Value, NewList, Player)),
    getEnemy(Player, Enemy),
    depthSearch_1(Depth, node(_, _, NewList, Player), Enemy).

depthSearch_4(World, Player, ParentNode, Stone, Depth, [Neighbour|Relation]) :-
    writeln("4 Neighbour:\t" + Neighbour + "\t" + Relation),
    canHit(World, Stone, Neighbour, Relation).

%% subtract removes occurence of stone from StoneList and writes the rest of
%% the list into NewList.
%% Add new Stone to NewList.
doMove(Stone, StoneList, Direction, NewList) :-
    Stone = stone(Field,Color,Type),
    subtract(StoneList, [Stone], TempList),
    hasRelation(Field,Field2,Direction),
    append(TempList, [stone(Field2, Color, Type)], NewList).
