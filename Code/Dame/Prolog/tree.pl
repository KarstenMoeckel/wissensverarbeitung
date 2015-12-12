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
    depthSearch_1(Depth, node(_, 0, World, Player)).

% Check if depth is reached.
%
% For each stone of the current Player
%     call depthSearch_1
depthSearch_1(Depth, ParentNode) :-
    arg(3, ParentNode, World),
    arg(4, ParentNode, Player),
    Depth > 0,
    NewDepth is Depth - 1,
    getEnemy(Player, Enemy),
    subtract(World, [stone(_,Enemy,_)], PlayerStones),
    maplist(depthSearch_2(World, Player, NewDepth, ParentNode), PlayerStones).

% For each Possible Move
%     do the move and call depthSearch with the new list of stones.
%
% For each Possible Attack
%     do the attack and call depthSearch with the new list of stones.
depthSearch_2(World, Player, Depth, ParentNode, Stone) :-
    writeln(Stone),
    member(Stone, World),
    (
        %% Get all possible Moves
        findall(X, moveDirections(Stone, X), Relations),
        maplist(depthSearch_3(World, Player, ParentNode, Stone), Relations),

        %% Get all possible HitChances

        %% Get all leafes of the tree and start a new search
        findall(node(_, _, NodeWorld, NodePlayer),
                node(ParentNode, Value, NodeWorld, NodePlayer), Nodes),
        maplist(depthSearch_1(Depth), Nodes)
    ).

depthSearch_3(World, Player, ParentNode, Stone, Relation) :-
    validMove(Stone, Relation, Player, World),
    doMove(Stone, World, Relation, NewList),
    assertz(node(ParentNode, 0, NewList, Player)).

%% subtract removes occurence of stone from StoneList and writes the rest of
%% the list into NewList.
%% Add new Stone to NewList.
doMove(Stone, StoneList, Direction, NewList) :-
    Stone = stone(Field,Color,Type),
    subtract(StoneList, [Stone], TempList),
    hasRelation(Field,Field2,Direction),
    append(TempList, [stone(Field2, Color, Type)], NewList).
