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
    assertz(node(_, 0, World, Player, [])),
    depthSearch(Depth, World, Player, node(_, 0, World, Player, [])).

% Check if depth is reached.
%
% For each stone of the current Player
%     call depthSearch_1
depthSearch(Depth, World, Player, ParentNode) :-
    writeln('Tiefensuche, Tiefe: ' + Depth),
    Depth >= 0,
    NewDepth is Depth - 1,
    %% subtract only the players stones
    getEnemy(Player, Enemy),
    subtract(World, [stone(_,Enemy,_)], PlayerStones),
    maplist(depthSearch(World, Player, NewDepth, ParentNode), PlayerStones).

% For each Possible Move
%     do the move and call depthSearch with the new list of stones.
%
% For each Possible Attack
%     do the attack and call depthSearch with the new list of stones.
depthSearch(World, Player, Depth, ParentNode, Stone) :-
    member(Stone, World),
    writeln(Stone),
    (
        findall(X, moveDirections(Stone, X), Relations),
        maplist(depthSearch(World, Player, Depth, ParentNode, Stone), Relations)
    ).

depthSearch(World, Player, Depth, ParentNode, Stone, Relation) :-
    writeln('stage 3: ' + Relation),
    validMove(Stone, Relation, Player, World),
    doMove(Stone, World, Relation, NewList),
    assertz(node(ParentNode, 0, World, Player, [])),
    writeln('Move ' + Relation),
    getEnemy(Player, Enemy).
    %depthSearch(Depth, World, Enemy, node(ParentNode, 0, NewList, Player, [])).

%% subtract removes occurence of stone from StoneList and writes the rest of
%% the list into NewList.
%% Add new Stone to NewList.
doMove(Stone, StoneList, Direction, NewList) :-
    Stone = stone(Field,Color,Type),
    subtract(StoneList, [Stone], TempList),
    hasRelation(Field,Field2,Direction),
    append(TempList, [stone(Field2, Color, Type)], NewList).

addChildNode(Node, NewChildnode) :-
   Node = node(Parent, Value,World,Player,ChildNodes),
   retract(Node),
   append(ChildNodes, [NewChildnode], NewList),
   assertz(node(Parent, Value, World, Player, NewList)).
