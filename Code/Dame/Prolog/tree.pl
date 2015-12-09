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
    getEnemy(Player, Enemy),
    createStoneList(World),
    assertz(node(_, 0, World, Player, [])),

    % Start the depth search.
    depthSearch(Depth, World, Player, node(_, 0, World, Player, [])).

% Check if depth is reached.
%
% For each stone of the current Player
%     call depthSearch_1
depthSearch(Depth, World, Player, ParentNode) :-
    writeln('Tiefensuche, Tiefe: ' + Depth),
    Depth > 0,
    NewDepth is Depth - 1,
    maplist(depthSearch(World, Player, NewDepth, ParentNode),
                CurrentPlayerStones).

% For each Possible Move
%     do the move and call depthSearch with the new list of stones.
%
% For each Possible Attack
%     do the attack and call depthSearch with the new list of stones.
depthSearch(World, Player, Depth, ParentNode, Stone) :-
    member(Stone, CurrentPlayerStones),
    getEnemy(Player, Enemy),
    (
        %% check if stone can move
        moveDirections(Stone, topLeft),
        (
            validMove(Stone, topLeft, Player, World),
            doMove(Stone, World, topLeft, NewList),
            assertz(node(ParentNode, 0, World, Player, [])),
            depthSearch(Depth, World, Enemy, node(ParentNode, 0, NewList, Player, [])),
            fail
        )
        ;
        moveDirections(Stone, topRight),
        (
            validMove(Stone, topRight, Player, World),
            doMove(Stone, World, topRight, NewList),
            assertz(node(ParentNode, 0, World, Player, [])),
            depthSearch(Depth, World, Enemy, node(ParentNode, 0, NewList, Player, [])),
            fail
        )
        ;
        moveDirections(Stone, bottomLeft),
        (
            validMove(Stone, bottomLeft, Player, World),
            doMove(Stone, World, bottomLeft, NewList),
            assertz(node(ParentNode, 0, World, Player, [])),
            depthSearch(Depth, World, Enemy, node(ParentNode, 0, NewList, Player, [])),
            fail
        )
        ;
        moveDirections(Stone, bottomRight),
        (
            validMove(Stone, bottomRight, Player, World),
            doMove(Stone, World, bottomRight, NewList),
            assertz(node(ParentNode, 0, World, Player, [])),
            depthSearch(Depth, World, Enemy, node(ParentNode, 0, NewList, Player, [])),
            fail
        )
        %% Check if stone can hit
    ).

%% subtract removes occurence of stone from StoneList and writes the rest of
%% the list into NewList.
%% Add new Stone to NewList.
doMove(Stone, StoneList, Direction, NewList) :-
   Stone = stone(Field,Color,Type),
    subtract(StoneList, [Stone], TempList),
    hasRelation(Field,Field2,Direction),
    con(TempList, [stone(Field2, Color, Type)], NewList).

addChildNode(Node, NewChildnode) :-
   Node = node(Parent, Value,World,Player,ChildNodes),
   retract(Node),
   con(ChildNodes, [NewChildnode], NewList),
   assertz(node(Parent, Value, World, Player, NewList)).

%% Concatenate two lists.
con([],L1,L1).
con([X|Tail],L2,[X|Tail1]):-
    con(Tail,L2,Tail1).
