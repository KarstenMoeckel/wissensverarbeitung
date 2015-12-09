%author Karsten Möckel
%date 07.12.2015

% Return opposite player of the color.
getEnemy(Color, Return) :-
    (Color = white, Return = black, !)
    ;
    (Color = black, Return = white, !).

% Player: current player.
% Depth: Depth of the search.
createTree(Player, Depth) :-
    % get opposite player.
    getEnemy(Player, Enemy),

    % Find all Stones of the current player.
    findall(stone(Row,Col,Player,Type), stone(Row,Col,Player,Type), CurrentPlayerStones),

    % Find all Stones of the enemy player.
    findall(stone(Row,Col,Enemy,Type), stone(Row,Col,Enemy,Type), EnemyPlayerStones),

    %node(parent, value, world, player, childnodes[])
    % Create root node and add the root node to the knowledge base.
    con(CurrentPlayerStones, EnemyPlayerStones, World),
    writeln(World),
    assertz(node(_, 0, World, Player, [])),

    % Start the depth search.
    depthSearch(Depth, CurrentPlayerStones, EnemyPlayerStones, Player,
        node(_, 0, World, Player, [])).

% Check if depth is reached.
%
% For each stone of the current Player
%     call depthSearch_1
depthSearch(Depth, CurrentPlayerStones, EnemyPlayerStones, Player, ParentNode) :-
    writeln('Tiefensuche, Tiefe: ' + Depth),
    Depth > 0,
    NewDepth is Depth - 1,
    maplist(depthSearch(CurrentPlayerStones, EnemyPlayerStones, Player, NewDepth, ParentNode),
                CurrentPlayerStones).

% For each Possible Move
%     do the move and call depthSearch with the new list of stones.
%
% For each Possible Attack
%     do the attack and call depthSearch with the new list of stones.
depthSearch(CurrentPlayerStones, EnemyPlayerStones, Player, Depth, ParentNode, Stone) :-
    member(Stone, CurrentPlayerStones),
    getEnemy(Player, Enemy),
    (
        %% check if stone can move
        moveDirections(Stone, topLeft),
        (
            validMove(Stone, topLeft, Player, CurrentPlayerStones),
            doMove(Stone, CurrentPlayerStones, topLeft, NewList),
            writeln(Stone),
            con(NewList, EnemyPlayerStones, World),
            writeln(World),
            assertz(node(ParentNode, 0, World, Player, [])),
            depthSearch(Depth, EnemyPlayerStones, CurrentPlayerStones, Enemy,
                node(ParentNode, 0, World, Player, [])),
            fail
        )
        ;
        moveDirections(Stone, topRight),
        (
            validMove(Stone, topRight, Player, CurrentPlayerStones),
            doMove(Stone, CurrentPlayerStones, topRight, NewList),
            writeln(Stone),
            con(NewList, EnemyPlayerStones, World),
            writeln(World),
            assertz(node(ParentNode, 0, World, Player, [])),
            depthSearch(Depth, EnemyPlayerStones, CurrentPlayerStones, Enemy,
                node(ParentNode, 0, World, Player, [])),
            fail
        )
        ;
        moveDirections(Stone, bottomLeft),
        (
            validMove(Stone, bottomLeft, Player, CurrentPlayerStones),
            doMove(Stone, CurrentPlayerStones, bottomLeft, NewList),
            writeln(Stone),
            con(NewList, EnemyPlayerStones, World),
            writeln(World),
            assertz(node(ParentNode, 0, World, Player, [])),
            depthSearch(Depth, EnemyPlayerStones, CurrentPlayerStones, Enemy,
                node(ParentNode, 0, World, Player, [])),
            fail
        )
        ;
        moveDirections(Stone, bottomRight),
        (
            validMove(Stone, bottomRight, Player, CurrentPlayerStones),
            doMove(Stone, CurrentPlayerStones, bottomRight, NewList),
            writeln(Stone),
            con(NewList, EnemyPlayerStones, World),
            writeln(World),
            assertz(node(ParentNode, 0, World, Player, [])),
            depthSearch(Depth, EnemyPlayerStones, CurrentPlayerStones, Enemy,
                node(ParentNode, 0, World, Player, [])),
            fail
        )
        %% Check if stone can hit
    ).

%% subtract removes occurence of stone from StoneList and writes the rest of
%% the list into NewList.
%% Add new Stone to NewList.
doMove(Stone, StoneList, Direction, NewList) :-
    arg(1, Stone, Row1),
    arg(2, Stone, Col1),
    arg(3, Stone, Color),
    arg(4, Stone, Type),
    subtract(StoneList, [Stone], TempList),
    (
        Direction == topLeft,
            (
                moveTopLeft(Col1, Row1, Col2, Row2)
            )
        ;
        Direction == topRight,
            (
                moveTopRight(Col1, Row1, Col2, Row2)
            )
        ;
        Direction == bottomLeft,
            (
                moveBottomLeft(Col1, Row1, Col2, Row2)
            )
        ;
        Direction == bottomRight,
            (
                moveBottomRight(Col1, Row1, Col2, Row2)
            )
    ),
    con(TempList, [stone(Col2, Row2, Color, Type)], NewList).

addChildNode(Node, NewChildnode) :-
    arg(1, Node, Parent),
    arg(2, Node, Value),
    arg(3, Node, World),
    arg(4, Node, Player),
    arg(5, Node, Childnodes),
    retract(Node),
    con(Childnodes, [NewChildnode], NewList),
    assertz(node(Parent, Value, World, Player, NewList)).

%% Concatenate two lists.
con([],L1,L1).
con([X|Tail],L2,[X|Tail1]):-
    con(Tail,L2,Tail1).
