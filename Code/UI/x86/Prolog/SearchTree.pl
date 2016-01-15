% Autor: Christian Schuett, Karsten Möckel
% Datum: 11.01.2016
:- module(searchTree, [
    initialSearchTree/3
    ]).

:- use_module(game).
:- use_module(tree).
:- use_module(search).
:- use_module(rulez).
:- use_module(simulation).
:- use_module(evaluation).
:- use_module(ai).

:- dynamic childNodes/2.

initialSearchTree(MaxDepth,StartPlayer,Tree) :-
    game:createStoneList(World),
    initialSearchTree(1,MaxDepth,StartPlayer,World,StartPlayer,[],Tree).

initialSearchTree(MaxDepth,MaxDepth,AIPlayer,World,Player,Call,Tree):-
    rulez:isEnemy(Player,Enemy),
    evaluation:valueOfGame(World,AIPlayer,Value),
    createTreeNode(World,Player,Value,Call,Tree),!.

initialSearchTree(CurDepth,MaxDepth,AIPlayer,World,Player,Call,Tree) :-
    CurDepth < MaxDepth,
    rulez:isGameOver(World,WonPlayer),
    (
        WonPlayer == AIPlayer ->
            createTreeNode(World,Player,ai_won,Call,Tree)
        ;
            createTreeNode(World,Player,ai_lost,Call,Tree)
    ),!.

initialSearchTree(CurDepth,MaxDepth,AIPlayer,World,Player,DoneCall,Tree) :-
    CurDepth < MaxDepth,
    not(rulez:isGameOver(World,_)),
    NextDepth is CurDepth + 1,
    rulez:isEnemy(Player,Enemy),
    repeat,
    (
            simulation:possibleMove(World,Player,NewWorld,PossibleCall),
            initialSearchTree(NextDepth,MaxDepth,AIPlayer,NewWorld,Enemy,PossibleCall,Node),
            assert(childNodes(CurDepth,Node)),
            fail
        ;
            !
    ),
    findall(Node,childNodes(CurDepth,Node),Nodes),
    retractall(childNodes(CurDepth,_)),
    (
       Nodes == [] ->
          initialSearchTree(MaxDepth,MaxDepth,AIPlayer,World,Player,DoneCall,Tree) %No moves possible, Node is Leaf
       ;
       (
          minimax:miniMax(Value,Nodes),
          createTreeNode(World,Player,Value,DoneCall,TmpTree),
          tree:appendChildNodesToRootNode(Nodes,TmpTree,Tree)
       )
    ).

createNewNodes([],_,_,[]).
createNewNodes([OldNode| OldNodes],StartDepth, MaxDepth, [NewNode| NewNodes]):-
    createNewNodes(OldNodes,StartDepth,MaxDepth,NewNodes),
    searchNode:datasOfNode(OldNode,World, Turn, _, Calls),
    initialSearchTree(StartDepth,MaxDepth,World,Turn,Calls,NewNode).

createTreeNode(World,Player,Value,Call,Node):-
    tree:appendTree(_,node(World,Player,Value,Call),_, Node).
