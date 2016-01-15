/** <ai>

This module generates the searchtree and evaluates the leaves and starts Min-Max.

@autor: Karsten Möeckel, Robert Maas
@date: 15.01.2016
 */
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

/**
 * Predicate to start generation of search tree
 * @param MaxDepth maximal depth of tree
 * @param StartPlayer player with wich the generation starts
 * @return Tree the searchTree
 */
initialSearchTree(MaxDepth,StartPlayer,Tree) :-
    game:createStoneList(World),
    initialSearchTree(1,MaxDepth,StartPlayer,World,StartPlayer,[],Tree).

/**
 * Predicate to generate the tree
 * @param CurDepth current depth in tree
 * @param MaxDepth maximal Depth of tree
 * @param AIPlayer Color of AI-player
 * @param World list of all stones
 * @param Player current player
 * @param Call move, which was done to come from previoues wrold to current world
 * @return Tree generated Tree
 */
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

createTreeNode(World,Player,Value,Call,Node):-
    tree:appendTree(_,node(World,Player,Value,Call),_, Node).
