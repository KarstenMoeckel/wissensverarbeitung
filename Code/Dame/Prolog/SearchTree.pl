% Autor: Christian Schuett, Karsten Möckel
% Datum: 11.01.2016
:- module(searchTree, [
    updateRoot/3,
    appendNewLevels/4,
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
    initialSearchTree(1,MaxDepth,World,StartPlayer,[],Tree).

initialSearchTree(MaxDepth,MaxDepth,World,Player,Call,Tree):-
    rulez:isEnemy(Player,Enemy),
    evaluation:valueOfGame(World,Enemy,Value),
    createTreeNode(World,Player,Value,Call,Tree),!.

initialSearchTree(CurDepth,MaxDepth,World,Player,Call,Tree) :-
    CurDepth < MaxDepth,
    rulez:isGameOver(World,WonPlayer),
    (
        WonPlayer == Player ->
            createTreeNode(World,Player,won,Call,Tree)
        ;
            createTreeNode(World,Player,lost,Call,Tree)
    ),!.

initialSearchTree(CurDepth,MaxDepth,World,Player,DoneCall,Tree) :-
    CurDepth < MaxDepth,
    not(rulez:isGameOver(World,_)),
    NextDepth is CurDepth + 1,
    rulez:isEnemy(Player,Enemy),
    repeat,
    (
            simulation:possibleMove(World,Player,NewWorld,PossibleCall),
            initialSearchTree(NextDepth,MaxDepth,NewWorld,Enemy,PossibleCall,Node),
            assert(childNodes(CurDepth,Node)),
            fail
        ;
            !
    ),
    Findall(Node,childNodes(CurDepth,Node),Nodes),
    retractall(childNodes(CurDepth,_)),
    minimax:miniMax(Value,Nodes),
    createTreeNode(World,Player,Value,DoneCall,TmpTree),
    tree:appendChildNodesToRootNode(Nodes,TmpTree,Tree).

appendNewLevels(MaxDepth, StartDepth, Tree, NewTree) :-
    search:nodesOfLevel(Tree,StartDepth,Nodes),
    createNewNodes(Nodes,StartDepth,MaxDepth,NewNodes),
    replaceTreeNodes(Nodes,NewNodes,Tree,NewTree).

replaceTreeNodes([],[],Tree,Tree).
replaceTreeNodes([OldNode|OldNodes], [NewNode|NewNodes], Tree, NewTree) :-
    tree:replaceSubTree(OldNode,NewNode,Tree,TmpTree),
    replaceTreeNodes(OldNodes,NewNodes,TmpTree,NewTree).

createNewNodes([],_,_,[]).
createNewNodes([OldNode| OldNodes],StartDepth, MaxDepth, [NewNode| NewNodes]):-
    createNewNodes(OldNodes,StartDepth,MaxDepth,NewNodes),
    searchNode:datasOfNode(OldNode,World, Turn, _, Calls),
    initialSearchTree(StartDepth,MaxDepth,World,Turn,Calls,NewNode).

updateRoot(Calls, CurTree, NewTree) :-
    subTree(node(_,_,_,Calls), CurTree, NewTree).

createTreeNode(World,Player,Value,Call,Node):-
    tree:appendTree(_,node(World,Player,Value,Call),_, Node).
