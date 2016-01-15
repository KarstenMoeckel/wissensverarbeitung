% Autor: Christian Schuett
% Datum: 30.12.2015

:- module(ai, [
     updateTreeDepth/1,
     updateAIPlayer/1,
     nextAiMove/2,
     moveTreeOfPlayer/2
     ]).

:- use_module(searchNode).
:- use_module(searchTree).
:- use_module(minimax).
:- use_module(tree).

:- dynamic treeDepth/1.
:- dynamic aiPlayer/1.

buildSearchTree(Tree) :-
    treeDepth(Depth),
    aiPlayer(Player),
    initialSearchTree(Depth,Player,Tree).

updateTreeDepth(Depth):-
    retractall(treeDepth(_)),
    assert(treeDepth(Depth)).

updateAIPlayer(Player):-
    retractall(aiPlayer(_)),
    assert(aiPlayer(Player)).

findNodeByValue([],_,_) :- fail.
findNodeByValue([Node|Nodes], Value,FoundNode):-
    nodeData(Node,node(_,_,Value,_)),
    FoundNode = Node
    ;
    findNodeByValue(Nodes,Value,FoundNode).

moveTreeOfPlayer(Player,Tree) :-
   initialSearchTree(2,Player,Tree).

nextAiMove(Calls, Value):-
    buildSearchTree(Tree),
    not(tree:isLeaf(Tree)),
    valueOfNode(Tree,Value),
    nodeChildren(Tree,Children),
    findNodeByValue(Children,Value,Node),
    callsOfNode(Node,Calls).
