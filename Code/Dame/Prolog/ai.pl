/** <ai>

This module starts the generation of the searchtree.
With this tree the ai can decide its next move.

@autor: Christian Schuett, Karsten Moeckel
@date: 30.12.2015
 */
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

/**
 * Predicate to start the generation of the searchTree.
 * @return Tree the built tree.
 */
buildSearchTree(Tree) :-
    treeDepth(Depth),
    aiPlayer(Player),
    initialSearchTree(Depth,Player,Tree).

/**
 * Predicate to update the treeDepth in the knowledge base.
 * @param Depth The new depth.
 */
updateTreeDepth(Depth):-
    retractall(treeDepth(_)),
    assert(treeDepth(Depth)).

/**
 * Predicate to set any of the given players as the
 * ai player.
 * @param Player
 */
updateAIPlayer(Player):-
    retractall(aiPlayer(_)),
    assert(aiPlayer(Player)).

/**
 * Predicate to find a node with a specific value in
 * a list of Nodes.
 * @param Nodes List of Nodes.
 * @param Value The value to look for.
 * @return FoundNode The node with the given value.
 */
findNodeByValue([],_,_) :- fail.
findNodeByValue([Node|Nodes], Value,FoundNode):-
    nodeData(Node,node(_,_,Value,_)),
    FoundNode = Node
    ;
    findNodeByValue(Nodes,Value,FoundNode).

/**
 *
 */
moveTreeOfPlayer(Player,Tree) :-
   initialSearchTree(2,Player,Tree).

/**
 * Predicate to get the calls of the next ai move.
 * @return Calls The next moves.
 * @return Value The value of the Node.
 */
nextAiMove(Calls, Value):-
    buildSearchTree(Tree),
    not(tree:isLeaf(Tree)),
    valueOfNode(Tree,Value),
    nodeChildren(Tree,Children),
    findNodeByValue(Children,Value,Node),
    callsOfNode(Node,Calls).
