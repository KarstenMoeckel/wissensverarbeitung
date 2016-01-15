/** <searchNode>

This module returns data from nodes.

@author Karsten Moeckel, Christian Schuett
*/

:- module(searchNode, [
        worldOfNode/2,
        turnOfNode/2,
        valueOfNode/2,
        callsOfNode/2,
        datasOfNode/5
    ]).

:- use_module(tree).

/**
 * Get the world from a node.
 * @param Node The treenode.
 * @return World All stones.
 */
worldOfNode(Node, World) :-
    tree:nodeData(Node, node(World, _, _, _)).

/**
 * Get the turn from a node.
 * @param Node The treenode.
 * @return Turn The player who has the turn.
 */
turnOfNode(Node, Turn) :-
    tree:nodeData(Node, node(_, Turn, _, _)).

/**
 * Get the value from a node.
 * @param Node The treenode.
 * @return Value The value of the node.
 */
valueOfNode(Node, Value) :-
    tree:nodeData(Node, node(_, _, Value, _)).

/**
 * Get the calls from a node.
 * @param Node The treenode.
 * @return Calls The Calls to move a stone.
 */
callsOfNode(Node, Calls) :-
    tree:nodeData(Node, node(_, _, _, Calls)).

/**
 * Get all data from a node separated.
 * @param Node The treenode.
 * @return World All stones.
 * @return Turn The player who has the turn.
 * @return Value The value of the node.
 * @return Calls The Calls to move a stone.
 */
datasOfNode(Node, World, Turn, Value, Calls) :-
    tree:nodeData(Node, node(World,Turn,Value,Calls)).
