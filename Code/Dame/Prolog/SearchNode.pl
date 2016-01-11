:- module(searchNode, [
        worldOfNode/2,
        turnOfNode/2,
        valueOfNode/2,
        callsOfNode/2,
        datasOfNode/5
    ]).

:- use_module(tree).

worldOfNode(Node, World) :-
    tree:nodeData(Node, node(World, _, _, _)).

turnOfNode(Node, Turn) :-
    tree:nodeData(Node, node(_, Turn, _, _)).

valueOfNode(Node, Value) :-
    tree:nodeData(Node, node(_, _, Value, _)).

callsOfNode(Node, Calls) :-
    tree:nodeData(Node, node(_, _, _, Calls)).

datasOfNode(Node, World, Turn, Value, Calls) :- tree:nodeData(Node, node(World,Turn,Value,Calls)).
