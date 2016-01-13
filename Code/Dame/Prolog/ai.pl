% Autor: Christian Schuett
% Datum: 30.12.2015

:- module(ai, [
     updateTreeDepth/1,
     updateAIPlayer/1,
     nextAiMove/1
     ]).

:- use_module(searchNode).
:- use_module(searchTree).
:- use_module(minimax).
:- use_module(tree).

:- dynamic treeDepth/1.
:- dynamic aiPlayer/1.

% TODO: remove performAiMove
%       treeDepth(4) löschen
%       Stabile Signaturen von Toplevelprädikaten für Aufruf -> Public
%       Warnungen beheben
%       Code Optimieren, writeln entfernen, Leerzeilen
%       Kommentieren
%       Search: membersOfLevel, nodesOfLevel mit Callbacks? (Optional)
%       initialSearchTree mit abbruch
%       evaluation für Gewinner
%       set für die node-eigenschaften(optional)

treeDepth(4).

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

performAiMove([]).
performAiMove([ Call | RestCalls]) :-
    call(Call),
    performAiMove(RestCalls).

findNodeByValue([],_,_) :- fail.
findNodeByValue([Node|Nodes], Value,FoundNode):-
    nodeData(Node,node(_,_,Value,_)),
    FoundNode = Node
    ;
    findNodeByValue(Nodes,Value,FoundNode).

nextAiMove(Calls):-
    buildSearchTree(Tree),
    valueOfNode(Tree,Value),
    nodeChildren(Tree,Children),
    findNodeByValue(Children,Value,Node),
    callsOfNode(Node,Calls).
