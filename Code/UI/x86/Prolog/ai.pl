% Autor: Christian Schuett
% Datum: 30.12.2015

:- module(ai, [
     updateTreeDepth/1,
     buildInitialSearchTree/1,
     updateSearchTree/1,
     nextAiMove/1
     ]).

:- use_module(searchNode).
:- use_module(searchTree).
:- use_module(minimax).

:- dynamic searchTree/1.
:- dynamic treeDepth/1.

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

buildInitialSearchTree(Player) :-
    treeDepth(Depth),
    searchTree:createRootNode(Player, Root),
    searchTree:writeSearchTreeToFacts(Root),
    searchTree:initialSearchTree(Depth, Root),
    minimax:evaluateTree(Depth).

updateTreeDepth(Depth):-
    retractall(treeDepth(_)),
    retractall(searchTree(_)),
    assertz(treeDepth(Depth)).

updateSearchTree(Calls) :-
   updateRoot(Calls),
   appendNewLeaves,
   treeDepth(Depth),
   minimax:evaluateTree(Depth).

performAiMove([]).
performAiMove([ Call | RestCalls]) :-
    call(Call),
    performAiMove(RestCalls).

nextAiMove(Calls):-
    searchTree(Tree),
    searchNode:turnOfNode(Tree, Color),
    tree:nodeChildren(Tree, Children),
    minimax:isStrategy(Color, Strategy),
    %Strategy, BestChild, [Child | RestChilds], ReturnChild
    minimax:miniMax(Strategy, _, Children, BestChild),
    %minimax:bestChildOf(Children, _, Strategy, BestChild),
    searchNode:callsOfNode(BestChild, Calls).
