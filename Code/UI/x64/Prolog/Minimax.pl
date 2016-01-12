:- module(minimax, [
    isStrategy/2,
    miniMax/4,
    evaluateTree/1
]).

:- use_module(game).
:- use_module(search).
:- use_module(searchNode).
:- use_module(tree).
:- use_module(evaluation).
:- use_module(searchTree).
:- use_module(ai).

isStrategy(Color, Strategy) :-
    main:player(Color) -> Strategy = min
    ;
    Strategy = max.

evaluateTree(MaxDepth) :-
    ai:searchTree(Tree),
    search:nodesOfLevel(Tree, MaxDepth, Leaves),
    evaluateLeaves(Leaves, NewLeaves),
    replaceLevel(Leaves, NewLeaves),
    NewDepth is MaxDepth - 1,
    miniMax(NewDepth).

evaluateLeaves([], []).
evaluateLeaves([Leaf | Leaves], ResultList) :-
    searchNode:datasOfNode(Leaf, World, ViewColor, _, Calls),
    evaluation:valueOfGame(World, ViewColor, NewValue),
    NewChild = t(node(World, ViewColor, NewValue, Calls), []),
    tree:appendTree(_,node(World, ViewColor, NewValue, Calls), _, NewLeaf),
    evaluateLeaves(Leaves, TempResult),
    ResultList = [NewLeaf | TempResult].

replaceLevel([], []).
replaceLevel([OldLeaf | OldRest], [NewLeaf | NewRest]) :-
    tree:nodeData(OldLeaf, Data),
    ai:searchTree(Tree),
    tree:replaceSubTree(Data, NewLeaf, Tree, NewTree),
    searchTree:writeSearchTreeToFacts(NewTree),
    replaceLevel(OldRest, NewRest).

% 2 because 1 is root node.
miniMax(1).
miniMax(CurrentLevel) :-
    ai:searchTree(Tree),
    search:nodesOfLevel(Tree, CurrentLevel, Members),
    miniMax(_, Members),
    NxtLevel is CurrentLevel - 1,
    miniMax(NxtLevel).

miniMax(_, []).
miniMax(BestValue , [Node | RList]):-
    tree:nodeChildren(Node, Children),
    searchNode:turnOfNode(Node, Color),
    isStrategy(Color, Strategy),
    miniMax(Strategy, BestValue, Children, ReturnChild),
    searchNode:valueOfNode(ReturnChild, ReturnValue),
    registerValue(ReturnValue, Node),
    miniMax(BestValue, RList).

miniMax(_, BestChild, [], ReturnChild) :- ReturnChild = BestChild.
miniMax(Strategy, BestChild, [Child | RestChilds], ReturnChild) :-
    setIfUnset(Child,BestChild),
    bestChild(Strategy, BestChild, Child, Result),
    miniMax(Strategy, Result, RestChilds, ReturnChild).

setIfUnset(Var1, Var2) :-
    (var(Var2) ->
        Var2 = Var1
    ; true).

registerValue(NewValue, OldNode):-
    OldNode = t(node( World, ViewColor, _, Calls), ChildChilds),
    tree:nodeData(OldNode, OldData),
    NewNode = t( node(World, ViewColor, NewValue, Calls), ChildChilds),
    ai:searchTree(Tree),
    tree:replaceSubTree(OldData, NewNode, Tree, NewTree),
    searchTree:writeSearchTreeToFacts(NewTree).

bestChild(Strategy, OldChild, NewChild, BestChild) :-
    searchNode:valueOfNode(OldChild, OldVal),
    searchNode:valueOfNode(NewChild, NewVal),
    (
            OldVal == 'n/a' ->
                BestChild = NewChild
        ;
            Strategy == min ->
                (
                    NewVal < OldVal ->
                        BestChild = NewChild
                    ;
                        BestChild = OldChild
                )
        ;
            Strategy == max ->
                (
                    NewVal > OldVal ->
                        BestChild = NewChild
                    ;
                        BestChild = OldChild
                )
    ).
