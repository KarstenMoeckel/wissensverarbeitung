:- module(minimax, [
    isStrategy/2,
    miniMax/4,
    evaluateLeaves/1
]).

:- use_module(game).
:- use_module(search).
:- use_module(searchNode).
:- use_module(tree).
:- use_module(evaluation).
:- use_module(searchTree).
:- use_module(ai).

isStrategy(CurrentTurn, Strategy) :-
    game:player(Player),
    CurrentTurn = Player -> Strategy = min
    ;
    Strategy = max.

% 2 because 1 is root node.
evaluateLeaves(2).
evaluateLeaves(CurrentLevel) :-
    ai:searchTree(Tree),
    NewDepth is CurrentLevel - 1,
    search:nodesOfLevel(Tree, NewDepth, Members),
    Members = [First|_],
    searchNode:turnOfNode(First, Player),
    isStrategy(Player, Strategy),
    iterateMembers(Strategy, _, Members, NewDepth),
    evaluateLeaves(NewDepth).

miniMax(_, BestChild, [], ReturnChild) :- ReturnChild = BestChild.
miniMax(Strategy, BestChild, [Child | RestChilds], ReturnChild) :-
    setIfUnset(Child,BestChild),
    bestChild(Strategy, BestChild, Child, Result),
    miniMax(Strategy, Result, RestChilds, ReturnChild).

setIfUnset(Var1, Var2) :-
    var(Var2) ->
        Var2 = Var1.

/*bestChildOf([], BestChild, _, ReturnChild):- ReturnChild = BestChild.
bestChildOf([CurrentChild|Rest], BestChild, Strategy, ReturnChild) :-
    setIfUnset(Child, BestChild),
    bestChild(Strategy, BestChild, CurrentChild, ResultChild),
    bestChildOf(Rest, ResultChild, Strategy, ReturnChild).*/

registerValue(NewValue,OldNode):-
    OldNode = t(node( World, ViewColor, _, Calls), ChildChilds),
    tree:nodeData(OldNode, OldData),
    NewNode = t( node(World, ViewColor, NewValue, Calls),ChildChilds),
    ai:searchTree(Tree),
    tree:replaceSubTree(OldData, NewNode, Tree, NewTree),
    searchTree:writeSearchTreeToFacts(NewTree).

iterateMembers(_,_,[], _).
iterateMembers(Strategy, BestValue , [Node|RList], CurrentDepth):-
    tree:nodeChildren(Node, Children),
    ai:treeDepth(Level),
    BottomLevel is Level - 1,
    (
        (
            CurrentDepth =  BottomLevel,
            eval(Strategy, BestValue, Children, ReturnValue)
        )
        ;
            miniMax(Strategy, BestValue, Children, ReturnChild),
            searchNode:valueOfNode(ReturnChild, ReturnValue)
    ),
    registerValue(ReturnValue, Node),
    iterateMembers(Strategy, BestValue, RList, CurrentDepth).

eval( _, BestChild, [], Return) :- searchNode:valueOfNode(BestChild, Return).
eval( Strategy, BestChild, [Child | RestChilds],Return) :-
    Child = t(node(World, ViewColor, _, Calls), ChildChilds),
    %tree:nodeData(Child, _),
    evaluation:valueOfGame(World, ViewColor, NewValue),
    NewChild = t(node(World, ViewColor, NewValue, Calls), ChildChilds),
    setIfUnset(Child, BestChild),
    bestChild(Strategy, BestChild, NewChild, ResultChild),
    tree:valueOfNode(ResultChild, ResultValue),
    registerValue(ResultValue, Child),
    eval(Strategy,ResultChild, RestChilds, Return).

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
