% Autor: Christian Schuett
% Datum: 30.12.2015

:- module(ai, [
     updateTreeDepth/1,
     buildInitialSearchTree/1,
     updateSearchTree/1
     ]).

:- use_module(tree).
:- use_module(simulation).
:- use_module(game).
:- use_module(search).
:- use_module(rulez).
:- use_module(evaluation).

:- dynamic searchTree/1.
:- dynamic treeDepth/1.

% TODO: remove performAiMove
%       treeDepth(4) löschen
%       Stabile Signaturen von Toplevelprädikaten für Aufruf -> Public
%       MinMax auslagern
%       Node auslagern (optional)
%       Warnungen beheben
%       Code Optimieren, writeln entfernen, Leerzeilen
%       Kommentieren
%       Search: membersOfLevel, nodesOfLevel mit Callbacks? (Optional)
%       Treezeugs aus tree entfernen, Aufrufen in ai abändern.
%       initialSearchTree mit abbruch
%       evaluation für Gewinner

treeDepth(4).

initialSearchTree(1, _).
initialSearchTree(Depth, CurrentNode) :-
    searchTree(Tree),

    nodeData(CurrentNode,OldData),
    OldData = node(World,_,_,_),


    (
        rulez:isGameOver(World, _) ->
            initialSearchTree(0, _) %TODO: change to cancel precessing
            ;
            true
    ),

    appendSearchTree(CurrentNode, NewNode),
    replaceSubTree(OldData, NewNode,Tree,NewTree),

    retractall(searchTree(_)),
    assertz(searchTree(NewTree)),

    treeDepth(MaxLevel),

    NewDepth is Depth - 1,
    Level is MaxLevel - NewDepth + 1,
    search:membersOfLevel(NewTree,Level,Childs),

    maplist(initialSearchTree(NewDepth), Childs) .

buildInitialSearchTree(Player) :-
    retractall(searchTree(_)),
    treeDepth(Depth),
    game:createStoneList(World),
    tree:appendTree(_, node(World, Player, 'n/a', []), _, Root),
    assertz(searchTree(Root)),
    initialSearchTree(Depth, Root)
    .



updateTreeDepth(Depth):-
	retractall(treeDepth(_)),
	assertz(treeDepth(Depth)).


worldOfNode(Node, World) :-
        nodeData(Node, node(World, _, _, _)).


isStrategy(CurrentTurn, Result) :-
    game:player(Player),
    CurrentTurn = Player -> Result = min
    ;
    Result = max.

% 2 because 1 is root node.
evaluateLeaves(2).
evaluateLeaves(CurrentLevel) :-
    searchTree(Tree),
    NewDepth is CurrentLevel - 1,
    search:nodesOfLevel(Tree, NewDepth, Members),
    Members = [First|_],
    tree:turnOfNode(First, Player),
    isStrategy(Player, Strategy),
    iterateMembers(Strategy, BestValue, Members, NewDepth),
    searchTree(CheckTree),
    evaluateLeaves(NewDepth).

miniMax(_,BestChild,[], ReturnValue) :- tree:valueOfNode(BestChild, ReturnValue).
miniMax(Strategy, BestChild, [Child | RestChilds], ReturnValue) :-
    (
        var(BestChild),
        BestChild = Child
        ; true
    ),

    bestChild(Strategy, BestChild, Child, Result),
    miniMax(Strategy, Result, RestChilds, ReturnValue).

%TODO: Calls als parameter
updateSearchTree(CurrentWorld):-
    searchTree(Before),
    updateRoot(CurrentWorld),
    appendNewLeaves,
    searchTree(After),
    writeln(Before),
    writeln(After).


appendNewLeaves():-
    searchTree(OldTree),
    treeDepth(Depth),
    %because the tree shrinked after the root was extirpated
    TargetDepth = Depth - 1,
    search:membersOfLevelTree(OldTree, TargetDepth, Members),
    % 2 because we append just one level
    maplist(initialSearchTree(2), Members).


updateRoot(DoneCalls) :-
    searchTree(OldTree),
    %game:createStoneList(CurrentWorld),
    cutRoot(CurrentWorld, OldTree, NewTree),
    retractall(searchTree(_)),
    assertz(searchTree(NewTree)).

cutRoot(Calls) :-
        searchTree(Tree),
        subTree(node(_,_,_,Calls),Tree,Subtree),
        retactall(searchTree(_)),
        assert(searchTree(SubTree)).

performAiMove([]).
performAiMove([ Call | RestCalls]) :-
    call(Call),
    performAiMove(RestCalls).

nextAiMove(Calls):-
    searchTree(Tree),
    Tree = t( node(_, Color, _, _), Children),
    isStrategy(Color,Strategy),
    bestChildOf(Children, _, Strategy, BestChild),
    tree:callsOfNode(BestChild, Calls).


bestChildOf([], BestChild, _, ReturnChild):- ReturnChild = BestChild.
bestChildOf([CurrentChild|Rest], BestChild, Strategy, ReturnChild) :-
    (
        var(BestChild),
        BestChild = CurrentChild
        ; true
    ),
    bestChild(Strategy, BestChild, CurrentChild, ResultChild),
    bestChildOf(Rest, ResultChild, Strategy, ReturnChild).

registerValue(NewValue,OldNode):-
    OldNode = t(node( World, ViewColor, _, Calls), ChildChilds),
    tree:nodeData(OldNode, OldData),

    NewNode = t( node(World, ViewColor, NewValue, Calls),ChildChilds),


    searchTree(Tree),
    tree:replaceSubTree(OldData, NewNode, Tree, NewTree),
    retractall(searchTree(_)),
    assertz(searchTree(NewTree)).



iterateMembers(_,_,[], _).
iterateMembers(Strategy, BestValue , [Item|RList], CurrentDepth):-
    Item = t(_,Childs),
    treeDepth(Level),
    BottomLevel is Level - 1,

    (
        (
            CurrentDepth =  BottomLevel,
            eval(Strategy, BestValue, Childs, Return)
        )
        ;
            miniMax(Strategy, BestValue, Childs, Return)
    ),

    registerValue(Return,Item),

    searchTree(CheckTree),
    iterateMembers(Strategy, BestValue, RList, CurrentDepth).


eval( _, BestChild, [],Return) :- tree:valueOfNode(BestChild, Return).
eval( Strategy, BestChild, [Child | RestChilds],Return) :-

    Child = t(node(World, ViewColor, OldValue, Calls), ChildChilds),

    tree:nodeData(Child, OldData),

    evaluation:valueOfGame(World, ViewColor, NewValue),

    NewChild = t(node(World, ViewColor, NewValue, Calls), ChildChilds),

    (
        var(BestChild),
        BestChild = Child
        ; true
    ),

    bestChild(Strategy, BestChild, NewChild, ResultChild),

    tree:valueOfNode(ResultChild, ResultValue),

    registerValue(ResultValue, Child),
    eval(Strategy,ResultChild, RestChilds, Return).


bestChild(Strategy, OldChild, NewChild, BestChild) :-
    tree:valueOfNode(OldChild, OldVal),
    tree:valueOfNode(NewChild, NewVal),
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
/*
compare(Strategy, OldVal, NewVal, Result) :-
    OldVal = 'n/a' ->
        Result = NewVal, !
    ;
    number(OldVal), number(NewVal),
    Strategy = min ->
    (
        (
            NewVal < OldVal,
            Result = NewVal, !
        )
        ;
        (
            NewVal >= OldVal,
            Result = OldVal, !
        )
    )
    ;
    Strategy = max ->
    (
        (
            NewVal > OldVal,
            Result = NewVal, !
        )
        ;
        (
            NewVal =< OldVal,
            Result = OldVal, !
        )
    )
    .
*/
