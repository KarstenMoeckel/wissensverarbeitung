% Autor: Christian Schuett, Karsten Möckel
% Datum: 11.01.2016
:- module(searchTree, [
    updateSearchTree/1,
    writeSearchTreeToFacts/1,
    initialSearchTree/2
    ]).

:- use_module(game).
:- use_module(tree).
:- use_module(search).
:- use_module(rulez).
:- use_module(simulation).
:- use_module(ai).

initialSearchTree(1, _).
initialSearchTree(Depth, CurrentNode) :-
    ai:searchTree(Tree),
    nodeData(CurrentNode, OldData),
    OldData = node(World, _, _, _),
    (
        rulez:isGameOver(World, _) ->
            initialSearchTree(1, _) %TODO: change to cancel precessing
            ;
            true
    ),
    simulation:appendSearchTree(CurrentNode, NewNode),
    replaceSubTree(OldData, NewNode, Tree, NewTree),
    writeSearchTreeToFacts(NewTree),
    ai:treeDepth(MaxLevel),
    NewDepth is Depth - 1,
    Level is MaxLevel - NewDepth + 1,
    search:nodesOfLevel(NewTree, Level, Children),
    maplist(initialSearchTree(NewDepth), Children).

writeSearchTreeToFacts(Tree) :-
    retractall(ai:searchTree(_)),
    assertz(ai:searchTree(Tree)).

%TODO: Calls als parameter
updateSearchTree(Calls):-
    ai:searchTree(Before),
    updateRoot(Calls),
    appendNewLeaves,
    ai:searchTree(After),
    writeln(Before),
    writeln(After).

appendNewLeaves():-
    ai:searchTree(OldTree),
    ai:treeDepth(Depth),
    %because the tree shrinked after the root was extirpated
    TargetDepth = Depth - 1,
    search:nodesOfLevel(OldTree, TargetDepth, Members),
    % 2 because we append just one level
    maplist(initialSearchTree(2), Members).

updateRoot(Calls) :-
    ai:searchTree(Tree),
    subTree(node(_,_,_,Calls), Tree, SubTree),
    writeSearchTreeToFacts(SubTree).

createRootNode(Player, RootNode) :-
    game:createStoneList(World),
    tree:appendTree(_, node(World, Player, 'n/a', []), _, RootNode).
