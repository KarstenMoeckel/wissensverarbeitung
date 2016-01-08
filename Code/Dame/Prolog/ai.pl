% Autor: Christian Schuett
% Datum: 30.12.2015

:- module(ai, [
     updateTreeDepth/1,
     buildInitialSearchTree/2
     ]).

:- use_module(simulation).
:- use_module(game).
:- use_module(tree).
:- use_module(search).
:- use_module(rulez).

:- dynamic searchTree/1.
:- dynamic treeDepth/1.
treeDepth(6).

initialSearchTree(0, _).
initialSearchTree(Depth, CurrentNode) :-
    searchTree(Tree),
    %TODO: need currentNode

    %JUST FOR YOU ROBERT
    (
        CurrentNode = node(Data, _, _, _),
        TmpTree = t(CurrentNode,[])
    ;
        CurrentNode = t(Data,_),
        TmpTree = CurrentNode
    ),

    TmpTree = t(node(World, _, _, _), _),

    (
        rulez:isGameOver(World, Winner) ->
            initialSearchTree(0, _)
            ;
            true
    ),

    simulation:appendSearchTree(TmpTree, NewNode),
    tree:nodeData(TmpTree, OldData),
    tree:replaceSubTree(OldData, NewNode,Tree,NewTree),

    retractall(searchTree(_)),
    assertz(searchTree(NewTree)),

    treeDepth(MaxLevel),

    NewDepth is Depth - 1,
    Level is MaxLevel - NewDepth + 1,
    search:membersOfLevel(NewTree,Level,Childs),

    maplist(initialSearchTree(NewDepth), Childs)
    .

buildInitialSearchTree(Player, Tree) :-
    retractall(searchTree(_)),
    treeDepth(Depth),
    game:createStoneList(World),
    tree:appendTree(_, node(World, Player, 'n/a', []), _, Root),
    assertz(searchTree(Root)),
    initialSearchTree(Depth, Root),
    searchTree(Tree),
    writeln(Tree).

updateTreeDepth(Depth):-
	retractall(treeDepth(_)),
	assertz(treeDepth(Depth)).


%minimax() :-
