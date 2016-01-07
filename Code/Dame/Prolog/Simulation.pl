% Autor: Christian Schuett
% Datum: 30.12.2015

:- module(simulation, [
     appendSearchTree/2
     ]).

:- use_module(search).
:- use_module(rulez).
:- use_module(tree).
:- use_module(board).

:- dynamic searchTree/1.
:- dynamic moves/2.

appendSearchTree(Tree, NewTree):- %TreeData: World,CurPlayer,Value, [MoveCalls]
    tree:nodeData(Tree,node(World,Player,_,_)),

    rulez:isEnemy(Player, Enemy),
    removeEnemyStones(World, Player, EnemyStones),
    checkMovePossibilities(World,EnemyStones, PossibleMoves),
    appendPossiblitiesToTree(Tree,PossibleMoves,Enemy,NewTree).

appendPossiblitiesToTree(Tree,[],_,NewTree) :- Tree = NewTree.
appendPossiblitiesToTree(Tree,[moves(Calls,World)|Moves], Player,NewTree) :-
    tree:nodeData(Tree,Data),
    tree:appendTree(Data,node(World,Player,_,Calls),Tree,TmpTree),
    appendPossiblitiesToTree(TmpTree,Moves,Player,NewTree).

checkMoveStonePossibility(World,Stone,Call,NewWorld):-
    rulez:moveDirections(Stone,Direction),
    (
       Stone = stone(Field,_,_),
       board:hasRelation(Field,Destination,Direction),
       rulez:isMoveValid(World,Stone,Direction,Destination)->
          simulateMove(World,Stone,Destination,NewWorld,Call)
       ;
          true
    )
    ;
       fail.

checkMovePossibility(World,Stone):-
    (
       rulez:canHit(World,Stone,HitTree),
       not(tree:isLeaf(HitTree)),
       search:longesPath(HitTree,_,Path),
       Path = [Stone | Victims],
       simulateHits(World,Stone,Victims,NewWorld,Calls),
       assertz(moves(Calls,NewWorld)),
       fail
    )
    ;
    (
       repeat,
       (
          (
             checkMoveStonePossibility(World,Stone,Call,NewWorld),
             nonvar(Call),
             assertz(moves(Call,NewWorld)),
             fail
             ;
             !
          )
        )
    ).

checkMovePossibilities(_,[], []). %PossibleMoves: moves([MoveCalls], NewWorld)
checkMovePossibilities(World,[Stone|PlayerStones], PossibleMoves) :-
    checkMovePossibilities(World,PlayerStones, FoundMoves),
    retractall(moves(_,_)),
    checkMovePossibility(World,Stone),
    findall(moves(MCalls,MWorld),moves(MCalls,MWorld),MoveList),
    append(MoveList,FoundMoves,PossibleMoves).

simulateHits(World,Hitter,Victims, NewWorld,Calls) :-
    simulateHits(World,Hitter,Victims, [],NewWorld,TmpCalls),
    reverse(TmpCalls,Calls).

simulateHits(World,_,[], TmpCalls,NewWorld,Calls) :-
    NewWorld = World,
    Calls = TmpCalls.
simulateHits(World,Hitter, [Victim|Victims], CurCalls,NewWorld, Calls) :-
    Hitter = stone(HField,Color,Mode),
    Victim = stone(VField,_,_),
    board:isFieldBetween(HField,Destination,VField),
    simulateMove(World,Hitter,Destination,TmpWorld, Call),
    TmpCalls = [Call| CurCalls],
    simulateHits(TmpWorld,stone(Destination,Color,Mode),Victims,TmpCalls,NewWorld,Calls).

removeEnemyStones([], _, []).
removeEnemyStones([Stone| World], EnemyColor, NewList):-
    removeEnemyStones(World,EnemyColor,TmpList),
    Stone = stone(_,StoneColor,_),
    (
       StoneColor == EnemyColor ->
          NewList = TmpList
       ;
       NewList = [Stone | TmpList]
    ).

simulateMove(World, Stone,Destination,NewWorld, Call) :-
    subtract(World,[Stone],TmpWorld),
    Stone = stone(Source,Color,Mode),
    (
       rulez:canTransformIntoKing(Stone) ->
          Tmp2World = [stone(Destination,Color,king)| TmpWorld]
       ;
          Tmp2World = [stone(Destination,Color,Mode)| TmpWorld]
    ),
    (
       not(board:hasRelation(Source,Destination,_))->
           board:isFieldBetween(Source,Destination,Between),
           board:stoneAt(Tmp2World,Between,RemoveStone),
           subtract(Tmp2World,[RemoveStone],NewWorld)
       ;
          NewWorld = Tmp2World
    ),
    Call =.. [performMove,Source,Destination].
