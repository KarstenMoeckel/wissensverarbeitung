% Autor: Christian Schuett
% Datum: 30.12.2015

:- module(simulation, [
     possibleMove/4
     ]).

:- use_module(search).
:- use_module(rulez).
:- use_module(tree).
:- use_module(board).

possibleMove(World,Player,NewWorld,Call) :-
    rulez:isEnemy(Player, Enemy),
    removeEnemyStones(World,Enemy, PlayerStones),
    member(Stone,PlayerStones),
    movesOfStone(World,Stone,NewWorld,Call).

movesOfStone(World,Stone, NewWorld, Calls) :-
    rulez:moveDirections(Stone,Direction),
    Stone = stone(Field,Color,Mode),
    rulez:isMoveValid(World,Stone,Direction,Destination),
    simulateMove(World,Stone,Destination,TmpWorld,Call),
    (
        board:isFieldBetween(Field,Destination,_) ->
          (
              TmpStone = stone(Destination,Color,Mode),
              rulez:canHit(TmpWorld,TmpStone,HitTree),
              not(tree:isLeaf(HitTree)) ->
                  search:longesPath(HitTree,_,[hit(TmpWorld,_) | Hits]),
                  createCallList(Hits,NewWorld,FurtherCalls),
                  Calls = [Call| FurtherCalls]
              ;
                  Calls = [Call],
                  NewWorld = TmpWorld
          )
        ;
        Calls = [Call],
        NewWorld = TmpWorld
    ).

createCallList([hit(World,Call)],World,[Call]).
createCallList([hit(_,Call)| Hits],NewWorld,[Call | Calls]) :-
    createCallList(Hits,NewWorld, Calls).

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
