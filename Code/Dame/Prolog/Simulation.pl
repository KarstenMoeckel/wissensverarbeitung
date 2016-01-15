/** <simulation>

This module simulates moves for the ai.

@author Christian Schuett
@date 30.12.2015
*/

:- module(simulation, [
     possibleMove/4
     ]).

:- use_module(search).
:- use_module(rulez).
:- use_module(tree).
:- use_module(board).

/**
 * Predicate to find all possible moves for every stone.
 * @param World List of all stones.
 * @param Player The current player.
 * @return NewWorld The world after the move.
 * @return Call The calls to get the new world.
 */
possibleMove(World,Player,NewWorld,Call) :-
    rulez:isEnemy(Player, Enemy),
    removeEnemyStones(World,Enemy, PlayerStones),
    member(Stone,PlayerStones),
    movesOfStone(World,Stone,NewWorld,Call).

/**
 * Predicate to find all possible moves or hits of the stone and return
 * its calls. Checks all movedirections and hitpossibilities in rulz.
 * @param World The world to find the moves.
 * @param Stone The stone we try to find moves for.
 * @return NewWorld The world after the move with the stone is done.
 * @return Calls The calls that are necessary to create the new world.
 */
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

/**
 * Predicate to create a list with calls.
 */
createCallList([hit(World,Call)],World,[Call]).
createCallList([hit(_,Call)| Hits],NewWorld,[Call | Calls]) :-
    createCallList(Hits,NewWorld, Calls).

/**
 * Predicate to remove all stones of a given color.
 * @param World List of alle Stones.
 * @param EnemyColor Color of the enemy.
 * @return NewList The world without the enemy stones.
 */
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
