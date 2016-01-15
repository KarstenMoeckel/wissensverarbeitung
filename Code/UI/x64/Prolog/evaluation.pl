/** <evaluation>

Module to evaluate a world from the viewpoint of a specific player.


@author Robert Maas
@date 22.12.2015
*/

:- module(evaluation,[
      valueOfGame/3 %call: +World, +ViewColor, -Result
      ]).

:- use_module(board).
:- use_module(game).
:- use_module(rulez).
:- use_module(search).

:- dynamic hittenStone/1.

%evalValue(StoneType, Position, Value)
/**
 * Predicate to evaluate stones depending on their position
 * and hit posibility. Bonus if unhittable or in position to hit.
 * @param Type Type of stone.
 * @param Row The closer to the last line, the higher the value.
 */
evalValue(normal, row1, 1000). %value of normal stone in relation to row 8; no entry for row 8 because stone is then a king
evalValue(normal, row2, 1020).
evalValue(normal, row3, 1040).
evalValue(normal, row4, 1060).
evalValue(normal, row5, 1080).
evalValue(normal, row6, 1100).
evalValue(normal, row7, 1120).
evalValue(normal, canBeHitten, 100).
evalValue(king, normal, 2000). %nothing special at king position
evalValue(king, canBeHitten, 200).
evalBonus(unhittable, 200).
evalBonus(canHit, 600).

/**
 * Predicate to evaluate a game.
 * @param World List of all stones.
 * @param ViewColor The point of view.
 * @return Result The value of the world.
 */
valueOfGame(World,ViewColor,Result) :-
   retractall(hittenStone(_)),
   valueOfGame(World,World,ViewColor,Result).

valueOfGame(_,[],_,0).
valueOfGame(World,[Stone|State],ViewColor,Value) :-
   valueOfGame(World,State,ViewColor,CurResult),
   valueOfStone(World,Stone,StoneValue),
   addValue(ViewColor,Stone,CurResult,StoneValue,Result_n),
   findall(Stone,hittenStone(Stone),HittenStones),
   wrongCalculatedValues(World, ViewColor,HittenStones,State,Correction),
   Value is Result_n + Correction.

/**
 * Predicate to add a Value or to subtract it if it is the wrong viewcolor.
 * @param ViewColor
 * @param Stone
 * @param BaseValue
 * @return Result The new value.
 */
addValue(ViewColor, Stone,BaseValue,ValueToAdd,Result) :-
    Stone = stone(_,ViewColor,_) ->
       Result is BaseValue + ValueToAdd
    ;
       Result is BaseValue - ValueToAdd.

/**
 * Predicate to add a Value or to subtract it if it is the wrong viewcolor.
 * @param ViewColor
 * @param Stone
 * @param BaseValue
 * @return Result The new value.
 */
subtractValue(ViewColor, Stone,BaseValue,ValueToAdd,Result) :-
    Stone = stone(_,ViewColor,_) ->
        Result is BaseValue - ValueToAdd
    ;
        Result is BaseValue + ValueToAdd.

wrongCalculatedValues(_,_,[],_,0).
wrongCalculatedValues(World,ViewColor,[Stone|HittenStones],State,Corrections) :-
   wrongCalculatedValues(World,ViewColor,HittenStones,State,Correction1),
   (
         member(Stone,State) ->
            retract(hittenStone(Stone)),
            valueOfStone(Stone,WrongValue),
            subtractValue(ViewColor,Stone,Correction1,WrongValue,TmpValue),
            hittenValue(Stone, CorrectValue),
            addValue(ViewColor,Stone,TmpValue,CorrectValue,Corrections)
      ;
         Corrections = Correction1
   ).

hittenValue(stone(_,_,Type),Value) :- evalValue(Type,canBeHitten,Value).

/**
 * Predicate to get the Value of a stone.
 * @param World List of all stones.
 * @param Stone The stone to evaluate.
 * @return Value The value.
 */
valueOfStone(World,Stone,Value) :-
   Stone = stone(Field,_,_),
   (
         hittenStone(Stone),!,
         retract(hittenStone(Stone)) ->
            hittenValue(Stone,Value)
      ;
         baseValueOfStone(Stone,BaseValue),
         unhittableBonus(Field,Bonus1),
         hitBonus(World,Stone,Bonus2),
         Value is BaseValue + Bonus1 + Bonus2
   ).

/**
 * Predicate compute hitBonus for Stones that can hit /multihit.
 * Checks all hitpossibilities and evaluates.
 *
 * @param World List of all stones.
 * @param Stone The stone to evaluate.
 * @return Bones The hitbonus.
 */
hitBonus(World,Stone,Bonus) :-
   rulez:canHit(World,Stone,HitTree),
   search:longesPath(HitTree,Length,Path),
   (
         Length > 1 ->
            Path = [hit(_,_)|Hits],
            assertVictimList(World,Hits),
            Multiplier is Length - 1,
            evalBonus(canHit,BonusBase),
            Bonus is Multiplier * BonusBase
      ;
         Bonus = 0
   ).

/**
 * Predicate to assert a list of victims in the knowledge base.
 * Needed for hitbonus to shortly store alls victims.
 * @param World List of all stones.
 * @param Calls List of all calls.
 */
assertVictimList(_,[]).
assertVictimList(World,[hit(_,Call)|Hits]) :-
  Call =.. [performMove,Source,Destination],
  board:isFieldBetween(Source,Destination,VField),
  board:stoneAt(World,VField,Victim),
   assertz(hittenStone(Victim)),
   assertVictimList(World,Hits).

/**
 * Predicate to determine the base value of a stone.
 * @param Stone The stone to determine the value.
 * @return Value The base value.
 */
baseValueOfStone(stone(_,_,king), Value) :- evalValue(king,normal,Value).
baseValueOfStone(stone(field(Row,_),Color,normal),Value) :-
   isNormalized(Row,Color,Normalized),
   atom_concat('row',Normalized,EvalPos),
   evalValue(normal,EvalPos,Value).

isNormalized(Row,Color,Normalized) :-
   game:player(StartPos,Color),
   (
      StartPos == top ->
         Normalized = Row
      ;
      StartPos == bottom ->
         Normalized is 9 - Row
   ).

/**
 * Predicate to find out if a stone lies on a border field -> is unhittable.
 * @param Field The field the stone lies on.
 * @return Bonus.
 */
unhittableBonus(Field,Bonus) :-
   (
      board:isBorderCol(Field)
      ;
      board:isBorderRow(Field)
   ) ->
      evalBonus(unhittable,Bonus)
   ;
   Bonus = 0.
