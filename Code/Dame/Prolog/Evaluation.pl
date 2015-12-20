% Autor: Robert Maas
% Datum: 20.12.2015

:- module(evaluation,[
      valueOfGame/3 %call: +World, +ViewColor, -Result
      ]).

:- use_module(board).
:- use_module(game).
:- use_module(rulez).
:- use_module(search).

:- dynamic hittenStone/1.
      
%evalValue(StoneType, Position, Value)
evalValue(normal, row1, 1000). %value of normal stone in relation to row 8; no entry for row 8 because stone is then a queen
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

valueOfGame(World,ViewColor,Result) :-
   retractall(hittenStone(_)),
   valueOfGame(World,World,ViewColor,Result).
   
valueOfGame(_,[],_,0).
valueOfGame(World,[Stone|State],ViewColor,Value) :-
   valueOfGame(World,State,ViewColor,CurResult),
   valueOfStone(World,Stone,StoneValue),
   (
      Stone = stone(_,ViewColor,_) ->
         Result_n is CurResult + StoneValue
      ;
         Result_n is CurResult - StoneValue
   ),
   findall(Stone,hittenStone(Stone),HittenStones),
   wrongCalculatedValues(World, ViewColor,HittenStones,State,Correction),
   Value is Result_n + Correction.

wrongCalculatedValues(_,_,[],_,0).
wrongCalculatedValues(World,ViewColor,[Stone|HittenStones],State,Corrections) :-
   wrongCalculatedValues(World,ViewColor,HittenStones,State,Correction1),
   (
         member(Stone,State) ->
            retract(hittenStone(Stone)),
            valueOfStone(Stone,WrongValue),
            hittenValue(Stone, CorrectValue),
            Correction is CorrectValue - WrongValue,
            (
               Stone = stone(_,ViewColor,_) ->
                  Corrections is Correction1 - Correction
               ;
               Corrections is Correction1 + Correction
            )
      ;
         Corrections = Correction1
   ).


hittenValue(stone(_,_,Type),Value) :- evalValue(Type,canBeHitten,Value).
   
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

hitBonus(World,Stone,Bonus) :-
   rulez:canHit(World,Stone,HitTree),
   search:longesPath(HitTree,Length,Path),
   (
         Length > 1 ->
            Path = [Stone|Victims],
            assertVictimList(Victims),
            Multiplier is Length - 1,
            evalBonus(canHit,BonusBase),
            Bonus is Multiplier * BonusBase
      ;
         Bonus = 0
   ).

assertVictimList([]).
assertVictimList([Victim|Victims]) :-
   assertz(hittenStone(Victim)),
   assertVictimList(Victims).
   
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
   
unhittableBonus(Field,Bonus) :-
   board:isBorderCol(Field) ->
      evalBonus(unhittable,Bonus)
   ;
   Bonus = 0.