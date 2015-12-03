% Autor: Robert Maas
% Datum: 03.12.2015

%VVVVVVVVVVVVVVVVVEEEEEEEEEEEEEEEEEEERRRRRRRRRRRRRRRRRRRRRRRYYYYYYYYYYYYYYYY
%simple evaluation algorithm based uppon stone count
evaluateGame([],_, 0,_).
evaluateGame([Stone| State], World, EvaluationResult, ViewColor) :-
   evaluateGame(State, World, Result, ViewColor),
   evaluateStone2(World, Stone, Value, ViewColor),
   EvaluationResult is Value + Result.
   
evaluateStone(_, stone(_,_,Color,Type), Result, ViewColor) :-
   evalValue(Type, normal, Value),
   (
      Color == ViewColor ->
      Result is Value
      ;
      (
         Result is Value * -1
      )
   ).

evaluateStone2(World, Stone, Result, ViewColor) :-
   Stone = stone(_,_,Color,Type),
   (
      Color == ViewColor ->
         evaluateOwnStone(World, Stone, Position)
      ;
         evaluateForeignStone(World, Stone, Position)
   ),
   evalValue(Type,Position,Result).

evaluateOwnStone(World, Stone, Position) :-
   neighbours(Stone,World, Neighbours),
   (
      checkHitPosibility(World,Stone,Neighbours, Victim) ->
      (
         Victim = stone(_,_,_,Type),
         Type == normal ->
            Position = canHitNormal
         ;
            Position = canHitQueen
      )
      ;
         Position = normal
   ).

evaluateForeignStone(_,_, normal).

%searches for neighbours of given stone
neighbours(_,[],[]).
neighbours(stone(Row,Col,_,_), [CheckStone |GameState], Neighbours) :-
   neighbours(stone(Row,Col,_,_),GameState,Neighbours2),
   (
      (
         CheckStone = stone(Row2,Col2,_,_),
         checkRelation(Row,Col,Row2,Col2,Relation)
      ) ->
         Neighbours = [[CheckStone, Relation] | Neighbours2]
      ;
         Neighbours = Neighbours2
   ).

checkHitPosibility(_,_,[]) :- fail.
checkHitPosibility(World,Hitter,[[Victim2,Relation]|Neighbours], Victim) :-
   canHit(World,Hitter,Relation,Victim2) ->
      Victim = Victim2
   ;
      checkHitPosibility(World,Hitter,Neighbours).

canHit(World, Hitter, Relation, Victim) :-
   Hitter = stone(_,_,HColor,_),
   Victim = stone(VRow,VCol,VColor,_),
   HColor \== VColor,
   calculateTarget(VRow,VCol,Relation,TRow,TCol),
   fieldFree(World,TRow,TCol).
