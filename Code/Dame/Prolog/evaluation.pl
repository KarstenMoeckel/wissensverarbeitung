% Autor: Robert Maas
% Datum: 03.12.2015

%VVVVVVVVVVVVVVVEEEEEEEEEEEEEEEEERRRRRRRRRRRRRRRRRRRRRYYYYYYYYYYYYYYYY
%simple evaluation algorithm based uppon stone count
evaluateGame([],_, 0,_).
evaluateGame([Stone| State], World, EvaluationResult, ViewColor) :-
   evaluateGame(State, World, Result, ViewColor),
   evaluateStone(World, Stone, Value, ViewColor),
   EvaluationResult is Value + Result.
   
evaluateStone(World, Stone, Result, ViewColor) :-
   Stone = stone(_,_,Color,_),
   evaluateStone(World, Stone, Result1),
   (
      Color == ViewColor ->
         Result is Result1
      ;
         Result is Result1 * -1
   ).

evaluateStone(World, Stone,Result) :-
   Stone = stone(_,_,_,Type),
   neighbours(Stone,World, Neighbours),
   (
      checkHitPossibility(World,Stone,Neighbours, Victim) ->
      (
         Victim = stone(_,_,_,VType),
         VType == normal ->
            evalValue(Type,canHitNormal,Result)
         ;
            evalValue(Type,canHitQueen,Result)
      )
      ;
         evalValue(Type,normal,Result)
   ).

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

checkHitPossibility(_,_,[]) :- fail.
checkHitPossibility(World,Hitter,[[Victim2,Relation]|Neighbours], Victim) :-
   canHit(World,Hitter,Relation,Victim2) ->
      Victim = Victim2
   ;
      checkHitPossibility(World,Hitter,Neighbours, Victim).

canHit(World, Hitter, Relation, stone(VRow,VCol,VColor,_)) :-
   Hitter = stone(_,_,HColor,_),
   HColor \== VColor,
   moveDirections(Hitter, Relation),
   calculateTarget(VRow,VCol,Relation,TRow,TCol),
   fieldFree(World,TRow,TCol).

moveDirections(stone(_,_,_,queen), Direction):-
      Direction = bottomLeft
   ;
      Direction = bottomRight
   ;
      Direction = topLeft
   ;
      Direction = topRight.

moveDirections(stone(_,_,Color,normal), Direction) :-
   player(Position, Color),
   (
      Position == top ->
      (
         Direction = bottomLeft
         ;
         Direction = bottomRight
      )
      ;
      Position == bottom ->
      (
         Direction = topLeft
         ;
         Direction = topRight
      )
   ).
      