% Autor: Robert Maas
% Datum: 07.12.2015

%simple evaluation algorithm
evaluateGame([],_, 0,_).
evaluateGame([Stone| State], World, EvaluationResult, ViewColor) :-
   evaluateGame(State, World, Result, ViewColor),
   evaluateStone(World, Stone, Value, ViewColor),
   (
      Stone = stone(_,_,ViewColor,_)->
         EvaluationResult is Value + Result
      ;
         EvaluationResult is Result - Value
   ).

evaluateStone(World, Stone,Result, ViewColor) :-
   Stone = stone(_,_,_,Type),
   neighbours(Stone,World, Neighbours),
   (
      checkHit(World, ViewColor, Stone,Neighbours, EvalPos) ->
         evalValue(Type, EvalPos,Result)
      ;
         evalValue(Type,normal,Result)
   ).

checkHit(World, ViewColor, Stone, Neighbours, EvalPos) :-
   (
      Stone = stone(_,_,ViewColor,_),
      checkBeHitten(World,Stone,Neighbours) %if stonecolor is in turn, the stone cannot hit enemy, but enemy can perhaps (and will, if possible) hit stone
   ) ->
      EvalPos = willBeHitten,!
   ;
      checkHitPossibility(World,Stone,Neighbours) -> %stone cannot be hitten by enemy, but the stone can perhaps hit an enemy
         EvalPos = canHit.

checkBeHitten(_,_,[]) :- fail.
checkBeHitten(World,Victim,[[Hitter,_]|Neighbours]) :-
   canHit(World,Hitter,Victim, _) ->
      true
   ;
      checkHitPossibility(World,Hitter,Neighbours).

%searches for neighbours of given stone
neighbours(_,[],[]).
neighbours(stone(Row,Col,_,_), [CheckStone |GameState], Neighbours) :-
   neighbours(stone(Row,Col,_,_),GameState,FoundNeighbours),
   (
      (
         CheckStone = stone(Row2,Col2,_,_),
         checkRelation(Row,Col,Row2,Col2,Relation)
      ) ->
         Neighbours = [[CheckStone, Relation] | FoundNeighbours]
      ;
         Neighbours = FoundNeighbours
   ).

%checks, if a stone can hit a neighbour
checkHitPossibility(_,_,[]) :- fail.
checkHitPossibility(World,Hitter,[[Victim2, Relation]|Neighbours]) :-
   canHit(World,Hitter,Victim2, Relation) ->
      true
   ;
      checkHitPossibility(World,Hitter,Neighbours).

canHit(World, Hitter, stone(VRow,VCol,VColor,_), Relation) :-
   Hitter = stone(HRow,HCol,HColor,_),
   HColor \==VColor,
   moveDirections(Hitter, Relation),
   calculateTarget(HRow,HCol,Relation,VRow,VCol),!, %check if victim stone at given position; only necessary in case of backtracking
   calculateTarget(VRow,VCol,Relation,TRow,TCol),
   fieldFree(World,TRow,TCol).
