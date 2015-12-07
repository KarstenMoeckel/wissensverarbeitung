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
   checkHitPossibility(World,Stone,Neighbours) ->
   (
      (
         Stone = stone(_,_,ViewColor,_) ->
            EvalPos = canHit
         ;
            EvalPos = willBeHitted
      )
   ).

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

checkHitPossibility(_,_,[]) :- fail.
checkHitPossibility(World,Hitter,[Entry|Neighbours]) :-
   canHit(World,Hitter,Entry) ->
      true
   ;
      checkHitPossibility(World,Hitter,Neighbours).

canHit(World, Hitter, [stone(VRow,VCol,VColor,_), Relation]) :-
   not(Hitter = stone(_,_,VColor,_)),
   moveDirections(Hitter, Relation),
   calculateTarget(VRow,VCol,Relation,TRow,TCol),
   fieldFree(World,TRow,TCol).
