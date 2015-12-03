% Autor: Robert Maas
% Datum: 03.12.2015

%VVVVVVVVVVVVVVVVVEEEEEEEEEEEEEEEEEEERRRRRRRRRRRRRRRRRRRRRRRYYYYYYYYYYYYYYYY
%simple evaluation algorithm based uppon stone count
evaluateGame([], 0,_).
evaluateGame([Stone| State], EvaluationResult, ViewColor) :-
   evaluateGame(State,Result,ViewColor),
   evaluateStone(Stone,Value, ViewColor),
   EvaluationResult is Value + Result.
   
evaluateStone(stone(_,_,Color,Type), Result, ViewColor) :-
   evalValue(Type, normal, Value),
   (
      Color == ViewColor ->
      Result is Value
      ;
      (
         Result is Value * -1
      )
   ).

%searches for neighbours of given stone
neighbours(_,[],[]).
neighbours(stone(Row,Col,_,_), [CheckStone |GameState], Neighbours) :-
   CheckStone = stone(Row2,Col2,_,_),
   neighbours(stone(Row,Col,_,_),GameState,Neighbours2),
   (
         (
            Row2 is Row + 1,
            Col2 is Col + 1
         ) ->
            Neighbours = [CheckStone | Neighbours2]
         ;
         (
            Row2 is Row + 1,
            Col2 is Col - 1
         ) ->
            Neighbours = [CheckStone | Neighbours2]
         ;
         (
            Row2 is Row - 1,
            Col2 is Col + 1
         ) ->
            Neighbours = [CheckStone | Neighbours2]
         ;
         (
            Row2 is Row - 1,
            Col2 is Col - 1
         ) ->
            Neighbours = [CheckStone | Neighbours2]
         ;
         Neighbours = Neighbours2
   ).