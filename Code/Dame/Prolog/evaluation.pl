% Autor: Robert Maas
% Datum: 03.12.2015

%VVVVVVVVVVVVVVVVVEEEEEEEEEEEEEEEEEEERRRRRRRRRRRRRRRRRRRRRRRYYYYYYYYYYYYYYYY
%simple evaluation algorithm based uppon stone count
evaluateGame([], 0,_).
evaluateGame([Stone| State], EvaluationResult, ViewColor) :-
   evaluateGame(State,Result,ViewColor),
   evaluateStone(Stone,Value, ViewColor),
   EvaluationResult is Value + Result.
   
evaluateStone(stone(_,_,Color,Type), Result,ViewColor) :-
   evalValue(Type, normal, Value),
   (
      Color == ViewColor ->
      Result is Value
      ;
      (
         Result is Value * -1
      )
   ).