:- module(minimax, [
    miniMax/2
]).

:- use_module(searchNode).
:- use_module(main).

isStrategy(Color, Strategy) :-
    main:player(Color) -> Strategy = min
    ;
    Strategy = max.

miniMax(_, []).
miniMax(BestValue , [Node | RList]) :-
    searchNode:turnOfNode(Node, Color),
    searchNode:valueOfNode(Node, Value),
    isStrategy(Color, Strategy),
    miniMax(Strategy, Value, RList, BestValue).

miniMax(_,Value,[],Value).
miniMax(Strategy,CurBestValue,[Node| RList], BestValue) :-
    searchNode:valueOfNode(Node,NodeValue),
    compareValues(Strategy,CurBestValue,NodeValue,TmpValue),
    miniMax(Strategy,TmpValue,RList,BestValue).

compareValues(min,Value1,Value2, OptimalValue) :-
    Value1 == ai_lost ->
        OptimalValue = Value2
    ;
    Value2 == ai_lost ->
        OptimalValue = Value1
    ;
    (Value1 == ai_won ; Value2 == ai_won) ->
        OptimalValue = ai_won
    ;
    Value1 < Value2 ->
        OptimalValue is Value1
    ;
        OptimalValue is Value2.
compareValues(max,Value1,Value2, OptimalValue) :-
    Value1 == ai_lost ->
        OptimalValue = Value2
    ;
    Value2 == ai_lost ->
        OptimalValue = Value1
    ;
    (Value1 == ai_won ; Value2 == ai_won) ->
        OptimalValue = ai_won
    ;
    Value1 > Value2 ->
        OptimalValue is Value1
    ;
        OptimalValue is Value2.
