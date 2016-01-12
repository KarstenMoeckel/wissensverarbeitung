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
    miniMax(Strategy, Value, RList, TmpValue),
    (
        TmpValue == lost ->
            BestValue = won
        ;
        TmpValue == won ->
            BestValue = lost
        ;
        BestValue = TmpValue
    ).

miniMax(_,Value,[],Value).
miniMax(Strategy,CurBestValue,[Node| RList], BestValue) :-
    searchNode:valueOfNode(Node,NodeValue),
    compareValues(Strategy,CurBestValue,NodeValue,TmpValue),
    miniMax(Strategy,TmpValue,RList,BestValue).

compareValues(min,Value1,Value2, OptimalValue) :-
    Value1 == won ->
        OptimalValue = Value2
    ;
    Value2 == won ->
        OptimalValue = Value1
    ;
    (Value1 == lost ; Value2 == lost) ->
        OptimalValue = lost
    ;
    Value1 < Value2 ->
        OptimalValue is Value1
    ;
        OptimalValue is Value2.
compareValues(max,Value1,Value2, OptimalValue) :-
    Value1 == lost ->
        OptimalValue = Value2
    ;
    Value2 == lost ->
        OptimalValue = Value1
    ;
    (Value1 == won ; Value2 == won) ->
        OptimalValue = won
    ;
    Value1 > Value2 ->
        OptimalValue is Value1
    ;
        OptimalValue is Value2.
