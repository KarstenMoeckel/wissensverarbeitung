/** <mninimax>

This module provides the minimax algorithm.

@author Christian Schuett, Karsten Moeckel, Robert Maas
*/

:- module(minimax, [
    miniMax/2
]).

:- use_module(searchNode).
:- use_module(main).

/**
 * Predicate to find the strategy for a player.
 * @param Color The color of the player you want to find the strategy for.
 * @return Strategy max if ai, else min
 */
isStrategy(Color, Strategy) :-
    ai:aiPlayer(Color) -> Strategy = min
    ;
    Strategy = max.

/**
 * Predicate to start the minimax algorithm.
 * @return BestValue The BestValue of all Nodes.
 * @param Nodes The nodes to search the best value.
 */
miniMax(_, []).
miniMax(BestValue , [Node | RList]) :-
    searchNode:turnOfNode(Node, Color),
    searchNode:valueOfNode(Node, Value),
    isStrategy(Color, Strategy),
    miniMax(Strategy, Value, RList, BestValue).

/**
 * Predicate to find the best value of the Nodes,
 * by a given strategy.
 * @param Strategy The strategy to compare the values. min: The lower the better. Max: The higher the better.
 * @param CurBestValue The best value the predicate found so far.
 * @param Nodes The list with the nodes.
 * @return BestValue The best value of all nodes.
 */
miniMax(_,Value,[],Value).
miniMax(Strategy,CurBestValue,[Node| RList], BestValue) :-
    searchNode:valueOfNode(Node,NodeValue),
    compareValues(Strategy,CurBestValue,NodeValue,TmpValue),
    miniMax(Strategy,TmpValue,RList,BestValue).

/**
 * Predicate to find the best value for the min strategy.
 * @param min The min strategy.
 * @param Value1 The first value.
 * @param Value2 The second value.
 * @return OptimalValue The best value of Value1 and Value2.
 */
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

/**
 * Predicate to find the best value for the max strategy.
 * @param max The max strategy.
 * @param Value1 The first value.
 * @param Value2 The second value.
 * @return OptimalValue The best value of Value1 and Value2.
 */
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
