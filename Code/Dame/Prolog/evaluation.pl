% Autor: Robert Maas
% Datum: 07.12.2015

%simple evaluation algorithm
%call: World,World,Result,ViewColor
%World: list of all stones
%World: first is for working, second for other predicates
%ViewColor: color from which view the game is evaluated
valueOfGame([],_, 0,_).
valueOfGame([Stone| State], World, EvaluationResult, ViewColor) :-
   valueOfGame(State, World, Result, ViewColor),
   valueOfStone(World, Stone, Value, ViewColor),
   (
      Stone = stone(_,_,ViewColor,_)->
         EvaluationResult is Value + Result
      ;
         EvaluationResult is Result - Value
   ).

valueOfStone(World, Stone, Result, ViewColor) :-
   Stone = stone(_,_,_,Type),
   hasNeighbours(Stone,World, Neighbours),
   (
      hasHitState(World, ViewColor, Stone,Neighbours, EvalPos) ->
         evalValue(Type, EvalPos,Result)
      ;
         evalValue(Type,normal,Result)
   ).

hasHitState(World, ViewColor, Stone, Neighbours, EvalPos) :-
   (
      Stone = stone(_,_,ViewColor,_),
      canBeHitten(World,Stone,Neighbours) %if stonecolor is in turn, the stone cannot hit enemy, but enemy can perhaps (and will, if possible) hit stone
   ) ->
      EvalPos = willBeHitten,!
   ;
      canHit(World,Stone,Neighbours) -> %stone cannot be hitten by enemy, but the stone can perhaps hit an enemy
         EvalPos = canHit.

%call: World, Victim, Neighbours
%true, if Victim can be hitten by a neighbour
canBeHitten(_,_,[]) :- fail.
canBeHitten(World,Victim,[[Hitter,_]|Neighbours]) :-
   canHit(World,Hitter,Victim, _) ->
      true
   ;
      canBeHitten(World,Hitter,Neighbours).

%call: Stone, World, Neighbours
%Neighbours: list of all Neighbours and their relation to the stone
%Neighbours list entry format: [stone, relation]
%searches for neighbours of given stone
%stone an world have to be known
%if Neighbours is known, their must be in same order as in World
hasNeighbours(_,[],[]).
hasNeighbours(stone(Row,Col,_,_), [CheckStone |GameState], Neighbours) :-
   hasNeighbours(stone(Row,Col,_,_),GameState,FoundNeighbours),
   (
      (
         CheckStone = stone(Row2,Col2,_,_),
         hasRelation(Row,Col,Row2,Col2,Relation)
      ) ->
         Neighbours = [[CheckStone, Relation] | FoundNeighbours]
      ;
         Neighbours = FoundNeighbours
   ).

%call: World, Hitter, Neighbours
%true, if Hitter can hit a neighbour
canHit(_,_,[]) :- fail.
canHit(World,Hitter,[[Victim2, Relation]|Neighbours]) :-
   canHit(World,Hitter,Victim2, Relation) ->
      true
   ;
      canHit(World,Hitter,Neighbours).

%call: World, Hitter, Victim, Relation
%Relation: <Victim> is <Relation> of <Hitter>
%works if relation is unknown
canHit(World, Hitter, stone(VRow,VCol,VColor,_), Relation) :-
   Hitter = stone(HRow,HCol,HColor,_),
   HColor \==VColor,
   moveDirections(Hitter, Relation),
   hasRelation(HRow,HCol,VRow,VCol,Relation), %check if victim stone at given position; only necessary in case of backtracking
   !,
   hasRelation(VRow,VCol,TRow,TCol,Relation),
   fieldFree(World,TRow,TCol).
