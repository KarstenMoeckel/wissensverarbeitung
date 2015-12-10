% Autor: Robert Maas
% Datum: 07.12.2015

%simple evaluation algorithm
%call: +World, +World, --Result, +ViewColor
%World: list of all stones
%World: first is for working, second for other predicates
%ViewColor: color from which view the game is evaluated
valueOfGame([],_, 0,_).
valueOfGame([Stone| State], World, EvaluationResult, ViewColor) :-
   valueOfGame(State, World, Result, ViewColor),
   valueOfStone(World, Stone, ViewColor, Value),
   (
      Stone = stone(_,ViewColor,_)->
         EvaluationResult is Value + Result
      ;
         EvaluationResult is Result - Value
   ).

%call: +World, +Stone, +ViewColor, -Value
valueOfStone(World, Stone, ViewColor, Value) :-
   Stone = stone(field(_,Col),_,Type),
   hasNeighbours(Stone, World, Neighbours),
   (
      (
         Stone = stone(_,ViewColor,_),
         canBeHitten(World,Stone,Neighbours)
      )->
         evalValue(Type,willBeHitten, Value)
      ;
      (
         valueOfStone(Stone,BaseValue),
         unhittableBonus(Col,Bonus1),
         hitBonus(World,Stone,Neighbours, Bonus2),
         Value = BaseValue + Bonus1 + Bonus2
      )
   ).

valueOfStone(stone(_,_,queen), Value) :- evalValue(queen, normal,Value).
valueOfStone(Stone, Value) :-
   Stone = stone(field(Row,_),Color,normal),
   isNormalized(Row,Color,Normalized),
   atom_concat(row,Normalized,EvalPos),
   evalValue(normal,EvalPos,Value).

unhittableBonus(Col, Bonus) :-
   (Col == 1 ; Col == 8)-> evalBonus(unhittable,Bonus)
   ;
   Bonus = 0.

isNormalized(Row, Color, NormalizedRow) :-
   player(StartPos,Color),
   (
      StartPos == top ->
         NormalizedRow = Row
      ;
      StartPos == bottom ->
         NormalizedRow is 9 - Row
   ).

hitBonus(World,Hitter,Neighbours,Bonus) :-
   canHit(World,Hitter,Neighbours) ->
      evalBonus(canHit,Bonus)
   ;
      Bonus = 0.

%call: +World, +Victim, +Neighbours
%true, if Victim can be hitten by a neighbour
canBeHitten(_,_,[]) :- fail.
canBeHitten(World,Victim,[[Hitter,_]|Neighbours]) :-
   canHit(World,Hitter,Victim, _) ->
      true
   ;
      canBeHitten(World,Hitter,Neighbours).

%call: +Stone, +World, -Neighbours
%Neighbours: list of all Neighbours and their relation to the stone
%Neighbours list entry format: [stone, relation]
%searches for neighbours of given stone
%stone an world have to be known
%if Neighbours is known, their must be in same order as in World
hasNeighbours(_,[],[]).
hasNeighbours(stone(Field,_,_), [CheckStone |GameState], Neighbours) :-
   hasNeighbours(stone(Field,_,_),GameState,FoundNeighbours),
   (
      (
         CheckStone = stone(Field2,_,_),
         hasRelation(Field,Field2,Relation)
      ) ->
         Neighbours = [[CheckStone, Relation] | FoundNeighbours]
      ;
         Neighbours = FoundNeighbours
   ).

%call: +World, +Hitter, -Neighbours
%true, if Hitter can hit a neighbour
canHit(_,_,[]) :- fail.
canHit(World,Hitter,[[Victim2, Relation]|Neighbours]) :-
   canHit(World,Hitter,Victim2, Relation) ->
      true
   ;
      canHit(World,Hitter,Neighbours).

%call: +World, +Hitter, +Victim, -Relation
%Relation: <Victim> is <Relation> of <Hitter>
canHit(World, Hitter, stone(VField,VColor,_), Relation) :-
   Hitter = stone(HField,HColor,_),
   HColor \==VColor,
   moveDirections(Hitter, Relation),
   hasRelation(HField,VField,Relation), %check if victim stone at given position; only necessary in case of backtracking
   !,
   hasRelation(VField,TField,Relation),
   fieldFree(World,TField).
