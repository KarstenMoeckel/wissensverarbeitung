% Autor: Robert Maas
% Datum: 07.12.2015

:- dynamic world/1.
:- dynamic viewColor/1.
:- dynamic hittenStone/1.

%evaluation algorithm
%call: +World, +ViewColor, --Result
%ViewColor: color from which view the game is evaluated
valueOfGame(World,ViewColor, Value) :-
   retractall(hittenStone(_)),
   assertz(world(World)),
   assertz(viewColor(ViewColor)),
   valueOfGame(World,Value),
   retractall(world(_)),
   retractall(viewColor(_)).

valueOfGame([], 0).
valueOfGame([Stone| State], Value) :-
   viewColor(ViewColor),
   valueOfGame(State, Result),
   valueOfStone(Stone, StoneValue),
   (
      Stone = stone(_,ViewColor,_)->
         Result_n is Result + StoneValue
      ;
         Result_n is Result - StoneValue
   ),
   findall(Stone,hittenStone(Stone),HittenStones),
   wrongCalculatedValues(HittenStones,State,Correction),
   Value is Result_n + Correction.

hittenValue(stone(_,_,Type),Value) :- evalValue(Type,canBeHitten,Value).

wrongCalculatedValues([],_,0).
wrongCalculatedValues([Stone,HittenStones], State, CorrectionValue) :-
   worngCalculatedValues(HittenStones, State, CorrectionValue1),
   (
         member(Stone, State)->
            retract(hittenStone(Stone)),
            valueOfStone(Stone,WrongValue),
            hittenValue(Stone, CorrectValue),
            Correction is CorrectValue - WrongValue,
            Stone = stone(_,Color,_),
            (
               viewColor(Color) ->
                  CorrectionValue is CorrectionValue1 - Correction
               ;
               CorrectionValue is CorrectionValue1 + Correction
            )
      ;
         CorrectionValue = CorrectionValue1
   ).


%call: +Stone, -Value
valueOfStone(Stone, Value) :-
   world(World),
   Stone = stone(Field,_,_),
   hasNeighbours(Stone, World, Neighbours),
   (
      (
            (
               hittenStone(Stone),!,
               retract(hittenStone(Stone))
            )
         ;
         (
            viewColor(ViewColor),
            Stone = stone(_,ViewColor,_),
            canBeHitten(World,Stone,Neighbours)
         )
      )->
         hittenValue(Stone,Value)
      ;
      (
         baseValueOfStone(Stone,BaseValue),
         unhittableBonus(Field,Bonus1),
         hitBonus(Stone, Bonus2),
         Value is BaseValue + Bonus1 + Bonus2
      )
   ).

baseValueOfStone(stone(_,_,queen), Value) :- evalValue(queen, normal,Value).
baseValueOfStone(Stone, Value) :-
   Stone = stone(field(Row,_),Color,normal),
   isNormalized(Row,Color,Normalized),
   atom_concat(row,Normalized,EvalPos),
   evalValue(normal,EvalPos,Value).

unhittableBonus(field(_,Col), Bonus) :-
      (
         Col == 1; Col == 8
      )->
         evalBonus(unhittable,Bonus)
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

hitBonus(Hitter,Bonus) :-
   (
      canMultiHit(Hitter,_,Counter)->
         evalBonus(canHit,BonusBase),
         Bonus is BonusBase * Counter
      ;
      Bonus = 0
   ).

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

canMultiHit(Hitter, PreviousVictim, Counter) :-
   world(World),
   hasNeighbours(Hitter,World,Neighbours1),
   (
      var(PreviousVictim) ->
         Neighbours = Neighbours1
      ;
         subtract(Neighbours1, [PreviousVictim,_], Neighbours)
   ),
   canHit(World,Hitter, Neighbours,[Victim,Relation])->
      assertz(hittenStone(Victim)),
      Victim = stone(Field,_,_),
      hasRelation(Field,Destination,Relation),
      Hitter = stone(_,Color,Type),
      (
         canMultiHit(stone(Destination,Color,Type),Victim,Counter1) ->
            Counter is Counter1 + 1
         ;
            Counter = 1
      ).

%call: +world, +Hitter, -PreviousViction, --HittenStones
canMultiHit(World,Hitter, PreviousVictim, HittenStones) :-
   assertz(World),
   (
      canMultiHit(Hitter,PreviousVictim,_)->
         retractall(World),
         findall(Stone,hittenStone(Stone),HittenStones),
         retractall(hittenStones(_))
      ;
         retractall(World),
         fail
   ).

%call: +World, +Hitter, -Neighbours
%true, if Hitter can hit a neighbour
canHit(_,_,[], _) :- fail.
canHit(World,Hitter,[[Victim, Relation]|Neighbours], VictimInfo) :-
   canHit(World,Hitter,Victim, Relation) ->
      VictimInfo = [Victim, Relation]
   ;
      canHit(World,Hitter,Neighbours, VictimInfo).

%call: +World, +Hitter, +Victim, -Relation
%Relation: <Victim> is <Relation> of <Hitter>
canHit(World, Hitter, stone(VField,VColor,_), Relation) :-
   Hitter = stone(HField,HColor,_),
   HColor \==VColor,
   moveDirections(Hitter, Relation),
   hasRelation(HField,VField,Relation), %check if victim stone at given position; only necessary in case of backtracking
   !,
   hasRelation(VField,TField,Relation),
   field(TField,black),
   fieldFree(World,TField).
