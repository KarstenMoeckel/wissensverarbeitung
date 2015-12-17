% Autor: Robert Maas
% Datum: 17.12.2015

:- module(rulez, [
     moveDirections/2
     ]).

:- use_module(game).
     
%call: +Stone, -Direction
moveDirections(stone(field(Row,Col),_,king), Direction):-
      Row \==8,
      bottomDirections(Col, Direction)
   ;
      Row \== 1,
      topDirections(Col, Direction).

moveDirections(stone(field(_,Col),Color,normal), Direction) :-
   player(Position, Color),
   (
         Position == top -> bottomDirections(Col, Direction)
      ;
         Position == bottom -> topDirections(Col, Direction)
   ).

topDirections(Col, Direction) :-
      Col \== 1,
      Direction = topLeft
   ;
      Col \==8,
      Direction = topRight.

bottomDirections(Col, Direction) :-
      Col \== 1,
      Direction = bottomLeft
   ;
      Col \==8,
      Direction = bottomRight.

%call: +World, +Hitter, +Neighbours, --Victims
hasHitPossibility(_,_,[],[]).
hasHitPossibility(World,Hitter, [[Victim,Relation]|Neighbours],Victims) :-
   hasHitPossibility(World,Hitter,Neighbours,FoundVictims),
   (
      canHit(World,Hitter,Victim,Relation)->
         Victims = [[Victim,Relation]|FoundVictims]
      ;
         Victims = FoundVictims
   ).
      
%call: +World, +Hitter, +Victim, +Relation
%Relation: <Victim> is <Relation> of <Hitter>
canHit(World, Hitter, stone(VField,VColor,_), Relation) :-
   not(Hitter = stone(_,VColor,_)),
   moveDirections(Hitter, Relation),
   board:hasRelation(VField,TField,Relation),
   game:field(TField,black),
   board:isFree(World,TField).