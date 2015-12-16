% Autor: Robert Maas
% Datum: 16.12.2015

:- module(rulez, [
     moveDirections/2]).

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