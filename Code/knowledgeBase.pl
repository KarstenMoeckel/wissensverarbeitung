% Autor: Robert Maas
% Datum: 02.12.2015

:- dynamic history/1.

% player(Position, Color).
player(top, black).
player(bottom, white).

:- dynamic turn/1. %turn(Color)
:- dynamic player/1. % player(Color)

% stone(Row,Col,StoneColor,Type).
:- dynamic stone/4.
stone(3,4,black,normal).
stone(4,5,white,normal).
stone(8,1,black,queen).
stone(2,7,white,queen).

field(Row,Col,Color) :-
   numbers(Row),
   numbers(Col),
   Sum is Row + Col,
   R is Sum mod 2,
   (
      R==1,
      Color='black',
      !
   )
   ;
   Color='white'.

numbers(X) :- between(1,8,X).