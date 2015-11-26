% Autor: Robert Maas
% Datum: 26.11.2015

:- use_module(library(pce)).

fieldLength(20).
blackFieldColor(gray).
   
drawGame(Picture) :-
   (
      numbers(X),
      drawRow(Picture, X, 5, 5),
      fail
   )
   ;
      true.

drawRow(Picture, Row, FieldOffsetT, FieldOffsetL) :-
   fieldLength(Length),
   !,
   Top is Length * (Row - 1) + FieldOffsetT,
   (
      numbers(X),
      createField(Row,X, Field),
      Left is Length * (X - 1) + FieldOffsetL,
      send(Picture, display, Field, point(Left, Top)),
      fail
   )
   ;
      true.
%   ;
%      write('| '),
%      writeln(Row).

createField(Row,Col,Field) :-
   fieldLength(Length),
   !,
   new(Field, box(Length,Length)),
   field(Row,Col, FieldColor),
   (
      (
         FieldColor == black,
         blackFieldColor(Color),
         send(Field, fill_pattern, colour(Color)),
         !
      )
      ;
         true
   ).
%   (
%      stone(Row,Col,Color,Lady),
%      legend(Color,Lady,Char),
%      write(Char),
%      !
%   ).
   
startGUI :-
   new(Window, dialog('EA Dame')),
   new(GameField, picture),
   send(Window, append, GameField),
   drawGame(GameField),
   send(Window, open).