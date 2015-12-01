% Autor: Robert Maas
% Datum: 01.12.2015

% player(Position, Color).
player(top, black).
player(bottom, white).

% stone(Row,Col,StoneColor,Type).
:- dynamic stone/4.
stone(3,4,black,normal).
stone(4,5,white,normal).
stone(8,1,black,queen).
stone(2,7,white,queen).