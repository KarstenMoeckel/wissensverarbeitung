% Autor: Robert Maas
% Datum: 09.12.2015

:- dynamic history/1.
:- dynamic historyUpdated/0. %flag for GUI for pending history changes
:- dynamic stonesUpdated/0. % flag for GUI for pending stone changes
:- dynamic option/2. %option(Type,Value), e.g. option(searchDepth,5).
:- dynamic turn/1. %turn(Color), color of player, who has to move
:- dynamic player/1. % player(Color), color of human player
:- dynamic game/1.

% stone(field(Row,Col),StoneColor,Type).
:- dynamic stone/3.
stone(field(2,3),black,normal).
stone(field(3,4),black,normal).
stone(field(4,5),white,normal).
stone(field(8,1),black,queen).
stone(field(2,7),white,queen).

player(top, black).
player(bottom, white).

%node(parent, value, world, player, childnodes[])
% parent:       node one level above.
% value:        value of the node from the evaluation function.
% world:        stones of the players.
% childnodes:   list of nodes one Level below.
:- dynamic node/5. %node(parent, value, world, player, childnodes[])

field(field(Row,Col),Color) :-
   numbers(Row),
   numbers(Col),
   Sum is Row + Col,
   R is Sum mod 2,
   (
      R==1 ->
         Color='black'
      ;
         Color='white'
   ).

numbers(X) :- between(1,8,X).

%evalValue(StoneType, Position, Value)
evalValue(normal, row1, 1000). %value of normal stone in relation to row 8; no entry for row 8 because stone is then a queen
evalValue(normal, row2, 1025).
evalValue(normal, row3, 1050).
evalValue(normal, row4, 1100).
evalValue(normal, row5, 1150).
evalValue(normal, row6, 1300).
evalValue(normal, row7, 1500).
evalValue(normal, willBeHitten, 100).
evalValue(queen, normal, 2000). %nothing special at queen position
evalValue(queen, willBeHitten, 200).
evalBonus(unhittable, 200).
evalBonus(canHit, 600).
