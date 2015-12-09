% Autor: Robert Maas
% Datum: 03.12.2015

:- dynamic history/1.
:- dynamic historyUpdated/0. %flag for GUI for pending history changes
:- dynamic stonesUpdated/0. % flag for GUI for pending stone changes

:- dynamic player/2. % player(Position, Color).

player(top, black).
player(bottom, white).

:- dynamic turn/1. %turn(Color), color of player, who has to move
:- dynamic player/1. % player(Color), color of human player

% stone(Row,Col,StoneColor,Type).
:- dynamic stone/4.
%stone(2,3,black,normal).
%stone(3,4,black,normal).
%stone(4,5,white,normal).
stone(8,1,black,queen).
stone(2,7,white,queen).

%node(parent, value, world, player, childnodes[])
% parent:       node one level above.
% value:        value of the node from the evaluation function.
% world:        stones of the players.
% childnodes:   list of nodes one Level below.
:- dynamic node/5. %node(parent, value, world, player, childnodes[])

field(Row,Col,Color) :-
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
evalValue(normal, normal, 1000). %nothing special at stone position
evalValue(normal, becomeQueen, 1500). %stone can become a queen in next turn
evalValue(normal, canHitNormal, 1700). %stone can hit a normal stone in next turn
evalValue(normal, canHitQueen, 1900). %stone can hit a queen in next turn
evalValue(queen, normal, 2000). %nothing special at queen position
evalValue(queen, canHitNormal, 2700). %queen can hit a normal Stone in next turn
evalValue(queen, canHitQueen, 2900). %queen can hit a queen in next turn
