% Autor: Robert Maas
% Datum: 02.01.2016

:- module('game',[
     createStoneList/1, %call: -World
                        %World: all stones in a list
     logMessage/1, %call: +Message
     getLogs/1, %call: -Logs
     getStones/1, %call: -Stones
     loadFile/1, %call: +Stream
     player/2, %player(StartPosition,Color)
     performMove/2, %call:+Source, +Destination
     move/3, % call: +SourceField, + Direction, - DestinationField
     %%move/4, % call: +SourceField, +World + Direction, - DestinationField

     stoneAt/2 %call: +SourceField, -Stone
     ]).

:- use_module(board).
:- use_module(rulez).

player(top, black).
player(bottom,white).

:- dynamic stonesUpdated/0.

:- dynamic stone/3.
%stone(field(2,3),black,normal).
%stone(field(3,4),black,normal).
%stone(field(4,5),white,normal).
%stone(field(4,3),white,normal).
%stone(field(6,5),white,normal).
%add by Tristan
%stone(field(6,5),white,normal).

%stone(field(8,1),black,king).
%stone(field(2,7),white,king).

%stone(field(1,2),black,normal).

%stone(field(8,7),white,normal).

stone(field(1,2),black,normal).
stone(field(1,4),black,normal).
stone(field(1,6),black,normal).
stone(field(1,8),black,normal).
stone(field(2,1),black,normal).
stone(field(2,3),black,normal).
stone(field(2,5),black,normal).
stone(field(2,7),black,normal).
stone(field(3,2),black,normal).
stone(field(3,4),black,normal).
stone(field(3,6),black,normal).
stone(field(3,8),black,normal).

stone(field(6,1),white,normal).
stone(field(6,3),white,normal).
stone(field(6,5),white,normal).
stone(field(6,7),white,normal).
stone(field(7,2),white,normal).
stone(field(7,4),white,normal).
stone(field(7,6),white,normal).
stone(field(7,8),white,normal).
stone(field(8,1),white,normal).
stone(field(8,3),white,normal).
stone(field(8,5),white,normal).
stone(field(8,7),white,normal).

createStoneList(List) :- findall(stone(Field,Color,Type), stone(Field,Color,Type),List).

stoneAt(Field,Stone) :-
   stone(Field,Color,Type),
   Stone = stone(Field,Color,Type).

move(Source,Direction,Destination) :-
   stone(Source,Color,Type),
   createStoneList(World),
   rulez:isMoveValid(World, stone(Source,Color,Type),Direction, Destination).

performMove(Source,Destination) :-
   stone(Source,Color,Type),
   retract(stone(Source,Color,Type)),
   (
      rulez:canTransformIntoKing(stone(Destination,Color,Type)) ->
         assertz(stone(Destination,Color,king))
      ;
         assertz(stone(Destination,Color,Type))
   ),
   (
      not(board:hasRelation(Source,Destination,_))->
         board:isFieldBetween(Source,Destination,Between),
         retract(stone(Between,_,_)) %remove overjumped stone
      ;
      true
   ),
   (
      not(stonesUpdated) -> assertz(stonesUpdated)
      ;
      true
   ).

getStones(Stones) :-
   stonesUpdated->
      createStoneList(Stones),
      retract(stonesUpdated).

%----------------------Logging---------------------------------------------
:- dynamic logUpdated/0.
:- dynamic logs/1.

logMessage(Message) :-
   (
      logs(Logs) ->
         retractall(logs(_)),
         assertz(logs([Message|Logs]))
      ;
         assertz(logs([Message]))
   ),
   (
      not(logUpdated)-> assertz(logUpdated)
      ;
         true
   ).

getLogs(Logs) :-
   logUpdated->
      logs(Logs),
      retract(logUpdated).
%----------------------End Logging----------------------------------------

loadFile(Stream) :-
   retractall(stone(_,_,_)),
   retractall(currentField(_)),
   assert(currentField(field(1,1))),
   repeat,
      get_char(Stream, Char),
      not(processChar(Char)),
   !,
   (
      Char == end_of_file ->
          (
             not(stonesUpdated) ->
                assert(stonesUpdated)
            ;
            true
          )
     ;
        fail
   ).

processChar(end_of_file) :- !, fail.
processChar('_') :- %empty field
   incrementField(col).
processChar('b') :- %black stone
   currentField(Field),
   board:field(Field,black),
   assertz(stone(Field,black,normal)),
   incrementField(col).
processChar('w') :- %white stone
   currentField(Field),
   board:field(Field,black),
   assertz(stone(Field,white,normal)),
   incrementField(col).
processChar('B') :- %black king
   currentField(Field),
   board:field(Field,black),
   assertz(stone(Field,black,king)),
   incrementField(col).
processChar('W') :- %white king
   currentField(Field),
   board:field(Field,black),
   assertz(stone(Field,white,king)),
   incrementField(col).
processChar(Char) :-
   %end of line
   (Char == '\r'; Char == '\n') ->
      incrementField(row)
   ;
   (
      atom_concat('invalid char: ', Char, Output),
      logMessage(Output),
      fail
   ).

incrementField(col) :-
   currentField(field(Row,Col)),
   retractall(currentField(_)),
   Col2 is Col + 1,
   assertz(currentField(field(Row,Col2))).
incrementField(row) :-
   currentField(field(Row,Col)),
   Col == 9, %prevents double excecution
   retractall(currentField(_)),
   Row2 is Row + 1,
   assertz(currentField(field(Row2,1))).
