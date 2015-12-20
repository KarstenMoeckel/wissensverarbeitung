% Autor: Robert Maas
% Datum: 20.12.2015

:- module('game',[
     createStoneList/1, %call: -World
                        %World: all stones in a list
     stonesUpdated/0,
     logMessage/1, %call: +Message
     getLogs/1, %call: -Logs
     loadFile/1, %call: +Stream
     player/2, %player(StartPosition,Color)
     performMove/2 %call:+Source, +Destination
     ]).
     
:- use_module(board).
:- use_module(rulez).

player(top, black).
player(bottom,white).

:- dynamic stonesUpdated/0.

:- dynamic stone/3.
stone(field(2,3),black,normal).
stone(field(3,4),black,normal).
stone(field(4,5),white,normal).
stone(field(4,3),white,normal).
stone(field(6,7),white,normal).
stone(field(8,1),black,king).
stone(field(2,7),white,king).

createStoneList(List) :- findall(stone(Field,Color,Type), stone(Field,Color,Type),List).

performMove(Source,Destination) :-
   stone(Source,Color,Type),
   retract(stone(Source,Color,Type)),
   assertz(stone(Destination,Color,Type)),
   (
      not(board:hasRelation(Source,Destination,_))->
         board:isFieldBetween(Source,Destination,Between),
         retract(stone(Between,_,_)) %remove overjumped stone
      ;
      true
   ).

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
   (not(stonesUpdated)-> assertz(stonesUpdated); true).
   
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