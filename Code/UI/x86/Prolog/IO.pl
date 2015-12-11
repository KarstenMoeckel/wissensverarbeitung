% author: Robert Maas
% Datum: 09.12.2015

:- dynamic currentField/1.

logMessage(Message) :-
   (
      (
         history(List) ->
         retractall(history(_)),
         List_n = [Message | List],
         assertz(history(List_n))
      )
      ;
      (
         not(history(_)) ->
         assertz(history([Message]))
      )
   ),
   (not(historyUpdated) -> assert(historyUpdated); true).

loadStartPos(File) :-
   retractall(stone(_,_,_)),
   retractall(currentField(_)),
   assertz(currentField(field(1,1))),
   open(File, read, Str),
   repeat,
      get_char(Str, Char),
      currentField(Field),
      not(processChar(Char, Field)),
   close(Str),
   !,
   (not(stonesUpdated)-> assertz(stonesUpdated); true),
   logMessage('Die StartPositionen wurden erfolgreich geladen').

processChar(end_of_file, _) :- !, fail.
processChar('_', _) :- %empty field
   incrementField(col).
processChar('b', Field) :- %black stone
   field(Field,black),
   assertz(stone(Field,black,normal)),
   incrementField(col).
processChar('w', Field) :- %white stone
   field(Field,black),
   assertz(stone(Field,white,normal)),
   incrementField(col).
processChar('B', Field) :- %black queen
   field(Field,black),
   assertz(stone(Field,black,queen)),
   incrementField(col).
processChar('W', Field) :- %white queen
   field(Field,black),
   assertz(stone(Field,white,queen)),
   incrementField(col).
processChar(Char, _) :-
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