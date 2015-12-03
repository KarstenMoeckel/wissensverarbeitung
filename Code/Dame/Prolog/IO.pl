% author: Robert Maas
% Datum: 03.12.2015

:- dynamic currentRow/1.
:- dynamic currentCol/1.

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
   retractall(stone(_,_,_,_)),
   retractall(currentRow(_)),
   retractall(currentCol(_)),
   assertz(currentRow(1)),
   assertz(currentCol(1)),
   open(File, read, Str),
   repeat,
      get_char(Str, Char),
      currentRow(Row),
      currentCol(Col),
      not(processChar(Char, Row, Col)),
   close(Str),
   !,
   (not(stonesUpdated)-> assertz(stonesUpdated); true),
   logMessage('Die StartPositionen wurden erfolgreich geladen').

processChar(end_of_file, _, _) :- !, fail.
processChar('_', _, Col) :- %empty field
   incrementCol(Col).
processChar('b', Row, Col) :- %black stone
   field(Row,Col,black),
   assertz(stone(Row,Col,black,normal)),
   incrementCol(Col).
processChar('w', Row, Col) :- %white stone
   field(Row,Col,black),
   assertz(stone(Row,Col,white,normal)),
   incrementCol(Col).
processChar('B', Row, Col) :- %black queen
   field(Row,Col,black),
   assertz(stone(Row,Col,black,queen)),
   incrementCol(Col).
processChar('W', Row, Col) :- %white queen
   field(Row,Col,black),
   assertz(stone(Row,Col,white,queen)),
   incrementCol(Col).
processChar(Char, Row, Col) :-
   %end of line
   (Char == '\r'; Char == '\n') ->
   incrementRow(Row,Col)
   ;
   (
      atom_concat('invalid char: ', Char, Output),
      logMessage(Output),
      fail
   ).
   
incrementRow(Row, Col) :-
   Col == 9 -> %prevents double excecution
   retract(currentRow(Row)),
   Row_new is Row + 1,
   numbers(Row_new),
   assertz(currentRow(Row_new)),
   retractall(currentCol(_)),
   assertz(currentCol(1)).
   
incrementCol(Col) :-
   retract(currentCol(Col)),
   Col_new is Col + 1,
   assertz(currentCol(Col_new)).