% author: Robert Maas
% Datum: 02.12.2015

:- dynamic currentRow/1.
:- dynamic currentCol/1.

logMessage(Message) :-
   history(List),
   retractall(history(_)),
   List_n = [Message | List],
   assertz(history(List_n)),
   updateHistory.

loadStartPos(File) :-
   retractall(stone(_,_,_,_)),
   retractall(currentRow(_)),
   retractall(currentCol(_)),
   assertz(currentRow(1)),
   assertz(currentCol(1)),
   open(File, read, Str),
   repeat,
   get_char(Str, Char),
   processChar(Char),
   close(Str).

processChar(end_of_file) :- !.
processChar(Char) :-
   currentCol(Col),
   currentRow(Row),
   (
      (
         %empty field
         Char == '_' ->
            incrementCol,
            fail
      )
      ;
      (
         %black stone
         Char == b ->
         (
         field(Row,Col,black),
         assertz(stone(Row,Col,black,normal)),
         incrementCol,
         fail
         )
      )
      ;
      (
         %white stone
         Char == w ->
         (
            field(Row,Col,black),
            assertz(stone(Row,Col,white,normal)),
            incrementCol,
            fail
         )
      )
      ;
      (
         %black queen
         Char == 'B' ->
         (
            field(Row,Col,black),
            assertz(stone(Row,Col,black,queen)),
            incrementCol,
            fail
         )
      )
      ;
      (
         %white queen
         Char == 'W' ->
         (
            field(Row,Col,black),
            assertz(stone(Row,Col,white,queen)),
            incrementCol,
            fail
         )
      )
      ;
      (
         %end of line
         (Char == '\r'; Char == '\n') ->
         (
            incrementRow,
            fail
         )
      )
      ;
      (
         !,
         atom_concat('invalid char: ', Char, Output),
         logMessage(Output)
      )
   ).
   
incrementRow :-
   currentRow(Row),
   currentCol(Col),
   Col \== 1, %prevents double excecution
   retract(currentRow(Row)),
   Row_new is Row + 1,
   numbers(Row_new),
   assertz(currentRow(Row_new)),
   retractall(currentCol(_)),
   assertz(currentCol(1)).
   
incrementCol :-
   currentCol(Col),
   retract(currentCol(Col)),
   Col_new is Col + 1,
   assertz(currentCol(Col_new)).