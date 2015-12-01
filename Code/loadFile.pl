% author: Robert Maas
% Datum: 01.12.2015

:- dynamic currentRow/1.
:- dynamic currentCol/1.

loadStartPos(File) :-
   retractall(stone(_,_,_,_)),
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
         Char == '_',
         !,
         incrementCol
      )
      ;
      (
         %black stone
         Char == b,
         !,
         field(Row,Col,black),
         assertz(stone(Row,Col,black,normal)),
         incrementCol
      )
      ;
      (
         %white stone
         Char == w,
         !,
         field(Row,Col,black),
         assertz(stone(Row,Col,white,normal)),
         incrementCol
      )
      ;
      (
         %black queen
         Char == 'B',
         !,
         field(Row,Col,black),
         assertz(stone(Row,Col,black,queen)),
         incrementCol
      )
      ;
      (
         %white queen
         Char == 'W',
         !,
         field(Row,Col,black),
         assertz(stone(Row,Col,white,queen)),
         incrementCol
      )
      ;
      (
         %end of line
         (Char == '\r'; Char == '\n'),
         !,
         incrementRow,
         retractall(currentCol(_)),
         assertz(currentCol(1))
      )
      ;
      (
         write('invalid char: '),
         write(Char),
         write(' '),
         atom_codes(Char,Code),
         writeln(Code)
      )
   ),
   fail.
   
incrementRow :-
   currentRow(Row),
   currentCol(Col),
   Col \== 1,
   retract(currentRow(Row)),
   Row_new is Row + 1,
   numbers(Row_new),
   assertz(currentRow(Row_new)).
   
incrementCol :-
   currentCol(Col),
   retract(currentCol(Col)),
   Col_new is Col + 1,
   assertz(currentCol(Col_new)).
   