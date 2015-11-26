% author: Robert Maas
% Datum: 24.11.2015

field(Row,Col,Color) :-
   numbers(Row),
   numbers(Col),
   Sum is Row + Col,
   R is Sum mod 2,
   (
      R==1,
      Color='black',
      !
   )
   ;
   Color='white'.
   
% legend(Stonecolor,Type, Char).
legend(black,normal,'b').
legend(black,queen,'B').
legend(white, normal, 'w').
legend(white,queen, 'W').

% stone(Row,Col,StoneColor,Type).
:- dynamic stone/4.
stone(3,4,black,normal).
stone(8,4,white,normal).
stone(8,1,black,queen).
stone(7,7,white,queen).

numbers(X) :- between(1,8,X).

drawLegend :-
   writeln('Legende:'),
   legend(black,normal,Char1),
   write('   schwarz: '),
   writeln(Char1),
   legend(black,queen,Char2),
   write('   schwarze Dame: '),
   writeln(Char2),
   legend(white,normal,Char3),
   write('   weiss: '),
   writeln(Char3),
   legend(white,queen,Char4),
   write('   weisse Dame: '),
   writeln(Char4).

printGame(legend) :-
   drawLegend,
   writeln(''),
   printGame.

printGame :-
   writeln(' 1 2 3 4 5 6 7 8 '),
   writeln(' _ _ _ _ _ _ _ _ '),
   (
      numbers(X),
      drawRow(X),
      fail
   )
   ;
      true.
   
drawRow(Row) :-
   (
      numbers(X),
      write('|'),
      drawField(Row,X),fail
   )
   ;
      write('| '),
      writeln(Row).

drawField(Row,Col) :-
   (
      stone(Row,Col,Color,Lady),
      legend(Color,Lady,Char),
      write(Char),
      !
   )
   ;
%      field(Row,Col, FieldColor),
%      (
%         FieldColor == black,
%         write('x'),
%         !
%      )
%      ;
         write('_').

loadStartPos(File) :-
   retractall(stone(_,_,_,_)),
   open(File, read, Str),
   repeat,
      read(Str,Term),
      process_and_fail(Term),
   close(Str).

process_and_fail(end_of_file) :- !.
process_and_fail(Term) :-
      (
         checkTerm(Term),
         !,
         assertz(Term),
         fail
      )
   ;
      (
         write('invalid Term: '),
         writeln(Term),
         writeln('abort reading file')
      ).
checkTerm(Term) :-
   Term =.. [Pred, Row,Col,Color,Type],
   Pred == stone,
   field(Row,Col,black),
   (
      (
         Color = 'black',
         !
      )
      ;
         Color = 'white'
   )
   ,
   (
      (
         Type = 'normal',
         !
      )
      ;
      Type = 'queen'
   ).