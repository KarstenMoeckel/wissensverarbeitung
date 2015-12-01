% author: Robert Maas
% Datum: 27.11.2015

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

numbers(X) :- between(1,8,X).

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
