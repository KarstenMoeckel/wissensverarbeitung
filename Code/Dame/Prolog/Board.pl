% Autor:
% Datum: 16.12.2015

:- module(board, [
    field/2]).
    
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