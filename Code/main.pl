% Autor:
% Datum: 26.11.2015

init :-
   consult('field'),
   consult('GUI'),
   consult('rules'),
   consult('utility'),
   writeln('startGUI.'),
   writeln('updateStonePos.').
