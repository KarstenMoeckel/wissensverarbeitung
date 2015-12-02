% Autor:
% Datum: 26.11.2015

init :-
   consult('knowledgeBase'),
   consult('IO'),
   consult('GUI'),
   consult('rules'),
   consult('utility'),
   writeln('startGUI.'),
   writeln('updateStonePos.').
