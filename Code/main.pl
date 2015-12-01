% Autor:
% Datum: 26.11.2015

init :-
   consult('knowledgeBase'),
   consult('loadFile'),
   consult('GUI'),
   consult('rules'),
   consult('utility'),
   writeln('startGUI.'),
   writeln('updateStonePos.').
