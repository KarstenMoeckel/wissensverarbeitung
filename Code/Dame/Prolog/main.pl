% Autor: Robert Maas
% Datum: 22.12.2015

:- use_module(game).
:- use_module(ai).

getLog(Logs) :- game:getLogs(Logs).

move(Source, Direction) :-
   game:move(Source,Direction,Destination) ->
      game:performMove(Source,Destination).

getStoneList(Stones) :- game:getStones(Stones).

loadStartPos(File) :-
   open(File, read, Stream),
   game:loadFile(Stream),
   close(Stream),
   atom_concat('Die Datei ', File, Message_tmp),
   atom_concat(Message_tmp, ' wurde erfolgreich geladen.', Message),
   game:logMessage(Message).