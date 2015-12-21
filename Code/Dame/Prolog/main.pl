% Autor: Robert Maas
% Datum: 21.12.2015

:- use_module(game).

getLog(Logs) :- game:getLogs(Logs).

getStoneList(Stones) :- game:getStones(Stones).

loadStartPos(File) :-
   open(File, read, Stream),
   game:loadFile(Stream),
   close(Stream),
   atom_concat('Die Datei ', File, Message_tmp),
   atom_concat(Message_tmp, ' wurde erfolgreich geladen.', Message),
   game:logMessage(Message).