﻿% Autor: Robert Maas
% Datum: 04.01.2016

:- module(main, [
   getLog/1,
   moveStone/3,
   areMoreHitsPossible/2,
   getStoneList/1,
   loadStartPos/1,
   option/2,
   nextTurn/0,
   startGame/0,
   isAIMove/0,
   aiNextMove/0,
   performAiMove/0
   ]).

:- use_module(game).
:- use_module(search).
:- use_module(ai).

:- dynamic player/1.
:- dynamic turn/1.
:- dynamic gameRunning/0.
:- dynamic stonesLoaded/0.
:- dynamic hitMove/1.
:- dynamic aiMove/1.

getLog(Logs) :- game:getLogs(Logs).

moveStone(Source, Direction, Destination) :-
   isGameRunning,
   isPlayerTurn,
   hasFieldStone(Source,Stone),
   isPlayerStone(Stone),
   (
      hitMove(CheckStone) ->
         (
            CheckStone = stone(Source,_,_) ->
               retract(hitMove(_)),
               doMove(hitForce,Source, Direction, Destination)
            ;
               game:logMessage('Es muss mit dem gleichem Stein weitergespielt werden.'),
               fail
         )

      ;
         doMove(noHitForce,Source, Direction, Destination)
   ),
   (
      board:isFieldBetween(Source,Destination,_) ->
         assert(hitMove(stone(Destination,_,_)))
      ;
      true
   ).

isAIMove :-
   player(Color),
   not(turn(Color)).

hasFieldStone(Field,Stone) :-
   game:stoneAt(Field,Stone) ->
      true
   ;
      game:logMessage('An dem Feld ist kein Stein.'),
      fail.

doMove(hitForce,Source, Direction, Destination) :-
   game:move(Source,Direction,Destination),
   board:isFieldBetween(Source,Destination,_) ->
      game:performMove(Source,Destination)
   ;
   game:logMessage('Der Zug ist ungültig.'),
   fail.

doMove(noHitForce,Source, Direction, Destination) :-
   game:move(Source,Direction,Destination) ->
      game:performMove(Source,Destination)
   ;
   game:logMessage('Der Zug ist ungültig.'),
   fail.

isPlayerStone(stone(_,Color,_)) :-
   player(Color)->
         true
   ;
      game:logMessage('Das war nicht die Spielerfarbe'),
      fail.

isPlayerTurn :-
   (
      player(Player),
      turn(Player)
   ) ->
      true
   ;
   game:logMessage('Der menschliche Spieler ist nicht am Zug.'),
   fail.

isGameRunning :-
   gameRunning ->
      true
   ;
   game:logMessage('Das Spiel wurde nicht gestartet.'),
   fail.

areMoreHitsPossible(Source, PossibleHits) :-
   hitMove(_),
   game:createStoneList(World),
   game:stoneAt(Source,Stone),
   rulez:canHit(World, Stone, HitTree),
   (
      tree:isLeaf(HitTree) ->
         retractall(hitMove(_)),
         fail
      ;
         search:membersOfLevel(HitTree,2,HitStones),
         getFields(HitStones,PossibleHits)
   ).

getFields([],[]).
getFields([hit(_,Call)| Hits],[VField | TmpList]) :-
   getFields(Hits,TmpList),
   Call =.. [performMove,Source,Destination],
   board:isFieldBetween(Source,Destination,VField).

getStoneList(Stones) :- game:getStones(Stones).

loadStartPos(File) :-
   not(gameRunning),
   open(File, read, Stream),
   (
      game:loadFile(Stream) ->
         close(Stream)
      ;
         close(Stream),
         fail
   ),
   (
      not(stonesLoaded) ->
         assert(stonesLoaded)
      ;
      true
   ),
   atom_concat('Die Datei ', File, Message_tmp),
   atom_concat(Message_tmp, ' wurde erfolgreich geladen.', Message),
   game:logMessage(Message).

option(treeDepth, Depth) :-
   not(gameRunning),
   number(Depth),
   ai:updateTreeDepth(Depth).

option(playerColor, Color) :-
   game:player(_, Color),
   not(gameRunning),
   retractall(player(_)),
   assert(player(Color)),
   rulez:isEnemy(Color,Enemy),
   ai:updateAIPlayer(Enemy).

option(startColor, Color) :-
   game:player(_, Color),
   not(gameRunning),
   retractall(turn(_)),
   assert(turn(Color)).

hasPlayerWon :-
   game:createStoneList(World),
   rulez:isGameOver(World,Winner) ->
   (
      Winner == black ->
         Message = 'schwarz hat gewonnen.'
      ;
      Winner == white ->
         Message = 'weiss hat gewonnen.'
   ),
   game:logMessage(Message),
   retract(gameRunning).

logCurrentPlayer :-
   turn(white) ->
      game:logMessage('weiss ist am Zug.')
   ;
      game:logMessage('schwarz ist am Zug.').

changeTurn :-
   turn(Player),
   rulez:isEnemy(Player,Enemy),
   retract(turn(Player)),
   assert(turn(Enemy)),
   logCurrentPlayer.

nextTurn :-
   gameRunning,
   (
      not(hitMove(_)) ->
         (
            hasPlayerWon
            ;
            (
               changeTurn,
               not(isAIMove) ->
                  (
                     not(canHumanPlayerMove)->
                        game:logMessage('Der menschliche Spieler kann keinen Zug mehr machen.'),
                        retractall(gameRunning)
                     ;
                        true
                  )

               ;
                  true
            )
         )
      ;
      game:logMessage('Es müssen noch Steine geschlagen werden.'),
         fail
   ).

startGame :-
   (
      stonesLoaded,
      turn(_),
      player(_),
      not(gameRunning)
   ) ->
      retract(stonesLoaded),
      assert(gameRunning),
      game:logMessage('Das Spiel wurde gestartet.'),
      logCurrentPlayer
   ;
      game:logMessage('Das Spiel kann nicht geladen werde.'),
      fail.

canHumanPlayerMove :-
   player(Player),
   moveTreeOfPlayer(Player,Tree),
   not(tree:isLeaf(Tree)).

aiNextMove :-
   ai:nextAiMove(Calls, Value)->
      assert(aiMove(Calls)),
      atom_concat('Der Zugwert de KI beträgt ', Value, LogValue),
      game:logMessage(LogValue)
   ;
   (
      canHumanPlayerMove ->
         game:logMessage('Die KI kann keinen Zug machen.'),
         retractall(gameRunning)
      ;
         game:logMessage('Beide Spieler können keine Züge mehr machen'),
         retractall(gameRunning)
   ).

performAiMove :-
   aiMove([Call | RestCalls]),
   retractall(aiMove(_)),
   call(Call),
   (
      not(RestCalls == []) ->
         assert(aiMove(RestCalls))
      ;
      true
   ).
