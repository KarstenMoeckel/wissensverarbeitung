% Autor: Robert Maas
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
   isAIMove/0
   ]).

:- use_module(game).
:- use_module(search).
:- use_module(ai).

:- dynamic player/1.
:- dynamic turn/1.
:- dynamic gameRunning/0.
:- dynamic usedStone/1.
:- dynamic stonesLoaded/0.
:- dynamic hitMove/0.

getLog(Logs) :- game:getLogs(Logs).

moveStone(Source, Direction, Destination) :-
   isGameRunning,
   isPlayerTurn,
   hasFieldStone(Source,Stone),
   isValidStone(Stone),
   doMove(Source, Direction, Destination),
   (
      usedStone(_) ->
         retract(usedStone(_))
      ;
      true
   ),
   Stone = stone(_,Color,Type),
   assert(usedStone(stone(Destination,Color,Type))),
   isHitMove(Source,Destination).

isHitMove(Source,Destination) :-
   board:isFieldBetween(Source,Destination,_) ->
      (
         not(hitMove)->
            assert(hitMove)
         ;
         true
      )
   ;
   retractall(hitMove).

isValidStone(Stone) :-
   usedStone(Used) ->
      (
         Used == Stone ->
            true
         ;
            game:logMessage('Es muss mit dem gleichem Stein weitergespielt werden.'),
            fail
      )
   ;
      isPlayerStone(Stone).

isAIMove :-
   player(Color),
   not(turn(Color)).

hasFieldStone(Field,Stone) :-
   game:stoneAt(Field,Stone) ->
      true
   ;
      game:logMessage('An dem Feld ist kein Stein.'),
      fail.

doMove(Source, Direction, Destination) :-
   game:move(Source,Direction,Destination) ->
      game:performMove(Source,Destination)
   ;
   game:logMessage('Der Zug ist ung�ltig.'),
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
   hitMove,
   game:createStoneList(World),
   game:stoneAt(Source,Stone),
   rulez:canHit(World, Stone, HitTree),
   search:membersOfLevel(HitTree,2,HitStones),
   (
      HitStones == [] ->
         retractall(usedStone(_)),
         fail
      ;
      getFields(HitStones,PossibleHits)
   ).

getFields([],[]).
getFields([stone(Field,_,_)| StoneList],FieldList) :-
   getFields(StoneList,TmpList),
   FieldList = [Field | TmpList].

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
   assert(player(Color)).
   
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

changeTurn :-
   turn(Player),
   rulez:isEnemy(Player,Enemy),
   retract(turn(Player)),
   assert(turn(Enemy)).
   
nextTurn :-
   isGameRunning,
   (
      not(usedStone(_)) ->
         retractall(hitMove),
         (
            hasPlayerWon
            ;
            changeTurn
         )
      ;
      game:logMessage('Es m�ssen noch Steine geschlagen werden.'),
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
      game:logMessage('Das Spiel wurde gestartet.')
   ;
      game:logMessage('Fehler beim Starten vom Spiel.'),
      fail.