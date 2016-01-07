% Autor: Robert Maas
% Datum: 02.01.2016

:- use_module(game).
:- use_module(search).
:- use_module(ai).
:- use_module(tree).

:- dynamic player/1.
:- dynamic turn/1.
:- dynamic gameRunning/0.

getLog(Logs) :- game:getLogs(Logs).

move(Source, Direction, Destination) :-
   gameRunning,
   player(Player),
   turn(Player),
   game:move(Source,Direction,Destination) ->
      game:performMove(Source,Destination).

moreMovesPossible(Source, PossibleHits) :-
   game:createStoneList(World),
   game:stoneAt(Source,Stone),
   rulez:canHit(World, Stone, HitTree),
   not(tree:isLeaf(HitTree)),
   search:membersOfLevel(HitTree,2,HitStones),
   getFields(HitStones,PossibleHits).

getFields([],[]).
getFields([stone(Field,_,_)| StoneList],FieldList) :-
   getFields(StoneList,TmpList),
   FieldList = [Field | TmpList].

getStoneList(Stones) :- game:getStones(Stones).

loadStartPos(File) :-
   not(gameRunning),
   open(File, read, Stream),
   game:loadFile(Stream),
   close(Stream),
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
   
nextTurn :-
   gameRunning,
   (
      rulez:isGameOver(Winner) ->
         (
            Winner == black ->
               Message = 'schwarz hat gewonnen.'
            ;
            Winner == white ->
               Message = 'weiss hat gewonnen.'
         ),
         game:logMessage(Message),
         retract(gameRunning)
      ;
      (
         turn(Player),
         rulez:isEnemy(Player,Enemy),
         retract(turn(Player)),
         assert(turn(Enemy))
      )
   ).