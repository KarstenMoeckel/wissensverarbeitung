/** <rulez>

Module realize the rules of checkers.

@author: Robert Maas and Christian Schuett
@date: 22.12.2015
*/

:- module(rulez, [
     moveDirections/2,
     isMoveValid/4, %call: +World, +Stone, +Direction, -Destination
     canTransformIntoKing/1,
     canHit/3, %call:+World,+Hitter, -HitTree
     isEnemy/2, %call: +Player, -Enemy
     isGameOver/2, %call: +World, --Winner
     simulateMove/5
     ]).

:- use_module(game).
:- use_module(tree).
:- use_module(board).

:- dynamic possibleHit/2.

/**
 * Predicate to check if a player won.
 * @param World The current world.
 * @return Winner If there is a winner, color will be returned else fail.
 */
isGameOver(World,Winner) :-
   stoneCount(World,WhiteCount,BlackCount),
   (
      WhiteCount == 0 ->
         Winner = black
      ;
      BlackCount == 0 ->
         Winner = white
      ;
      fail
   ).

/**
 * Predicate counts the stones of all players.
 * @param World List of all stones.
 * @return WhiteCount The number of white stones.
 * @return BlackCount The number of black stones.
 */
stoneCount([], 0, 0).
stoneCount([stone(_,Color,_)|World], WhiteCount,BlackCount) :-
   stoneCount(World,TmpWhite,TmpBlack),
   (
      Color == black ->
         WhiteCount = TmpWhite,
         BlackCount is TmpBlack + 1
      ;
         WhiteCount is TmpWhite + 1,
         BlackCount = TmpBlack
   ).

/**
 * Predicate returns the opposite player.
 * @param Player
 * @return Enemy
 */
isEnemy(Player,Enemy) :-
   Player == black ->
      Enemy = white
   ;
   Player == white ->
      Enemy = black.

/**
 * Checks if a move in specific direction is valid.
 *
 * @param World The world to look in.
 * @param Stone The stone to move.
 * @param Direction The direction to move.
 * @return Destination The field the stone will be.
 */
isMoveValid(World,Stone,Direction,Destination):-
   moveDirections(Stone,Direction),
   Stone = stone(SField,_,_),

   board:hasRelation(SField,DField,Direction),
   (
         board:isFree(World,DField) ->
            Destination = DField
      ;
         board:hasRelation(DField,JumpTarget,Direction),
         board:isFree(World,JumpTarget) ->
            board:stoneAt(World,DField,Victim),
            canHit(World,Stone,Victim,Direction),
            Destination = JumpTarget
   ),
   board:field(Destination,black).

/**
 * Predicate to check if stone can turn into a king after a move.
 * Stones can turn into kings, when they reache the enemies first row.
 * @param Stone The stone to check.
 * @param true if can turn to king, else false.
 */
canTransformIntoKing(stone(_,_,king)) :- fail.
canTransformIntoKing(stone(field(Row,_),Color,normal)) :-
   game:player(StartPos,Color),
   (
      StartPos == top ->
         Row == 8
      ;
      StartPos == bottom ->
         Row == 1
   ).


%call: +Stone, -Direction
/**
 * Predicate to get all possible move directions of a Stone.
 * @param Stone
 * @return Direction The move direction.
 */
moveDirections(stone(field(Row,Col),_,king), Direction):-
      Row \==8,
      bottomDirections(Col, Direction)
   ;
      Row \== 1,
      topDirections(Col, Direction).

moveDirections(stone(field(_,Col),Color,normal), Direction) :-
   player(Position, Color),
   (
         Position == top -> bottomDirections(Col, Direction)
      ;
         Position == bottom -> topDirections(Col, Direction)
   ).

topDirections(Col, Direction) :-
      Col \== 1,
      Direction = topLeft
   ;
      Col \==8,
      Direction = topRight.

bottomDirections(Col, Direction) :-
      Col \== 1,
      Direction = bottomLeft
   ;
      Col \==8,
      Direction = bottomRight.

/**
 * Predicate checks if Stone can hit or multihit an enemy stone.
 * @param World The world the Stone is in.
 * @param Hitter The stone that may can hit.
 * @return HitTree List of moves the stone can make to hit enemy stones.
 */
canHit(World,Hitter,HitTree) :-
   canMultiHit(World,Hitter,1,[],HitTree).

canMultiHit(World,Hitter,CurLevel,Call,Tree):-
    NextLevel is CurLevel + 1,
    repeat,
    (
        hasHitPossibility(World,Hitter,NewHitter,NewWorld,NewCall),
        canMultiHit(NewWorld,NewHitter,NextLevel,NewCall, Node),
        assert(possibleHit(CurLevel,Node)),
        fail
    ;
        !
    ),
    findall(Node,possibleHit(CurLevel,Node),Nodes),
    retractall(possibleHit(CurLevel,_)),
    tree:createNode(hit(World,Call),Nodes,Tree).

%call: +World, +Hitter, -NewHitter, NewWorld, -Call
/**
 * Predicate checks all hitpossibilities for a stone.
 * @param World List of all stones.
 * @param Hitter The stone that can hit.
 * @return NewHitter The hitter after it moved.
 * @return NewWorld
 * @return Call
 */
hasHitPossibility(World,Hitter,NewHitter,NewWorld,Call) :-
  rulez:moveDirections(Hitter,Direction),
  Hitter = stone(Field,_,_),
  rulez:isMoveValid(World,Hitter,Direction,Destination),
  board:isFieldBetween(Field, Destination,_),
  simulateMove(World,Hitter,Destination,NewWorld, Call),
  board:stoneAt(NewWorld,Destination,NewHitter).

%call: +World, +Hitter, +Victim, +Relation
%Relation: <Victim> is <Relation> of <Hitter>
canHit(World, Hitter, stone(VField,VColor,_), Relation) :-
   not(Hitter = stone(_,VColor,_)),
   moveDirections(Hitter, Relation),
   board:hasRelation(VField,Destination,Relation),
   game:field(Destination,black),
   board:isFree(World,Destination).

/**
 * Predicate to remove a stone from the knowledge base after it got beaten up.
 * @param World List of all stones.
 * @param OverjumpedField The field the stone could be on.
 * @return NewWorld The world without the stone.
 */
removeOverJumpedStone(World,OverjumpedField,NewWorld):-
    board:stoneAt(World,OverjumpedField,Stone),
    subtract(World,[Stone],NewWorld).

/**
 * Predicate simulates a move to check if the Hitter can
 * hit another stone -> multihit
 * @param World List of all stones.
 * @param Stone The hitter.
 * @param Destination The move direction
 * @return NewWorld The world after the move.
 * @return Call The list of moves that is necessary to reach tthe new world.
 */
simulateMove(World,Stone,Destination,NewWorld, Call) :-
    subtract(World,[Stone],TmpWorld),
    Stone = stone(Source,Color,Mode),
    (
       board:isFieldBetween(Source,Destination,Between)->
           removeOverJumpedStone(TmpWorld,Between,Tmp2World)
       ;
          Tmp2World = TmpWorld
    ),
    TmpStone = stone(Destination,Color,Mode),
    (
        canTransformIntoKing(TmpStone) ->
            NewStone = stone(Destination,Color,king)
        ;
            NewStone = TmpStone
    ),
    NewWorld = [NewStone | Tmp2World],
    Call =.. [performMove, Source,Destination].
