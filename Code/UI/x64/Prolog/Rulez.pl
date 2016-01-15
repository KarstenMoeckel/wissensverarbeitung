% Autor: Robert Maas
% Datum: 22.12.2015

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

isEnemy(Player,Enemy) :-
   Player == black ->
      Enemy = white
   ;
   Player == white ->
      Enemy = black.


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

removeOverJumpedStone(World,OverjumpedField,NewWorld):-
    board:stoneAt(World,OverjumpedField,Stone),
    subtract(World,[Stone],NewWorld).

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
