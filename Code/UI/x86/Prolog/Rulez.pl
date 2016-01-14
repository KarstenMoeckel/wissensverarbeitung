% Autor: Robert Maas
% Datum: 22.12.2015

:- module(rulez, [
     moveDirections/2,
     isMoveValid/4, %call: +World, +Stone, +Direction, -Destination
     canTransformIntoKing/1,
     canHit/3, %call:+World,+Hitter, -HitTree
     isEnemy/2, %call: +Player, -Enemy
     isGameOver/2 %call: +World, --Winner
     ]).

:- use_module(game).
:- use_module(tree).
:- use_module(board).

isGameOver(World,Winner) :-
   stoneCount(World,WhiteCount,BlackCount),
   (
      WhiteCount == 0 ->
         Winner = black
      ;
      BlackCount == 0 ->
         Winner == white
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
   canMultiHit(World,Hitter,_,_,HitTree).

canMultiHit(World,Hitter,PreviousVictim,CurTree,HitTree) :-
   board:hasNeighbour(World,Hitter,Neighbours1),
   (
      var(PreviousVictim) ->
         Neighbours = Neighbours1,
         PreviousVictim = Hitter,
         tree:appendTree(_,Hitter,_,CurTree)
      ;
         subtract(Neighbours1, [PreviousVictim,_], Neighbours)
   ),
   hasHitPossibility(World,Hitter, Neighbours,Victims),
   hasFurtherHits(World,CurTree,Hitter, PreviousVictim, Victims, HitTree).

hasFurtherHits(World, CurTree,Hitter, PrevVictim,Victims, NewTree) :-
   tree:appendTree(PrevVictim,Victim,CurTree,TmpTree),
   (
         Victims = [] -> NewTree = CurTree
      ;
         Victims = [[Victim,Relation] |OtherVictims],
         Hitter = stone(_,Color,Type),
         Victim = stone(Field,_,_),
         board:hasRelation(Field,Destination,Relation),
         canMultiHit(World,stone(Destination,Color,Type), Victim ,TmpTree, Tree),
         hasFurtherHits(World,Tree,Hitter,PrevVictim,OtherVictims,NewTree)
   ).

addStonesToTree(CurTree, _,[],CurTree).
addStonesToTree(CurTree, Parent, [Stone|Stones],NewTree) :-
   tree:appendTree(Parent,Stone,CurTree,TmpTree),
   addStonesToTree(TmpTree,Parent,Stones,NewTree).

%call: +World, +Hitter, +Neighbours, --Victims
hasHitPossibility(_,_,[],[]).
hasHitPossibility(World,Hitter, [[Victim,Relation]|Neighbours],Victims) :-
   hasHitPossibility(World,Hitter,Neighbours,FoundVictims),
   (
      canHit(World,Hitter,Victim,Relation)->
         Victims = [[Victim,Relation]|FoundVictims]
      ;
         Victims = FoundVictims
   ).

%call: +World, +Hitter, +Victim, +Relation
%Relation: <Victim> is <Relation> of <Hitter>
canHit(World, Hitter, stone(VField,VColor,_), Relation) :-
   not(Hitter = stone(_,VColor,_)),
   moveDirections(Hitter, Relation),
   board:hasRelation(VField,TField,Relation),
   game:field(TField,black),
   board:isFree(World,TField).
