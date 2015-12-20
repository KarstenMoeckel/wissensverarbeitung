% Autor: Robert Maas
% Datum: 19.12.2015

:- module(rulez, [
     moveDirections/2,
     canHit/3 %call:+World,+Hitter, -HitTree
     ]).

:- use_module(game).
:- use_module(tree).
:- use_module(board).
     
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

%hasFurterHits(_, CurTree, _,_,[],CurTree).
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