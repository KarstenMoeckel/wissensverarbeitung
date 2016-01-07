% Autor: Christian Schuett
% Datum: 30.12.2015

:- module(ai, [
     updateTreeDepth/1,
     findPossibleMoves/3
     ]).

:- use_module(search).
:- use_module(evaluation).
:- use_module(game).
:- use_module(rulez).
:- use_module(tree).
:- use_module(board).

:- dynamic searchTree/1.

updateTreeDepth(Depth):-
	retractall(treeDepth(_)),
	assertz(stone(Depth)).




findPossibleMoves(Player,Tree,PossibleMoves):-
    Tree = t(World,Childs),

    retractall(searchTree(_)),
    assertz(searchTree(Tree)),

   	member(Stone,World),
    (
      Stone = stone(field(X,Y),Color,Mode),
      %right color?
      Color = Player,

      rulez:canHit(World,Stone,HitTree),

      HitTree = t(Hitter,PossibleHits),


      maplist(unKnown2(Hitter, World), PossibleHits)
      /*member(Item,PossibleHits),
	  (
			( 	


				Known(Hitter, [Item], World, ResultWorld),

				searchTree(CurrTree),
				
				tree:appendTree(World, ResultWorld, CurrTree, NewTree),
				
				retractall(searchTree(_)),
				
				assertz(searchTree(NewTree))
			)
			
			%,fail
      )*/
     )
    .

unKnown2(Hitter, World,  Item) :-
	unKnown(Hitter, [Item], World, ResultWorld),

	searchTree(CurrTree),
				
	tree:appendTree(World, ResultWorld, CurrTree, NewTree),
			
	retractall(searchTree(_)),
				
	assertz(searchTree(NewTree)).


unKnown(_,[], World, ResultWorld):- append([],World,ResultWorld).
unKnown(Hitter, [HitTree], World, ResultWorld) :-	
	
	HitTree = t(Victim,Childs),
    Hitter = stone(HField,Color,Mode),

    subtract(World, [Victim], RemovedStoneFromWorld),
 	subtract(RemovedStoneFromWorld, [Hitter], RMStoneWorld2),

    Victim = stone(VField,_,_),

    hasRelation(HField,VField,Relation),

    game:move(HField,World,Relation,Destination),

    append(RMStoneWorld2, [stone(Destination,Color,Mode)], NewWorld),
  	
	unKnown(stone(Destination,Color,Mode),Childs,NewWorld,ResultWorld)
	.























/*
      %find all possible movingdirections without hitting
      findall(Directions, moveDirections(Stone, Directions), PossibleDirections),
       
      member(Direction,PossibleDirections),
      (
            %notes: -aware the possiblity for turning stones into kings
            game:move(field(X,Y),Direction,Destination),
            %checking for moves which does involves jump
            not(board:isFieldBetween(field(X,Y),Destination)),

            %remove stone from the old position
            subtract(World, [Stone], RemovedStoneFromWorld),
            %set stone to new position
            append(RemovedStoneFromWorld, [stone(field(X,Y),Color,Mode)], NewWorld)
            ,
            fail  
      )
      ,
      fail
   ).*/

