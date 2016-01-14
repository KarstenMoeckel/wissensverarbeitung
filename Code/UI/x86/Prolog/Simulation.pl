% Autor: Christian Schuett
% Datum: 30.12.2015

:- module(simulation, [
     possibleMove/4
     ]).

:- use_module(search).
:- use_module(rulez).
:- use_module(tree).
:- use_module(board).

possibleMove(World,Player,NewWorld,Call) :-
    rulez:isEnemy(Player, Enemy),
    removeEnemyStones(World,Enemy, PlayerStones),
    member(Stone,PlayerStones),
    checkStonePossibility(World,Stone,NewWorld,Call).

movesOfStone(World,Stone, NewWorld, [Call]) :-
    rulez:moveDirections(Stone,Direction),
    Stone = stone(Field,_,_),
    board:hasRelation(Field,Destination,Direction),
    rulez:isMoveValid(World,Stone,Direction,Destination),
    simulateMove(World,Stone,Destination,NewWorld,Call).

checkStonePossibility(World,Stone,NewWorld, Calls):-
    (
            rulez:canHit(World,Stone,HitTree),
            not(tree:isLeaf(HitTree)),
            search:longesPath(HitTree,_,[Stone | Victims]),
            simulateHits(World,Stone,Victims,NewWorld,Calls)
        ;
            movesOfStone(World,Stone,NewWorld,Calls)
    ).

simulateHits(World,Hitter,Victims, NewWorld,Calls) :-
    simulateHits(World,Hitter,Victims, [],NewWorld,Calls).

simulateHits(World,_,[], Calls,World,Calls).
simulateHits(World,Hitter, [Victim|Victims], CurCalls,NewWorld, [Call | TmpCalls]) :-
    Hitter = stone(HField,Color,Mode),
    Victim = stone(VField,_,_),
    board:isFieldBetween(HField,Destination,VField),
    simulateMove(World,Hitter,Destination,TmpWorld, Call),
    simulateHits(TmpWorld,stone(Destination,Color,Mode),Victims,CurCalls,NewWorld,TmpCalls).

removeEnemyStones([], _, []).
removeEnemyStones([Stone| World], EnemyColor, NewList):-
    removeEnemyStones(World,EnemyColor,TmpList),
    Stone = stone(_,StoneColor,_),
    (
       StoneColor == EnemyColor ->
          NewList = TmpList
       ;
       NewList = [Stone | TmpList]
    ).

simulateMove(World, Stone,Destination,NewWorld, Call) :-
    subtract(World,[Stone],TmpWorld),
    Stone = stone(Source,Color,Mode),
    (
       rulez:canTransformIntoKing(Stone) ->
          Tmp2World = [stone(Destination,Color,king)| TmpWorld]
       ;
          Tmp2World = [stone(Destination,Color,Mode)| TmpWorld]
    ),
    (
       not(board:hasRelation(Source,Destination,_))->
           board:isFieldBetween(Source,Destination,Between),
           board:stoneAt(Tmp2World,Between,RemoveStone),
           subtract(Tmp2World,[RemoveStone],NewWorld)
       ;
          NewWorld = Tmp2World
    ),
    Call =.. [performMove,Source,Destination].
