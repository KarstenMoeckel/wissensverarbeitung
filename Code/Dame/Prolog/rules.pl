% author: Karsten Möckel   und Christian Schütt
% Datum: 03.12.2015

moveTopLeft(Source,Destination) :- hasRelation(Source,Destination,topLeft).
moveTopRight(Source,Destination) :- hasRelation(Source,Destination,topRight).
moveBottomLeft(Source,Destination) :- hasRelation(Source,Destination,bottomLeft).
moveBottomRight(Source,Destination) :- hasRelation(Source,Destination,bottomRight).

jumpTopLeft(Source, Destination) :-
   hasRelation(Source,Tmp,topLeft),
   hasRelation(Tmp,Destination,topLeft).
jumpTopRight(Source, Destination) :-
   hasRelation(Source,Tmp,topRight),
   hasRelation(Tmp,Destination,topRight).
jumpBottomLeft(Source, Destination) :-
   hasRelation(Source,Tmp,bottomLeft),
   hasRelation(Tmp,Destination,bottomLeft).
jumpBottomRight(Source, Destination) :-
   hasRelation(Source,Tmp,bottomRight),
   hasRelation(Tmp,Destination,bottomRight).

validMove(stone(Source,_,_), Direction, Player, World) :-
   hasRelation(Source,Destination,Direction),
   validMove(Source, Destination, Player, World).

validMove(Source, Destination, Player, World):-
    field(Source,black),
    member(stone(Source, Player, Type), World),
    field(Destination, black),
    player(Position, Player),
    (
        (
            (Position == bottom ; Type == 'queen'),
            (
                moveTopLeft(Source, Destination);
                moveTopRight(Source, Destination);
                jumpTopLeft(Source, Destination);
                jumpTopRight(Source, Destination)
            )
            ;
            (Position == top ; Type == 'queen'),
            (
                moveBottomLeft(Source, Destination);
                moveBottomRight(Source, Destination);
                jumpBottomLeft(Source, Destination);
                jumpBottomRight(Source, Destination)
            )
        )
     ).

rightPlayer(Field):-
    stone(Field,Color,_),
    turn(CurrentPlayer),
    CurrentPlayer = Color.

%% Player   black, white
moveStone(Source, Destination):-
% author: Christian Schütt
% Datum: 29.11.2015
   game(on),
   rightPlayer(Source),
   (
      isMove(Source,Destination) ->
         (
            setStoneToNewPosition(Source,Destination),
            (
              (not(isKing(Destination)),stoneReachedLastRow(Destination))-> turnIntoKing(Destination)
              ;
              true
            )
         )
      ;
      isJump(Source,Destination) ->
         (
              detroyJumpedStone,
              setStoneToNewPosition(Source,Destination),
             (
                (not(isKing(Destination)),stoneReachedLastRow(Destination))-> turnIntoKing(Destination)
                ;
                true
             )
         )
    ),
    changeTurn,
    isGameOver,
    (not(stonesUpdated) -> assert(stonesUpdated); true),
    !.


isHitPossible(Field,HittableNeighbours):-
    createStoneList(World),
    hasNeighbours(stone(Field,_,_),World,Neighbours),
    (
        (
            member(Element,Neighbours),
            (
              (
                canHit(World,stone(Field,_,_),[Element],Vict) -> assert(hittableNeighbours(Element)),
                fail
              )
            )
        )
        ;
        true
    ),
    hittableNeighbours(X).


setStoneToNewPosition(Source,Destination):-
    stone(Source ,Color, StoneMode),
    retract(stone(Source, _, _)),
    assert(stone(Destination, Color, StoneMode)).

isJump(Source,Destination):-
    manhattenDistance(Source,Destination,Distance),
    Distance == 4 ->
       createStoneList(List),
       stone(Source,Color,_),
       validMove(Source, Destination, Color, List ),
       fieldFree(List,Destination),
       isStoneBetween(Source, Destination, JumpedField),
       rightColorToBeat(JumpedField),
       keepInMindToDestroyLater(JumpedField).

isMove(Source,Destination):-
    manhattenDistance(Source,Destination,Distance),

    Distance == 2 ->
       createStoneList(List),
       stone(Source,Color,_),
       validMove(Source, Destination, Color, List ),
       fieldFree(List,Destination).

keepInMindToDestroyLater(JumpedField):-
    assert(toDestroyStone(JumpedField)).

topRightNeighbor(Source, Destination):- hasRelation(Source,Destination,topRight).
bottomLeftNeighbor(Source, Destination):- hasRelation(Source,Destination,bottomLeft).
bottomRightNeighbor(Source, Destination):- hasRelation(Source,Destination,bottomRight).

rightColorToBeat(Field):-
    turn(CurrentPlayer),
    not(stone(Field,CurrentPlayer,_)).

isStoneBetween(Field1,Field2, FieldBetween):-
   Field1 = field(Row1,Col1),
   stone(field(Row1,Col1),_,_),
   (
      (
         hasRelation(Field1, FieldBetween, topLeft),
         hasRelation(FieldBetween, Field2, topLeft)

      ) ->
         true
      ;
      (
         hasRelation(Field1, FieldBetween, topRight),
         hasRelation(FieldBetween, Field2, topRight)
      ) ->
         true
      ;
      (
         hasRelation(Field1, FieldBetween, bottomLeft),
         hasRelation(FieldBetween, Field2, bottomLeft)
      ) ->
         true
      ;
      (
         hasRelation(Field1, FieldBetween, bottomRight),
         hasRelation(FieldBetween, Field2, bottomRight)
      ) ->
         true
   ),
   stone(FieldBetween, _,_).

detroyJumpedStone:-
    toDestroyStone(Field),
    retract(stone(Field,_,_)),
    retract(toDestroyStone(Field)).


% author: Robert Maas
% Datum: 03.12.2015
fieldFree([],_).
fieldFree([stone(Field2,_,_)|GameState], Field) :-
   (Field2 == Field) ->
      fail
   ;
      fieldFree(GameState,Field).

% author: Christian Schütt
% Datum: 29.11.2015

%change the turn of the player

changeTurn:-
(
    turn(white) ->
    retract(turn(white)),
    assert(turn(black))
)
 ;
(
    turn(black) ->
    retract(turn(black)),
    assert(turn(white))
).

%check if the player has the turn

rightTurn(Color):-
    not(turn(Color)) ->
       logMessage('Der falsche Spiele hat versucht einen Zug zu tätigen'),
       fail
   ;
   true.

isKing(Field):-
    stone(Field,_,queen).

stoneReachedLastRow(field(_,Col)):-
   stone(field(8,Col),black,_)
   ;
   stone(field(1,Col),white,_).

turnIntoKing(Field):-
   stone(Field,Color,_),
   retract(stone(Field, _, _)),
   assert(stone(Field, Color, queen)).

howManyStonesLeftOf(Color,Count):-
    findall(Z, stone(_,Color,Z), StonesLeft),
    length(StonesLeft,Count).

doesColorLose(Color):-
     howManyStonesLeftOf(Color,AmountOfStone),
     AmountOfStone = 0.

registerWinner(Color):-
     assert(winner(Color)).


isGameOver:-
   doesColorLose(white) ->
      (
         registerWinner(black),
         endGame
      )
   ;
   doesColorLose(black) ->
      (
         registerWinner(white),
         endGame
      )
   ;
   true.

