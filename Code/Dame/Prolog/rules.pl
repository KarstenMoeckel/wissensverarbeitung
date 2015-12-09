% author: Karsten Möckel   und Christian Schütt
% Datum: 03.12.2015

moveTopLeft(X1, Y1, X2, Y2) :-
    X2 is (X1 - 1), Y2 is (Y1 - 1).
moveTopRight(X1, Y1, X2, Y2) :-
    X2 is (X1 + 1), Y2 is (Y1 - 1).
moveBottomLeft(X1, Y1, X2, Y2) :-
    X2 is (X1 - 1), Y2 is (Y1 + 1).
moveBottomRight(X1, Y1, X2, Y2) :-
    X2 is (X1 + 1), Y2 is (Y1 + 1).

jumpTopLeft(X1, Y1, X2, Y2) :-
    X2 is (X1 - 2), Y2 is (Y1 - 2).
jumpTopRight(X1, Y1, X2, Y2) :-
    X2 is (X1 + 2), Y2 is (Y1 - 2).
jumpBottomLeft(X1, Y1, X2, Y2) :-
    X2 is (X1 - 2), Y2 is (Y1 + 2).
jumpBottomRight(X1, Y1, X2, Y2) :-
    X2 is (X1 + 2), Y2 is (Y1 + 2).



validMove(Stone, Direction, Player, SrcList) :-
    arg(1, Stone, Y1),
    arg(2, Stone, X1),
    (
        Direction == topLeft,
            (
                moveTopLeft(X1, Y1, X2, Y2),
                validMove(X1, Y1, X2, Y2, Player, SrcList)
            )
        ;
        Direction == topRight,
            (
                moveTopRight(X1, Y1, X2, Y2),
                validMove(X1, Y1, X2, Y2, Player, SrcList)
            )
        ;
        Direction == bottomLeft,
            (
                moveBottomLeft(X1, Y1, X2, Y2),
                validMove(X1, Y1, X2, Y2, Player, SrcList)
            )
        ;
        Direction == bottomRight,
            (
                moveBottomRight(X1, Y1, X2, Y2),
                validMove(X1, Y1, X2, Y2, Player, SrcList)
            )
    )
    .

validMove(X1, Y1, X2, Y2, Player, SrcList):-
    numbers(X1),
    numbers(Y1),
    member(stone(Y1, X1, Player, Type), SrcList),
    field(Y2, X2, black),
    player(Position, Player),
    (
        (
            (Position == bottom ; Type == 'queen'),
            (
                moveTopLeft(X1, Y1, X2, Y2);
                moveTopRight(X1, Y1, X2, Y2);
                jumpTopLeft(X1, Y1, X2, Y2);
                jumpTopRight(X1, Y1, X2, Y2)
            )
            ;
            (Position == top ; Type == 'queen'),
            (
                moveBottomLeft(X1, Y1, X2, Y2);
                moveBottomRight(X1, Y1, X2, Y2);
                jumpBottomLeft(X1, Y1, X2, Y2);
                jumpBottomRight(X1, Y1, X2, Y2)
            )
        )
    )
.

rightPlayer(X,Y):-
    stone(Y,X,Color,_),
    turn(CurrentPlayer),
    CurrentPlayer = Color.

%% Player   black, white
moveStone(X1, Y1, X2, Y2):-
% author: Christian Schütt
% Datum: 29.11.2015

% author: Christian Schütt
% Datum: 29.11.2015
    game(on),
    rightPlayer(X1,Y1),

    (   
        (isMove(X1,Y1,X2,Y2) -> setStoneToNewPosition(X1,Y1,X2,Y2),turnIntoKingIfPossible(X2,Y2),
                                changeTurn)
    ;
        (isJump(X1,Y1,X2,Y2) -> detroyJumpedStone,setStoneToNewPosition(X1,Y1,X2,Y2),turnIntoKingIfPossible(X2,Y2),
                                changeTurn)

    ),  

    gameOver,
   
    updateStonePos,  
    !.



setStoneToNewPosition(X1,Y1,X2,Y2):-
    stone(Y1 , X1 , Color, StoneMode),
    retract(stone(Y1 , X1 , _ , _)),
    assert(stone(Y2 , X2 , Color, StoneMode)).

isJump(X1,Y1,X2,Y2):-
    manhattenDistance(Y1,X1,Y2,X2,Distance),
        
    (Distance = 4) -> createStoneList(List),
                      stone(Y1,X1,Color,_),
                      validMove(X1, Y1, X2, Y2, Color, List ),
                      isFieldFree(X2,Y2), 
                      isStoneBetween(Y1,X1,Y2,X2,StoneX,StoneY),
                      rightColorToBeat(StoneX,StoneY),
                      keepInMindToDestroyLater(StoneX,StoneY).



isMove(X1,Y1,X2,Y2):-
    manhattenDistance(Y1,X1,Y2,X2,Distance),

    (Distance  = 2) ->createStoneList(List),
                      stone(Y1,X1,Color,_), 
                      validMove(X1, Y1, X2, Y2, Color, List ),
                      isFieldFree(X2,Y2).

keepInMindToDestroyLater(X,Y):-
    assert(toDestroyStone(X,Y)).


topLeftNeighbor(X, Y, NX, NY):-
        NX is X - 1,
        NY is Y -1.    

topRightNeighbor(X, Y, NX, NY):-
        NX is X + 1,
        NY is Y - 1.

bottomLeftNeighbor(X, Y, NX, NY):-
        NX is X - 1,
        NY is Y + 1.

bottomRightNeighbor(X, Y, NX, NY):-
        NX is X  + 1,
        NY is Y + 1.

rightColorToBeat(X,Y):-
    turn(CurrentPlayer),
    stone(X,Y,Color,_),
    Color \= CurrentPlayer.
        

isOver(X1,X2):-
    X1 > X2.

isUnder(X1,X2):-
    X1 < X2.

isRightOf(Y1,Y2):-
    Y1 < Y2.

isLeftOf(Y1,Y2):-
    Y1 > Y2.
    

isStoneBetween(X1,Y1,X2,Y2, NX, NY):-
      stone(X1,Y1,_,_),
      (
        (
            (isOver(X1, X2), isLeftOf(Y1,Y2)) -> topLeftNeighbor(X1,Y1, NX, NY)
        )
        ;
        (
            (isOver(X1, X2), isRightOf(Y1,Y2)) -> topRightNeighbor(X1,Y1, NX, NY)
        )
        ;
        (
            (isUnder(X1, X2), isLeftOf(Y1,Y2)) -> bottomLeftNeighbor(X1,Y1, NX, NY)
        )
        ;
        (
            (isUnder(X1, X2), isRightOf(Y1,Y2)) -> bottomRightNeighbor(X1,Y1, NX, NY)
        )
     ),
     stone( NX,NY, _,_).


detroyJumpedStone:-
    toDestroyStone(X,Y),
    retract(stone(X,Y,_,_)),
    retract(toDestroyStone(X,Y)).


% author: Robert Maas
% Datum: 03.12.2015
fieldFree([],_,_).
fieldFree([stone(R,C,_,_)|GameState], Row,Col) :-
   (R == Row,
   C == Col) ->
      fail
   ;
      fieldFree(GameState,Row,Col).

% author: Christian Schütt
% Datum: 29.11.2015

isFieldFree(X,Y):-
    not(stone(Y,X, _, _)).


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

turnIntoKingIfPossible(X,Y):-
    (
        notAKing(X,Y),
        stoneReachedLastRow(X,Y),
        turnIntoKing(X,Y)
    )
    ;
    true.

notAKing(Column,Row):-
    stone(Row,Column,_,Mode),
    Mode \= queen.

stoneReachedLastRow(Column,Row):-
    stone(Row,Column,Color,_),
    (
        (
            Color  = black,
            Row = 8
        )
        ;
        (   
            Color = white,
            Row = 1
        )
    ).


turnIntoKing(Column,Row):-
   stone(Row,Column,Color,_),
   retract(stone(Row , Column , _, _)),
   assert(stone(Row , Column , Color, queen)).


howManyStonesLeftOf(Color,Count):-
    findall(Z, stone(_,_,Color,Z), StonesLeft),
    length(StonesLeft,Count).

doesColorLose(Color):-
     howManyStonesLeftOf(Color,AmountOfStone),
     AmountOfStone = 0.

registerWinner(Color):-
     assert(winner(Color)).

endGame:-
     retract(game(on)),
     assert(game(over)).

gameOver:-
        (   
            (
                doesColorlose(white) -> registerWinner(black), endGame,
                !     
            )
            ;
        
            (
                doesColorlose(black) -> registerWinner(white),endGame,
                !        
            )
            ;
            true
        ). 

stopGame:-
    retractall(stone(_,_,_,_)),
    retractall(game(_)),
    retractall(turn(_)).

startGame(StartingPlayer):-
     (
        StartingPlayer = white ->
           assert(turn(white)),
           assert(game(on))
     )
     ;
     (
        StartingPlayer = black ->
           assert(turn(black)),
           assert(game(on))
     ).
