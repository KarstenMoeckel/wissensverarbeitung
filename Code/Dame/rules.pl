% author: Karsten Möckel   und Christian Schütt
% Datum: 27.11.2015

validMove(X1, Y1, X2, Y2, Player) :-
    numbers(X1),
    numbers(Y1),
    numbers(X2),
    numbers(Y2),
    stone(Y1, X1, Color, Type),
    field(Y2, X2, FieldColor),
    player(Position, Player),
    FieldColor == 'black',
    Player == Color,
    (
        (
            (Position == bottom ; Type == 'queen'),
            (
                %% move top left
                (X2 is (X1 - 1), Y2 is (Y1 - 1), !);
                %% move top right
                (X2 is (X1 + 1), Y2 is (Y1 - 1), !);
                %% jump top left
                (X2 is (X1 - 2), Y2 is (Y1 - 2), !);
                % jump top left
                (X2 is (X1 + 2), Y2 is (Y1 - 2), !)
            )
            ;
            (Position == top ; Type == 'queen'),
            (
                %% move bottom left
                (X2 is (X1 - 1), Y2 is (Y1 + 1), !);
                %% move bottom right
                (X2 is (X1 + 1), Y2 is (Y1 + 1), !);
                %% jump bottom left
                (X2 is (X1 - 2), Y2 is (Y1 + 2), !);
                %% jump bottom right
                (X2 is (X1 + 2), Y2 is (Y1 + 2), !)
            )
        )
    )
.

%% Player   black, white
moveStone(X1, Y1, X2, Y2, Player):-
% author: Christian Schütt
% Datum: 29.11.2015
    
%is game is runnin, the right player does the turn and the targetfield is free
    game(on),
    rightTurn(Player),
    

    %is this a normal move or jump?
    manhattenDistance(Y1,X1,Y2,X2,Dist),

    isMovePossible(X1,Y1,X2,Y2,Player, Dist),

%Karsten Möckel
% Datum: 27.11.2015

   % validMove(X1, Y1, X2, Y2, Player),
% author: Christian Schütt
% Datum: 29.11.2015

    %% update stone-position, turn into Queen if possible und update GUI
    stone(Y1 , X1 , Color, StoneMode),
    retract(stone(Y1 , X1 , _ , _)),
    assert(stone(Y2 , X2 , Color, StoneMode)),
   
    turnIntoQueenIfPossible(X2,Y2),
    changeTurn,
    updateStonePos,
    checkVictory.



isMovePossible(X1,Y1,X2,Y2,Color, Distance):-
(
    (
        % no jump, just move
        Distance  = 2,
        validMove(X1, Y1, X2, Y2, Color),
        targetFieldFree(X2,Y2)
    )
    ;
    (   % jump
        Distance > 2,
        validMove(X1, Y1, X2, Y2 , Color),
        targetFieldFree(X2,Y2),
        findAndDetroyOverjumpedStone(Y1, X1, Y2, X2,Color)
    )
).

findAndDetroyOverjumpedStone(X1,Y1, X2, Y2, Color):-
    (
        (
            X1 > X2,
            TargetX is X1 - 1
        )
        ;
        (
            X1 < X2,
            TargetX is X1 +1
        )
        ;
        (
             Y1 > Y2,
            TargetY is X1 - 1
        )
        ;
        (
            Y1 < Y2,
            TargetY is X1 + 1
        )
    ),
    stone(TargetX,TargetY, TargetColor,_),
    Color \= TargetColor,
    retract(stone(TargetX , TargetY , _, _)).

% author: Christian Schütt
% Datum: 29.11.2015

targetFieldFree(X,Y):-
    not(stone(Y,X, _, _)).



%change the turn of the player

changeTurn:-
(
    turn(white),
    retract(turn(white)),
    assert(turn(black)),
    !
)
 ;
(
    turn(black),
    retract(turn(black)),
    assert(turn(white)),
    !
).

%check if the player has the turn

rightTurn(Color):-
    turn(X),
    (
        (
            Color \= X,
            writeln('Der falsche Spiele hat versucht einen Zug zu taetigen'),
            fail
        )
        ;
        (
            Color = X
        )
    ).

%turn a stone into a queen if possible
turnIntoQueenIfPossible(X,Y):-
 stone(Y,X,Color,Mode),
 
 (  
    (
        Mode \= queen,
        Color = white,
        Y = 1,
        retract(stone(Y , X , _, _)),
        assert(stone(Y , X , Color, queen))
    )
    ;
    (
        Mode \= queen,
        Color  = black,
        Y = 8,
        retract(stone(Y , X , _, _)),
        assert(stone(Y , X , Color, queen))
    )
    ;
    true
 )

.

checkVictory:-

        %find all and count left stones for the white and black player
        findall(Z, stone(X,Y,white,Z), WhiteStonesLeft),
        length(WhiteStonesLeft,AmountOfWhites),

        findall(Z, stone(X,Y,black,Z), BlackStonesLeft),
        length(BlackStonesLeft,AmountOfBlacks),

        (   
            (
                AmountOfWhites = 0,
                writeln('Schwarz gewinnt!'),
                retract(game(on)),
                assert(game(over)),
                assert(winner(black)),
                !     
            )
            ;
        
            (
                AmountOfBlacks = 0,
                writeln('Weiss gewinnt!'),
                retract(game(on)),
                assert(game(over)),
                assert(winner(white))    
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
        (
            StartingPlayer = weiss,
            assert(turn(white)),
            assert(game(on)),
            !
        )
        ;
        (
             StartingPlayer = schwarz,
             assert(turn(black)),
             assert(game(on)),
             !
        )
    )
    ;
    writeln('Bitte waehlen Sie weiss oder schwarz als beginnende Farbe').