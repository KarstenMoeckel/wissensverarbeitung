% author: Karsten Möckel
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
                %% top left
                (X2 is (X1 - 1), Y2 is (Y1 - 1), !);
                %% top right
                (X2 is (X1 + 1), Y2 is (Y1 - 1), !)
            )
            ;
            (Position == top ; Type == 'queen'),
            (
                %% bottom left
                (X2 is (X1 - 1), Y2 is (Y1 + 1), !);
                %% bottom right
                (X2 is (X1 + 1), Y2 is (Y1 + 1), !)
            )
        )
    )
.

%% Player   black, white
moveStone(X1, Y1, X2, Y2, Player) :-
    validMove(X1, Y1, X2, Y2, Player).
    %% do the move
