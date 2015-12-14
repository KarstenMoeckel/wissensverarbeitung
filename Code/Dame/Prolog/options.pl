% author:  Christian Schütt
% Datum: 14.12.2015

setStartingPlayer(Color):-
  	assert(option(startingPlayer,Color)).

setSearchDepth(Number):-
	assert(option(searchDepth,Number)).

setStartingColor(Color):-
	assert(option(startingColor,Color)).

choosePlayerColor(Color):-
	assert(player(Color)).

setDifficultyLevel(Level):-
	Level = 'leicht' -> setSearchDepth(2);
	Level = 'mittel' ->	setSearchDepth(3);
	Level = 'schwer' -> setSearchDepth(5);
	Level = 'extrem' -> setSearchDepth(7).

resetGame:-
    retractall(stone(_,_,_)),
    retractall(game(_)),
    retractall(turn(_)).



startGame :-
   option(startingColor, Color),
   (
   Color = white ->
      (
         assert(turn(white)),
         assert(game(on))
      )
   ;
   Color = black ->
      (
         assert(turn(black)),
         assert(game(on))
      )
   ),!.


endGame:-
     retract(game(on)),
     assert(game(over)).


	
	

