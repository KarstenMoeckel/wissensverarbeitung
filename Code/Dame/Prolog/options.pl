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
	
	

