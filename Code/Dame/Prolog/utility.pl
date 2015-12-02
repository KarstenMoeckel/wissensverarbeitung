magnitudeOf2Numbers(X,Y,L):-
(
	X >= Y ,
	L is X - Y,
	!
)
;		
(
	X < Y,
	L is Y-X
).

manhattenDistance(X1, Y1, X2, Y2, Distance):-
	magnitudeOf2Numbers(X1,X2,L1),
	magnitudeOf2Numbers(Y1,Y2,L2),
	Distance is  L1 + L2,
	!.


