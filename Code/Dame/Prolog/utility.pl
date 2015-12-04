magnitudeOf2Numbers(X,Y,L):-
   X >= Y ->
      L is X - Y
   ;
      L is Y-X.

manhattenDistance(X1, Y1, X2, Y2, Distance):-
        magnitudeOf2Numbers(X1,X2,L1),
        magnitudeOf2Numbers(Y1,Y2,L2),
        Distance is  L1 + L2,
        !.


%Robert Maas
%03.12.2015
createStoneList(List) :-
   findall(stone(Row,Col,Color,Type),stone(Row,Col,Color,Type),List).

%checks Relation of Fields; manhatten-distance must be 2
%e.g. checkRelation(1,1,2,2,buttonRight)
checkRelation(Row1,Col1,Row2,Col2,Relation) :-
      (
         Row2 is Row1 + 1,
         Col2 is Col1 + 1
      ) ->
         Relation = bottomRight
   ;
      (
         Row2 is Row1 + 1,
         Col2 is Col1 - 1
      ) ->
         Relation = bottomLeft
   ;
      (
         Row2 is Row1 - 1,
         Col2 is Col1 + 1
      ) ->
         Relation = topRight
   ;
      (
         Row2 is Row1 - 1,
         Col2 is Col1 - 1
      ) ->
         Relation = topLeft.
      
calculateTarget(SRow,SCol,Relation,DRow,DCol) :-
   Relation == bottomRight ->
      DRow is SRow + 1,
      DCol is SCol + 1
   ;
   Relation == bottomLeft ->
      DRow is SRow + 1,
      DCol is SCol - 1
   ;
   Relation == topRight ->
      DRow is SRow - 1,
      DCol is SCol + 1
   ;
   Relation == topLeft ->
      DRow is SRow - 1,
      DCol is SCol - 1.