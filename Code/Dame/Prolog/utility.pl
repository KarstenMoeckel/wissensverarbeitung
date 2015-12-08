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
%08.12.2015
createStoneList(List) :-
   findall(stone(Row,Col,Color,Type),stone(Row,Col,Color,Type),List).

hasRelation(field(Row1,Col1),field(Row2,Col2),Relation) :- hasRelation(Row1,Col1,Row2,Col2,Relation).
%checks Relation of Fields; manhatten-distance must be 2
%Relation: <(Row2,Col2)> is <Relation> of <(Row1,Col1)>
hasRelation(Row1,Col1,Row2,Col2,Relation) :-
   number(Row1),
   number(Row2),
   number(Col1),
   number(Col2),
   (
      (
         Row2 =:= Row1 + 1,
         Col2 =:= Col1 + 1
      ) ->
         Relation = bottomRight
      ;
      (
         Row2 =:= Row1 + 1,
         Col2 =:= Col1 - 1
      ) ->
         Relation = bottomLeft
      ;
      (
         Row2 =:= Row1 - 1,
         Col2 =:= Col1 + 1
      ) ->
         Relation = topRight
      ;
      (
         Row2 =:= Row1 - 1,
         Col2 =:= Col1 - 1
      ) ->
         Relation = topLeft
   ).
hasRelation(SRow,SCol,DRow,DCol,Relation) :-
   var(DRow),
   var(DCol),
   number(SRow),
   number(SCol),
   atom(Relation),
   (
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
         DCol is SCol - 1
   ).

moveDirections(stone(_,_,_,queen), Direction):-
      Direction = bottomLeft
   ;
      Direction = bottomRight
   ;
      Direction = topLeft
   ;
      Direction = topRight.

moveDirections(stone(_,_,Color,normal), Direction) :-
   player(Position, Color),
   (
      Position == top ->
      (
         Direction = bottomLeft
         ;
         Direction = bottomRight
      )
      ;
      Position == bottom ->
      (
         Direction = topLeft
         ;
         Direction = topRight
      )
   ).