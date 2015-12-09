magnitudeOf2Numbers(N1,N2,L):-
   N1 >= N2 ->
      L is N1 - N2
   ;
      L is N2-N1.

manhattenDistance(field(Row1,Col1), field(Row2,Col2), Distance):-
        magnitudeOf2Numbers(Row1,Row2,L1),
        magnitudeOf2Numbers(Col1,Col2,L2),
        Distance is  L1 + L2,
        !.

%Robert Maas
%08.12.2015
%call: --List
createStoneList(List) :-
   findall(stone(Field,Color,Type),stone(Field,Color,Type),List).

%call: +Field1, +Field2, -Relation
%  OR: +Field1, --Field2, +Relation
%checks Relation of Fields; manhatten-distance must be 2
%Relation: <Field2> is <Relation> of <Field1>
hasRelation(field(Row1,Col1),field(Row2,Col2),Relation) :-
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
hasRelation(field(SRow,SCol),Destination,Relation) :-
   var(Destination),
   atom(Relation),
   (
      Relation == bottomRight ->
         DRow is SRow + 1,
         DCol is SCol + 1,
         Destination = field(DRow,DCol)
      ;
      Relation == bottomLeft ->
         DRow is SRow + 1,
         DCol is SCol - 1,
         Destination = field(DRow,DCol)
      ;
      Relation == topRight ->
         DRow is SRow - 1,
         DCol is SCol + 1,
         Destination = field(DRow,DCol)
      ;
      Relation == topLeft ->
         DRow is SRow - 1,
         DCol is SCol - 1,
         Destination = field(DRow,DCol)
   ).

%call: +Stone, -Direction
moveDirections(stone(_,_,queen), Direction):-
      Direction = bottomLeft
   ;
      Direction = bottomRight
   ;
      Direction = topLeft
   ;
      Direction = topRight.

moveDirections(stone(_,Color,normal), Direction) :-
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