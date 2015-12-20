% Autor:
% Datum: 17.12.2015

:- module(board, [
    field/2, %call: +Field, -Color
    hasRelation/3, %call: +Field1, +Field2, -Relation
                   %OR call: +SourceField, --DestinationField, -Relation
    isFree/2, %call: +World, +Field
    hasNeighbour/3, %call: +World, +Stone, -Neighbours
    isFieldBetween/3, %call: +Field1, +Field2, -FieldBetween
                     %OR call: +Field1, -Field2, +FieldBetween
    isBorderCol/1 %call: +Field
    ]).
    
field(field(Row,Col),Color) :-
   numbers(Row),
   numbers(Col),
   Sum is Row + Col,
   R is Sum mod 2,
   (
      R==1 ->
         Color='black'
      ;
         Color='white'
   ).
   
numbers(X) :- between(1,8,X).

isFree([],_).
isFree([stone(Field2,_,_)| World], Field) :-
   (Field2 == Field) ->
      fail
   ;
      isFree(World,Field).

hasRelation(field(Row1, Col1),Field2, Relation) :-
   nonvar(Field2),
   Field2 = field(Row2,Col2),
   (
         Row2 =:= Row1 + 1,
         Col2 =:= Col1 + 1 ->
            Relation = bottomRight
      ;
         Row2 =:= Row1 + 1,
         Col2 =:= Col1 - 1 ->
            Relation = bottomLeft
      ;
         Row2 =:= Row1 - 1,
         Col2 =:= Col1 + 1 ->
            Relation = topRight
      ;
         Row2 =:= Row1 - 1,
         Col2 =:= Col1 - 1 ->
            Relation = topLeft
   ).
hasRelation(field(SRow,SCol),Destination,Relation) :-
   var(Destination),
   %atom(Relation),
   (
         Relation = bottomRight,
         DRow is SRow + 1,
         DCol is SCol + 1,
         Destination = field(DRow,DCol)
      ;
         Relation = bottomLeft,
         DRow is SRow + 1,
         DCol is SCol - 1,
         Destination = field(DRow,DCol)
      ;
         Relation = topRight,
         DRow is SRow - 1,
         DCol is SCol + 1,
         Destination = field(DRow,DCol)
      ;
         Relation = topLeft ->
         DRow is SRow - 1,
         DCol is SCol - 1,
         Destination = field(DRow,DCol)
   ).
   
hasNeighbour([],_,[]).
hasNeighbour([TestStone|World],Stone,Neighbours) :-
   hasNeighbour(World,Stone,FoundNeighbours),
   (
         Stone = stone(Field,_,_),
         TestStone = stone(TestField,_,_),
         hasRelation(Field,TestField,Relation) ->
            Neighbours = [[TestStone, Relation]|FoundNeighbours]
      ;
         Neighbours = FoundNeighbours
   ).
   
isFieldBetween(Field1,Field2,FieldBetween) :-
   hasRelation(Field1,FieldBetween,Relation),
   hasRelation(FieldBetween,Field2,Relation).
   
isBorderCol(field(_,Col)) :- Col == 1; Col == 8.
isBorderRow(field(Row,_)) :- Row == 1; Row == 8.