/** <board>

Module to fuck cunt shit

*/

:- module(board, [
    field/2, %call: +Field, -Color
    hasRelation/3, %call: +Field1, +Field2, -Relation
                   %OR call: +SourceField, --DestinationField, -Relation
    isFree/2, %call: +World, +Field
    hasNeighbour/3, %call: +World, +Stone, -Neighbours
    isFieldBetween/3, %call: +Field1, +Field2, -FieldBetween
                     %OR call: +Field1, -Field2, +FieldBetween
    isBorderCol/1, %call: +Field
    isBorderRow/1, %call: +Field
    stoneAt/3 %call: +World, +Field, -Stone
    ]).

/**
 * Predicate to determine the color of a field.
 * @param Field
 * @return Color
 */
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

/**
 * Predicate checks if a stone of the world lies on the given field.
 * @param World List of all stones.
 * @param Field The field.
 * @return The stone that lies on the field.
 */
stoneAt([],_,_) :- fail.
stoneAt([CheckStone|World],Field,Stone) :-
   CheckStone = stone(Field,_,_) ->
      Stone = CheckStone
   ;
      stoneAt(World,Field,Stone).

/**
 * Predicate checks if a number is between 1 and 8.
 * Used to create the board.
 * @param X A number.
 */
numbers(X) :- between(1,8,X).

/**
 * Predicate to check if a given field has no stone.
 * @param World List of all stones.
 * @param Field A Field.
 * @return true if field is free, else false.
 */
isFree([],_).
isFree([stone(Field2,_,_)| World], Field) :-
   (Field2 == Field) ->
      fail
   ;
      isFree(World,Field).

/**
 * Predicate checks the relation of two fields.
 * @param Field1
 * @param Field2
 * @return Relation The relation of the fields.
 */
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

/**
 * Predicate checks the Relation of a field and gives the depending field.
 * @param Field
 * @return Destination The field.
 * @param Relation The Relation of the two fields.
 */
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

/**
 * Predicate to find a Neighbour for a Stone.
 * @param World List of all stones.
 * @param Stone The stone that may has Neighbours.
 * @return Neighbours List with all neighbours.
 */
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

/**
 * Predicate finds the field between two given fields.
 * @param Field1
 * @param Field2
 * @result FieldBetween
 */
isFieldBetween(Field1,Field2,FieldBetween) :-
   hasRelation(Field1,FieldBetween,Relation),
   hasRelation(FieldBetween,Field2,Relation).

isBorderCol(field(_,Col)) :- Col == 1; Col == 8.
isBorderRow(field(Row,_)) :- Row == 1; Row == 8.
