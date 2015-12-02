% Autor: Robert Maas
% Datum: 02.12.2015

:- use_module(library(pce)).
:- dynamic gameField/1.
:- dynamic stone/3. % stone(Color, Type, GUI-Element-Reference).
:- dynamic historyBrowser/1.

fieldLength(30).
blackFieldColor(colour(gray)).
history(['abc123', 'def456', 'ghi789']).
fieldOffset(25,5). % fieldOffset(Top,Left).

stoneImage(black,normal,'black_normal.xpm').
stoneImage(black,queen,'black_queen.xpm').
stoneImage(white,normal,'white_normal.xpm').
stoneImage(white,queen,'white_queen.xpm').

updateHistory :-
   historyBrowser(B),
   history(List),
   !,
   send(B, members(List)),
   send(B, flush).

updateStonePos :-
   freeStones,
   fieldLength(Length),
   fieldOffset(OffsetTop, OffsetLeft),
   gameField(Picture),
   !,
   (
      (
         stone(Row,Col,Color,Type),
         Top is Length * (Row - 1) + OffsetTop,
         Left is Length * (Col - 1) + OffsetLeft,
         stoneImage(Color, Type,File),
         new(I, image(File)),
         new(Image, bitmap(I)),
         send(Picture, display, Image, point(Left,Top)),
         assertz(stone(Color,Type,Image)),
         fail
      )
      ;
         true
   ),
   send(Picture, flush).

freeStones :-
   (
      stone(Color, Type, Image),
      free(Image),
      retract(stone(Color,Type,Image)),
      fail
   )
   ;
      true.
   
drawGame(Picture) :-
   new(Text, text('   1      2      3      4      5     6      7     8')),
   send(Picture, display, Text, point(5,5)),
   (
      numbers(X),
      drawRow(Picture, X),
      fail
   )
   ;
      true.

drawRow(Picture, Row) :-
   fieldLength(Length),
   fieldOffset(OffsetTop, OffsetLeft),
   Top is Length * (Row - 1) + OffsetTop,
   !,
   (
      (
         numbers(X),
         createField(Row,X, Field),
         Left is Length * (X - 1) + OffsetLeft,
         send(Picture, display, Field, point(Left, Top)),
         fail
      )
      ;
      (
         Left is Length * 8 + OffsetLeft,
         atom_concat('  ', Row, Txt),
         new(Text, text(Txt)),
         send(Picture, display, Text, point(Left, Top + 7))
      )
   ).

createField(Row,Col,Field) :-
   fieldLength(Length),
   !,
   new(Field, box(Length,Length)),
   field(Row,Col, FieldColor),
   (
      (
         FieldColor == black,
         blackFieldColor(Color),
         send(Field, fill_pattern, Color),
         !
      )
      ;
         true
   ).
   
startGUI :-
   retractall(gameField(_)),
   retractall(historyBrowser(_)),
   new(Window, dialog('EA Dame')),
   new(GameField, picture),
   assertz(gameField(GameField)),
   send(Window, append, GameField),
   send(GameField, size, size(280,280)),
   drawGame(GameField),
   new(Browser, list_browser),
   send(Browser, label, 'Verlauf:'),
   send(Window, append, Browser, right),
   send(Browser, bottom_side, GameField?bottom_side),
   assertz(historyBrowser(Browser)),
   updateHistory,
   send(Window, open).