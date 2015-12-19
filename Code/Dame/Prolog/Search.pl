% Autor: Robert Maas
% Datum: 19.12.2015

:- module(search,[
      ]).
      
membersOfLevel(Tree,Level,Members) :-
      Level == 1 ->
         Tree = t(Data,_),
         Members = [Data]
   ;
      membersOfLevel(Tree,Level,1,Members).

membersOfLevel(Tree,WantedLevel,CurLevel,Members) :-
   Tree = t(_,Childs),
   (
         WantedLevel =:= CurLevel + 1 ->
            childData(Childs,Members)
      ;
         NextLevel is CurLevel + 1,
         traverseChilds(Childs,WantedLevel,NextLevel,Members)
   ).

traverseChilds([],_,_,[]).
traverseChilds([Child| Childs], WantedLevel, NextLevel,Data) :-
   membersOfLevel(Child, WantedLevel,NextLevel,FoundData),
   traverseChilds(Childs,WantedLevel,NextLevel,FoundData2),
   append(FoundData,FoundData2,Data).

childData([],[]).
childData([t(Data,_)|Childs],DataList) :-
   childData(Childs,CurData),
   DataList = [Data|CurData].