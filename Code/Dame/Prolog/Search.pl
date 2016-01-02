% Autor: Robert Maas
% Datum: 20.12.2015

:- module(search,[
      membersOfLevel/3, %call: +Tree, +Level, -Members
      longesPath/3 %call: +Tree, --PathLength, --Path
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
         membersOfChilds(Childs,WantedLevel,NextLevel,Members)
   ).

membersOfChilds([],_,_,[]).
membersOfChilds([Child| Childs], WantedLevel, NextLevel,Data) :-
   membersOfLevel(Child, WantedLevel,NextLevel,FoundData),
   membersOfChilds(Childs,WantedLevel,NextLevel,FoundData2),
   append(FoundData,FoundData2,Data).

childData([],[]).
childData([t(Data,_)|Childs],DataList) :-
   childData(Childs,CurData),
   DataList = [Data|CurData].

longesPath(Tree,PathLength,Path) :-
   nonvar(Tree),
   Tree = t(Data,Childs),
   longesChildPath(Childs, ChildLength,ChildPath),
   PathLength is ChildLength + 1,
   Path = [Data|ChildPath].

longesChildPath([],0,[]).
longesChildPath([Child|Childs],LongesPathLength,LongesPath) :-
   longesChildPath(Childs,PathLength,Path),
   longesPath(Child,ChildLength,ChildPath),
   (
         PathLength > ChildLength ->
            LongesPathLength = PathLength,
            LongesPath = Path
      ;
         LongesPathLength = ChildLength,
         LongesPath = ChildPath
   ).