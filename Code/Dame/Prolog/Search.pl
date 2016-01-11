% Autor: Robert Maas
% Datum: 20.12.2015

:- module(search,[
      membersOfLevel/3, %call: +Tree, +Level, -Members
      nodesOfLevel/3,
      longesPath/3, %call: +Tree, --PathLength, --Path,
      childData/2
      ]).

:- use_module(tree).

nodesOfLevel(Tree,WantedLevel,Nodes) :-
    WantedLevel == 1 ->
        Nodes = [Tree]
    ;
    nodesOfLevel(Tree,WantedLevel,0,Nodes).

nodesOfLevel(Tree,WantedLevel,CurLevel,Nodes) :-
    Tree = t(_,Childs),
    NextLevel is CurLevel +1,
    (
          WantedLevel == NextLevel ->
             Nodes = Childs
       ;
          nodesOfChilds(Childs,WantedLevel,NextLevel,Nodes)
    ).

nodesOfChilds([],_,_,[]).
nodesOfChilds([Child| Childs], WantedLevel, NextLevel,Nodes) :-
    nodesOfLevel(Child, WantedLevel,NextLevel,FoundData),
    nodesOfChilds(Childs,WantedLevel,NextLevel,FoundData2),
    append(FoundData,FoundData2,Nodes).

membersOfLevel(Tree,Level,Members) :-
    Level == 1->
        nodeData(Tree, Data),
        Members = [Data]
    ;
    membersOfLevel(Tree,Level,1,Members).

membersOfLevel(Tree,WantedLevel,CurLevel,Members) :-
   Tree = t(_,Childs),
   NextLevel is CurLevel + 1,
   (
         WantedLevel == NextLevel ->
            childData(Childs,Members)
      ;
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
