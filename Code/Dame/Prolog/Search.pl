/** <search>

Modul to search in the searchtree.

@author: Robert Maas
@date: 20.12.2015
*/
:- module(search,[
      membersOfLevel/3, %call: +Tree, +Level, -Members
      nodesOfLevel/3,
      longesPath/3, %call: +Tree, --PathLength, --Path,
      childData/2
      ]).

:- use_module(tree).

/**
 * Find all Treenodes of a given level.
 * @param Tree The Tree.
 * @param WantedLevel The level to search for the Treenodes.
 * @return Nodes The list with the found nodes.
 */
nodesOfLevel(Tree,WantedLevel,Nodes) :-
    WantedLevel == 1 ->
        Nodes = [Tree]
    ;
    nodesOfLevel(Tree,WantedLevel,1,Nodes).

nodesOfLevel(Tree,WantedLevel,CurLevel,Nodes) :-
    Tree = t(_,Childs),
    NextLevel is CurLevel +1,
    (
          WantedLevel == NextLevel ->
             Nodes = Childs
       ;
          nodesOfChilds(Childs,WantedLevel,NextLevel,Nodes)
    ).

/**
 * Predicate to find all nodes of a list Childs.
 * @param Childs List of Childs.
 * @param WantedLevel The wanted level.
 * @param NextLevel The next level.
 * @return Nodes The found nodes of the wanted level.
 */
nodesOfChilds([],_,_,[]).
nodesOfChilds([Child| Childs], WantedLevel, NextLevel,Nodes) :-
    nodesOfLevel(Child, WantedLevel,NextLevel,FoundData),
    nodesOfChilds(Childs,WantedLevel,NextLevel,FoundData2),
    append(FoundData,FoundData2,Nodes).

/**
 * Predicate to find all Nodes of a given level.
 * @param Tree The Tree to search.
 * @param Level The wanted level.
 * @return Members The found nodes.
 */
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

/**
 * Predicate to find all nodes of a list Childs.
 * @param Childs List of Childs.
 * @param WantedLevel The wanted level.
 * @param NextLevel The next level.
 * @return Nodes The found nodes of the wanted level.
 */
membersOfChilds([],_,_,[]).
membersOfChilds([Child| Childs], WantedLevel, NextLevel,Data) :-
   membersOfLevel(Child, WantedLevel,NextLevel,FoundData),
   membersOfChilds(Childs,WantedLevel,NextLevel,FoundData2),
   append(FoundData,FoundData2,Data).

/**
 * Predicate to collect all the data from the childs.
 * @param Tree
 * @return DataList
 */
childData([],[]).
childData([t(Data,_)|Childs],DataList) :-
   childData(Childs,CurData),
   DataList = [Data|CurData].

/**
 * Predicate to find the longest path in a tree.
 * @param Tree The tree to search the longest path.
 * @return PathLength The length of the longest path.
 * @return Path The longest path.
 */
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
