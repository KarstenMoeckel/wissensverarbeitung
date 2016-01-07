% Autor: Robert Maas
% Datum: 19.12.2015

:- module(tree,[
     appendTree/4, %call: ParentData, NodeData, Tree, NewTree
     isLeaf/1,
     nodeData/2
     ]).

nodeData(Node,Data):- Node = t(Data,_).

isLeaf(Tree) :-
   nonvar(Tree),
   Tree = t(_,[]).

appendTree(Parent,Data,Tree,NewTree):-
   var(Parent),
   var(Tree),
   NewTree = t(Data,[]).
appendTree(Parent,Data,Tree,NewTree) :-
   nonvar(Parent),
   nonvar(Tree),
   Tree = t(Parent,SubTrees)->
      NewTree = t(Parent,[t(Data,[])|SubTrees]).
appendTree(Parent,Data,Tree,NewTree) :-
   nonvar(Parent),
   nonvar(Tree),
   Tree = t(CurNodeData,SubTrees),
   CurNodeData \== Parent ->
      checkSubTrees(Parent,Data,SubTrees,NewSubTrees),
      NewTree = t(CurNodeData,NewSubTrees).

checkSubTrees(Parent,Data,[Tree|SubTrees],NewSubTrees) :-
      appendTree(Parent,Data,Tree,NewTree) ->
         NewSubTrees = [NewTree|SubTrees]
   ;
      checkSubTrees(Parent,Data, SubTrees,NewTree),
      NewSubTrees = [Tree|NewTree].
