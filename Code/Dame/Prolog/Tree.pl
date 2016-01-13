% Autor: Robert Maas
% Datum: 19.12.2015

:- module(tree,[
     appendTree/4, %call: ParentData, NodeData, Tree, NewTree
     isLeaf/1,
     nodeData/2,
     subTree/3,
     nodeChildren/2,
     appendChildNodesToRootNode/3
     ]).

nodeData(Node,Data):- Node = t(Data,_).
nodeChildren(Node, Children):- Node = t(_, Children).

isLeaf(Tree) :-
   nonvar(Tree),
   Tree = t(_,[]).

appendChildNodesToRootNode(Childs,OldTree,NewTree):-
    OldTree = t(Data,CurChilds),
    append(Childs,CurChilds,NewChilds),
    NewTree = t(Data,NewChilds).

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

subTree(DataTemplate,Tree,SubTree) :-
    Tree = t(Data, Childs),
    (
        Data = DataTemplate ->
            SubTree = Tree
        ;
            findSubTree(DataTemplate,Childs, SubTree)
    ).

findSubTree(_,[],_) :- fail.
findSubTree(DataTemplate, [ Child | Childs], SubTrees) :-
    subTree(DataTemplate,Child,SubTrees)
    ;
    findSubTree(DataTemplate,Childs,SubTrees).
