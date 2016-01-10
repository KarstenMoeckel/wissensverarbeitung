% Autor: Robert Maas
% Datum: 19.12.2015

:- module(tree,[
     appendTree/4, %call: ParentData, NodeData, Tree, NewTree
     isLeaf/1,
     nodeData/2,
     replaceSubTree/4,
     subTree/3
     ]).

nodeData(Node,Data):- Node = t(Data,_).

turnOfNode(Node, Turn) :-
    nodeData(Node, node(_, Turn, _, _)).

valueOfNode(Node, Value) :-
    nodeData(Node, node(_, _, Value, _)).

callsOfNode(Node, Calls) :-
    nodeData(Node, node(_, _, _, Calls)).

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

replaceSubTree(OldData,NewNode, Tree, NewTree) :-
    Tree = t(Data, Childs),
    (
       Data == OldData ->
           NewTree = NewNode
        ;
            replaceChilds(OldData,NewNode,Childs,NewChilds),
            NewTree = t(Data,NewChilds)
    ).

replaceChilds(_,_,[],_) :- fail.
replaceChilds(OldData, NewNode,[TestChild|Childs],NewSubTrees) :-
    replaceSubTree(OldData, NewNode, TestChild, NewChilds) ->
        NewSubTrees = [NewChilds | Childs]
    ;
        replaceChilds(OldData,NewNode,Childs,NewChilds),
        NewSubTrees = [TestChild|NewChilds].

subTree(DataTemplate,Tree,SubTree) :-
    Tree = t(Data, Childs),
    (
        Data = DataTemplate ->
            SubTree = Tree
        ;
            findSubTrees(DataTemplate,Childs, SubTree)
    ).

findSubTree(_,[],_) :- fail.
findSubTree(DataTemplate, [ Child | Childs], SubTrees) :-
    subTree(DataTemplate,Child,SubTrees)
    ;
    findSubTree(DataTemplate,Childs,SubTrees).
