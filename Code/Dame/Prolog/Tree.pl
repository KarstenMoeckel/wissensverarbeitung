/** <tree>

Module to create a tree.

@author Robert Maas
@date 19.12.2015
*/

:- module(tree,[
     appendTree/4, %call: ParentData, NodeData, Tree, NewTree
     isLeaf/1,
     nodeData/2,
     subTree/3,
     nodeChildren/2,
     appendChildNodesToRootNode/3,
     createNode/3
     ]).

/**
 * Predicate to get the data of a Tree.
 * @param Node The node to get the data from.
 * @return Data The data of the Tree.
 */
nodeData(Node,Data):- Node = t(Data,_).

/**
 * Predicate to get the Children of a Tree.
 * @param Node The node to get the children from.
 * @return Children The children of the node.
 */
nodeChildren(Node, Children):- Node = t(_, Children).

/**
 * Predicate to create a node with given data and children.
 * @param Data The data for the node.
 * @param Childs The children for the new node.
 * @return Node The new node.
 */
createNode(Data,Childs,Node) :- Node = t(Data,Childs).

/**
 * Predicate to check if the iven tree is a leaf (has no children).
 * @param Tree The tree to check.
 * @return true if leaf, else false.
 */
isLeaf(Tree) :-
   nonvar(Tree),
   Tree = t(_,[]).

/**
 * Predicate to append children to a given tree.
 * @param Childs The new children of the tree.
 * @param OldTree The tree to append the children.
 * @return NewTree The tree with the appended children.
 */
appendChildNodesToRootNode(Childs,OldTree,NewTree):-
    OldTree = t(Data,CurChilds),
    append(Childs,CurChilds,NewChilds),
    NewTree = t(Data,NewChilds).

/**
 * Predicate to append a tree to another tree.
 * If the parent tree has subtrees, traverse until
 * the node with the correct data is found.
 * @param Parent The tree to append.
 * @param Data The data to compare.
 * @param Tree The new tree.
 * @return NewTree The Tree with the appended Tree.
 */
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

/**
 * Predicate to check if a tree has subtrees.
 * @param Parent
 * @param Data
 * @param Tree List of trees.
 * @return NewSubTrees.
 */
checkSubTrees(Parent,Data,[Tree|SubTrees],NewSubTrees) :-
      appendTree(Parent,Data,Tree,NewTree) ->
         NewSubTrees = [NewTree|SubTrees]
   ;
      checkSubTrees(Parent,Data, SubTrees,NewTree),
      NewSubTrees = [Tree|NewTree].

/**
 * Predicate checks if a subtree maches a DataTemplate.
 * Traverse through children to find the template.
 *
 * @param DataTemplate Template to look for.
 * @param Tree Tree to find the data in.
 * @return SubTree The subtree the matches the pattern.
 */
subTree(DataTemplate,Tree,SubTree) :-
    Tree = t(Data, Childs),
    (
        Data = DataTemplate ->
            SubTree = Tree
        ;
            findSubTree(DataTemplate,Childs, SubTree)
    ).

/**
 * Traverse through all children in a list.
 * @param DataTemplate The Template to look for.
 * @param Children The subtrees.
 * @return Subtrees The found subtree.
 */
findSubTree(_,[],_) :- fail.
findSubTree(DataTemplate, [ Child | Childs], SubTrees) :-
    subTree(DataTemplate,Child,SubTrees)
    ;
    findSubTree(DataTemplate,Childs,SubTrees).
