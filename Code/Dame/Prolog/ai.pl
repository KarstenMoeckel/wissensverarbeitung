% Autor: Christian Schuett
% Datum: 30.12.2015

:- module(ai, [
     updateTreeDepth/1
     ]).

:- use_module(simulation).

:- dynamic searchTree/1.
:- dynamic treeDepth/1.

updateTreeDepth(Depth):-
	retractall(treeDepth(_)),
	assertz(treeDepth(Depth)).
