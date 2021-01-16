(* ::Package:: *)

(* Functional realisation of Leftist Min-Heap datastructure. *)
(* Implementation uses LinkedList method in WM. *)


BeginPackage["LeftistHeap`"];


leftistHeapCreate::usage  = "Returns node of DataStructure with elements: {priority, elem, <distance to nearest leaf> = 0, <left child> = {}, <right child> = {}}.";
leftistHeapMeld::usage    = "Melds (merges) 2 heaps.";
leftistHeapHeapify::usage = "To create a leftist heap from list of paris {<elem>, <priority>}.";
leftistHeapPush::usage    = "To push a pair {<elem>, <priority>} to an existing leftist heap.";
leftistExtractMin::usage  = "To extract <elem> with !lowest! priority from an existing leftist heap.";


Begin["`Private`"];


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapCreate]

(*({priority, element, closest leaf distance, left child, right child})*)
leftistHeapCreate[] := {}
leftistHeapCreate[{elem_, priority_}] := {elem, priority, 0, {}, {}}


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapMeldSwap, leftistHeapMeldRecalcDistance, leftistHeapMeld]

leftistHeapMeldSwap[x:{xe_, xp_, xd_, xl_, xr_ }, y:{ye_, yp_, yd_, yl_, yr_}] := Sequence[x, y]
leftistHeapMeldSwap[x:{xe_, xp_, xd_, xl_, xr_ }, y:{ye_, yp_, yd_, yl_, yr_}]/;xd<yd := Sequence[y, x]
leftistHeapMeldSwap[x_, {}] := Sequence[x, {}]
leftistHeapMeldSwap[{}, y_] := Sequence[y, {}]

leftistHeapMeldRecalcDistance[dist_, x_, y:{ye_, yp_, yd_, yl_, yr_}] := Sequence[yd+1, x, y]
leftistHeapMeldRecalcDistance[dist_, x_, {}] := Sequence[0, x, {}]

leftistHeapMeld[x:{xe_, xp_, xd_, xl_, xr_ }, y:{ye_, yp_, yd_, yl_, yr_}] := 
{xe, xp, leftistHeapMeldRecalcDistance[xd, leftistHeapMeldSwap[xl, leftistHeapMeld[xr, y]]]}
leftistHeapMeld[x:{xe_, xp_, xd_, xl_, xr_ }, y:{ye_, yp_, yd_, yl_, yr_}]/;xp>yp  := leftistHeapMeld[y, x]
leftistHeapMeld[x_, {}] := x
leftistHeapMeld[{}, y_] := y 


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapHeapify, leftistHeapHeapifyStep]

leftistHeapHeapify[toAdd:{__List}]:=
Block[{$RecursionLimit=Infinity},
leftistHeapHeapifyStep[toAdd]
]

leftistHeapHeapifyStep[toAdd:{__List}]:=
leftistHeapMeld[leftistHeapHeapifyStep[Take[toAdd, Floor@#]], leftistHeapHeapifyStep[Take[toAdd, -Ceiling@#]]]&[Length[toAdd]/2]  
leftistHeapHeapifyStep[toAdd:{x_List}] := leftistHeapCreate[x]


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapPush]
SetAttributes[leftistHeapPush, HoldFirst]
leftistHeapPush[heap_, {elem_, priority_}] := Unevaluated[heap]=leftistHeapMeld[heap, leftistHeapCreate[{elem, priority}]]


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistExtractMin]

leftistExtractMin::usage = "Returns minimum <elem> in <heap>.";
SetAttributes[leftistExtractMin, HoldFirst]
leftistExtractMin[heap_] := 
(Unevaluated[heap]= leftistHeapMeld[heap[[4]], heap[[5]]];#)&[heap[[1]]]



End[];


EndPackage[]
