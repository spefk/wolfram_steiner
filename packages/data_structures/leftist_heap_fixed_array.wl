(* ::Package:: *)

(* Realisation uses FixedArray WM 12.1 DataStructure to carry node of heap. *)


(* ::Subsubsection:: *)
(*Package*)


BeginPackage["LeftistHeap`"];


leftistHeapCreate::usage  = "Returns FixedArray DataStructure with elements: {priority, elem, <distance to nearest leaf> = 0, <left child> = {}, <right child> = {}}."
leftistHeapMeld::usage    = "Melds (merges) 2 heaps.";
leftistHeapHeapify::usage = "To create a leftist heap from list of paris {<elem>, <priority>}.";
leftistHeapPush::usage    = "To push a pair {<elem>, <priority>} to an existing leftist heap.";
leftistExtractMin::usage  = "To extract <elem> with !lowest! priority from an existing leftist heap.";


Begin["`Private`"];


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapCreate]

leftistHeapCreate[] := {}
leftistHeapCreate[{elem_, priority_}] := 
Module[{ds},
ds = CreateDataStructure["FixedArray", {},  5];
ds["SetPart", 3, 0];
ds["SetPart", 2, priority];
ds["SetPart", 1, elem];
ds
]


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapMeldSwap, leftistHeapMeldRecalcDistance, leftistHeapMeld]

leftistHeapMeldSwap[orig_, x_, y_] := If[x["Part", 3] < y["Part", 3], (orig["Part", 4] = y; orig["Part", 5] = x;)];
leftistHeapMeldSwap[orig_, {}, y_] := (orig["Part", 4] = y; orig["Part", 5] = {};);
leftistHeapMeldSwap[orig_, x_, {}] := Nothing;

leftistHeapMeldRecalcDistance[orig_, x_, y_] := (orig["Part", 3] += 1);
leftistHeapMeldRecalcDistance[orig_, x_, {}] := (orig["Part", 3] = 0);

leftistHeapMeld[x_, y_] := 
If[x["Part", 2]<=y["Part", 2],
x["Part", 5] = leftistHeapMeld[x["Part", 5], y];
leftistHeapMeldSwap[x, x["Part", 4], x["Part", 5]];
leftistHeapMeldRecalcDistance[x, x["Part", 4], x["Part", 5]];
x,
leftistHeapMeld[y, x]];

leftistHeapMeld[x_, {}] := x
leftistHeapMeld[{}, y_] := y


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapPush]

leftistHeapPush[heap_, {elem_, priority_}] := leftistHeapMeld[heap, leftistHeapCreate[{elem, priority}]]


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistExtractMin]

leftistExtractMin::usage = "Returns {minimum}";
SetAttributes[leftistExtractMin, HoldFirst]
leftistExtractMin[heap_] := 
Module[{min =heap["Part", 1]},
Unevaluated[heap]= leftistHeapMeld[heap["Part", 4], heap["Part", 5]];
min]


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapNormal]

leftistHeapNormal[heap_]:=
{heap["Part", 1], heap["Part", 2], heap["Part", 3], leftistHeapNormal[heap["Part", 4]], leftistHeapNormal[heap["Part", 5]]}

leftistHeapNormal[{}]:={}


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapHeapify, leftistHeapHeapifyStep]

leftistHeapHeapify[toAdd:{__List}]:=
Block[{$RecursionLimit=Infinity},
leftistHeapHeapifyStep[toAdd]
]

leftistHeapHeapifyStep[toAdd:{__List}]:=
leftistHeapMeld[leftistHeapHeapifyStep[Take[toAdd, Floor@#]], leftistHeapHeapifyStep[Take[toAdd, -Ceiling@#]]]&[Length[toAdd]/2]  
leftistHeapHeapifyStep[toAdd:{x_List}] := leftistHeapCreate[x]


End[]


EndPackage[]
