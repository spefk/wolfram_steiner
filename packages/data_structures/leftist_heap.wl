(* ::Package:: *)

(* Functional realisation of Leftist Min-Heap datastructure. *)
(* Implementation uses LinkedList method in WM. *)


BeginPackage["LeftistHeap`"];


leftistHeapCreate::usage            =    "Returns node of DataStructure with elements: {priority, elem, <distance to nearest leaf> = 0, <left child> = {}, <right child> = {}}.";
leftistHeapMeld::usage              =    "Melds (merges) 2 heaps.";
leftistHeapMeldClearly::usage       =    "Melds (merges) 2 heaps, sets first argument to result, and clears rest 2 variables. Or melds 1st and 2nd, sets 1st, cleares 2nd.";
leftistHeapMeldClearlyList::usage   =    "As leftistHeapMeldClearly but for 1st argument and each of 2nd possibile argument from sequence.";
leftistHeapHeapify::usage           =    "To create a leftist heap from list of paris {<elem>, <priority>}.";
leftistHeapPush::usage              =    "To push a pair {<elem>, <priority>} to an existing leftist heap.";
leftistExtractMin::usage            =    "To extract <elem> with !lowest! priority from an existing leftist heap.";
leftistHeapQ::usage                 =    "Checks (lightly) if argument is leftist heap.";
leftistHeapPeek::usage              =    "Get key of minimum element, but not extract it.";
leftistHeapExtractAll::usage        =    "Get all elements heap contains in arbitrary order.";


Begin["`Private`"];


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapQ]

leftistHeapQ[heap_]:= MatchQ[heap, {_, _, _Integer, _List, _List}]


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapCreate]

(*({priority, element, closest leaf distance, left child, right child})*)
leftistHeapCreate[] := {}
leftistHeapCreate[{elem_, priority_}] := {elem, priority, 0, {}, {}}


(* ::Input::Initialization::Plain:: *)
ClearAll[leftistHeapMeldSwap, leftistHeapMeldRecalcDistance, leftistHeapMeld, leftistHeapMeldClearly]

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


leftistHeapMeld[x_, y_, z__List] := Fold[leftistHeapMeld, x, {y, z}]


ClearAll[leftistHeapMeldClearly, leftistHeapMeldClearlyList]

SetAttributes[leftistHeapMeldClearly, HoldAll]
leftistHeapMeldClearly[new_, x_, y_]:=
(Unevaluated[new]=leftistHeapMeld[x, y];
Unevaluated[x]=.;
Unevaluated[y]=.;)

leftistHeapMeldClearly[x_, y_]:=
(Unevaluated[x]=leftistHeapMeld[x, y];
Unevaluated[y]=.;)

SetAttributes[leftistHeapMeldClearlyList, HoldAllComplete]
leftistHeapMeldClearlyList[new_, x__]:=
(Unevaluated[new]=leftistHeapCreate[];
Scan[leftistHeapMeldClearly[Unevaluated@new, Unevaluated@#]&,
ReleaseHold@Map[Unevaluated, {Hold@x}, {2}]];)



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

SetAttributes[leftistExtractMin, HoldFirst]
leftistExtractMin[heap_] :=
If[MatchQ[Catch[Quiet@Check[First[heap], Throw["EmptyHeap"]]],"EmptyHeap"],
Null, 
(Unevaluated[heap]= leftistHeapMeld[heap[[4]], heap[[5]]];#)&[heap[[1]]]
]



ClearAll[leftistHeapPeek]

leftistHeapPeek[{xe_, xp_, xd_, xl_, xr_ }] := xe;


ClearAll[leftistHeapExtractAll, leftistHeapExtractAllRec]

leftistHeapExtractAll[{xe_, xp_, xd_, xl_, xr_ }] := Reap[leftistHeapExtractAllRec[{xe, xp, xd, xl, xr}]][[2, 1]]
leftistHeapExtractAllRec[{xe_, xp_, xd_, xl_, xr_ }] := (Sow[xe];leftistHeapExtractAllRec[xl];leftistHeapExtractAllRec[xr];)
leftistHeapExtractAllRec[{}] := Null 


End[];


EndPackage[]
