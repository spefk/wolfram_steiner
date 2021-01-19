(* ::Package:: *)

BeginPackage["Steiner`Algorithms`Exact`"];


Needs["Steiner`Algorithms`GraphUtilities`", NotebookDirectory[]~~"\\packages\\steiner_algorithms_graph_utilities.wl"]


runDreyfusWagner::usage ="
Input: graph \[Dash] weighted Graph instance, terminals \[Dash] list of terminals.
Output: {weight of optimal steiner tree, optimal steiner tree}.";


Begin["`Private`"];


(* ::Input::Initialization::Plain:: *)
runDreyfusWagner[graph_Graph, terminals_]:=
Block[
{vertList = VertexList@graph,
edgeList = EdgeList@graph,
distMatrix = GraphDistanceMatrix[graph],
tLen = Length@terminals,
runDreyfusWagnerQStep,
runDreyfusWagnerPStep,
restoredTreeDreyfusWagner,
restoredTree,
p, q, pAnc, qAnc, tmpMin},


If[tLen<=1, Return[{0, {}}]];


(* Internal cycle for q values *)
runDreyfusWagnerQStep[size_]:=
Scan[Function[U,
Scan[Function[x,
Scan[
q[Union[U, {x}], x] = Infinity;
(tmpMin = (p[Union[#, {x}]]+p[Union[Complement[U, #], {x}]]);
If[q[Union[U, {x}], x]>tmpMin,
q[Union[U, {x}], x] = tmpMin;
qAnc[Union[U, {x}], x] = {"q", {Union[#, {x}], Union[Complement[U, #], {x}]}}]
)&, Subsets[U, {1, size-1}]]][#]&,
Complement[vertList, U]]][#]&,
Sort/@Subsets[terminals, {size}]];

(* Internal cycle for p values *)
runDreyfusWagnerPStep[size_]:=
Scan[Function[U,
Scan[Function[x,
p[Union[U, {x}]] = Infinity;
Scan[
(tmpMin = (p[U] + distMatrix[[x, #]]);
If[p[Union[U, {x}]]>tmpMin,
p[Union[U, {x}]] = tmpMin;
pAnc[Union[U, {x}]] = {"pp", {U,{x, #}}}])&,
U];
Scan[
(tmpMin = (q[Union[U, {#}], #]+ distMatrix[[x, #]]);
If[p[Union[U, {x}]]>tmpMin,
p[Union[U, {x}]] = tmpMin;
pAnc[Union[U, {x}]] = {"pq",{{Union[U, {#}], #} , {x, #}}}])&,
Complement[vertList, U]];][#]&,
Complement[vertList, U]]][#]&,
Sort/@Subsets[terminals, {size}]];

(* Tree resroration *)
restoredTreeDreyfusWagner[]:=
Composition[
DeleteDuplicates[#, Sort@#1==Sort@#2&]&,
Flatten@#&,
Map[UndirectedEdge@@@Partition[#, 2, 1]&, #,{-2}]&
][restoredTreeDreyfusWagner[pAnc[Sort@terminals]]];

restoredTreeDreyfusWagner[{mark_, ancestor_}]:=
Switch[mark,
"sp",ancestor,
"q",restoredTreeDreyfusWagner[pAnc[#]]&/@ancestor,
"pp",{restoredTreeDreyfusWagner[pAnc[ancestor[[1]]]], FindShortestPath[graph, Sequence@@ancestor[[2]]]},
"pq",{restoredTreeDreyfusWagner[qAnc[Sequence@@ancestor[[1]]]], FindShortestPath[graph, Sequence@@ancestor[[2]]]}
];


(* Base definitions for p *)
Scan[(p[#]= distMatrix[[#[[1]], #[[2]]]]; pAnc[#] = {"sp", FindShortestPath[graph, #[[1]], #[[2]]]};)&,
(Sort/@Subsets[vertList, {2}])];

(* Main cycle *)
Scan[(runDreyfusWagnerQStep[#];runDreyfusWagnerPStep[#];)&, Range[2, tLen-1]];


{p[Sort@terminals], restoredTreeDreyfusWagner[]}
]


End[];


EndPackage[]
