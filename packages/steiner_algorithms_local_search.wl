(* ::Package:: *)

BeginPackage["Steiner`Algorithms`LocalSearch`"];


Needs["Steiner`Algorithms`GraphUtilities`", NotebookDirectory[]~~"\\packages\\steiner_algorithms_graph_utilities.wl"]
Needs["Steiner`Algorithms`Voronoi`", NotebookDirectory[]~~"\\packages\\steiner_algorithms_graph_utilities.wl"]
Needs["LeftistHeap`", NotebookDirectory[]~~"packages\\data_structures\\leftist_heap.wl"]


steinerVertexInsertion::usage = "Algorithm tries to add (insert) arbitrary vertex to tree and recalculate mst. It applies all possible insertion.";
steinerVertexInsertionFixedPoint::usage = "Uses steinerVertexInsertion untill possible.";
steinerVertexElimination::usage = "Algorithm tries to eliminate arbitrary steiner(!)-vertex from tree and recalculate mst. It applies all possible eliminations.";
steinerVertexEliminationFixedPoint::usage = "Uses steinerVertexElimination untill possible.";
steinerVIVE::usage = "";
steinerVIVEFixedPoint::usage = "";


Begin["`Private`"];


(* Steiner vertex insertion *)


(* ::Input::Initialization::Plain:: *)
ClearAll[steinerVertexInsertion, steinerVertexInsertionFixedPoint]

steinerVertexInsertion[graph_, tree_]:=
Block[{tryAddVertex},
With[{graphVertices = VertexList@graph},
Module[{treeVertices = VertexList@tree,
solWeight, solution = tree},

tryAddVertex[v_]:=
Composition[
If[ConnectedGraphQ[#[[1]]]\[And]solWeight > #[[2]], 
solWeight        = #[[2]];
solution         = EdgeList[#[[1]]];
treeVertices =  VertexList[solution];]&,
{#,  edgeWeightSum[graph, EdgeList[#]]}&,
FindSpanningTree[#]&,
Subgraph[graph, Union[treeVertices, {#}]]&
][v];

solWeight = edgeWeightSum[graph, EdgeList[tree]];

Scan[tryAddVertex[#]&, Complement[graphVertices, treeVertices]];

solution

]
]
]

steinerVertexInsertionFixedPoint[graph_, tree_, maxSteps_:Infinity]:=FixedPoint[steinerVertexInsertion[graph, #]&, tree, maxSteps]



(* Steiner vertex elimination *)


(* ::Input::Initialization::Plain:: *)
ClearAll[steinerVertexElimination, steinerVertexEliminationFixedPoint]

steinerVertexElimination[graph_, tree_, terminals_] :=
Block[{tryEliminateVertex},
Module[{treeVertices = VertexList @ tree, solution = tree, solWeight},
tryEliminateVertex[v_] :=
Composition[
If[ConnectedGraphQ[#[[1]]] \[And] solWeight > #[[2]],
solWeight = #[[2]];
solution = EdgeList[#[[1]]];
treeVertices = VertexList @ solution;]&,
{#, edgeWeightSum[graph, EdgeList[#]]}&,
FindSpanningTree[#]&,
Subgraph[graph, Complement[treeVertices, {#}]]&][v];

solWeight = edgeWeightSum[graph, EdgeList[tree]];

Scan[tryEliminateVertex[#]&,
Complement[treeVertices, terminals]];

solution
]
]

steinerVertexEliminationFixedPoint[graph_, tree_, terminals_, maxSteps_:Infinity] := FixedPoint[steinerVertexElimination[graph, #, terminals]&, tree, maxSteps]


(* Steiner vertex elimination and steiner vertex insertion *)


ClearAll[steinerVIVE, steinerVIVEFixedPoint]

steinerVIVE[graph_, tree_, terminals_] :=
Composition[
steinerVertexElimination[graph, #, terminals]&,
steinerVertexInsertion[graph, #]&
][tree]

steinerVIVEFixedPoint[graph_, tree_, terminals_, maxStep_:Infinity]:= FixedPoint[steinerVIVE[graph, #, terminals]&, tree, maxStep]


End[];


EndPackage[]
