(* ::Package:: *)

BeginPackage["Steiner`Algorithms`Approximation`"];


Needs["Steiner`Algorithms`GraphUtilities`", NotebookDirectory[]~~"\\packages\\steiner_algorithms_graph_utilities.wl"]
Needs["Steiner`Algorithms`Voronoi`", NotebookDirectory[]~~"\\packages\\steiner_algorithms_voronoi.wl"]


runKouMarkowskyBerman::usage := "
Asymptotic time of first step is O(|T||E| log|V|),
not recommended to use in solving instances with big |T|,
because it builds (full graph) metric closure on T.

Input: graph \[Dash] weighted Graph instance, terminals \[Dash] list of terminals.
Output: edges of 2-approximation of steiner tree.";


runMehlhorn::usage := "
Asymptotic time of first step is O(|E| log|V|) (binary heap dijkstra)(could be better with Fibonacci heap), it is not building a full grah on T,
so could be used in solving instances with big |T|.

Input: graph \[Dash] weighted Graph instance, terminals \[Dash] list of terminals.
Output: edges of 2-approximation of steiner tree.";


Begin["`Private`"];


(* 2-approximation (Kou \[Dash] Markowsky \[Dash] Berman) *)


(* ::Input::Initialization::Plain:: *)
getMetricClosureOfGraphTerminalsTedges[graph_, terminals_, t_]:=
Composition[
Function[weights, UndirectedEdge[Sequence@@Sort[{t, #}]]->weights[[#]]&/@Complement[terminals, {t}]][#]&,
GraphDistance[graph, #]&
][t]

getMetricClosureOfGraphTerminals[graph_Graph, terminals_List]:=
Composition[
Graph[Keys@#, EdgeWeight->#]&,
DeleteDuplicates[#]&,
Flatten@#&,
getMetricClosureOfGraphTerminalsTedges[graph, terminals, #]&/@terminals&
][]


(* ::Input::Initialization::Plain:: *)
runKouMarkowskyBerman[graph_Graph, terminals_]:=
Module[{minSpanningTreeEdges},

minSpanningTreeEdges=
Composition[
EdgeList[#]&,
Subgraph[#, terminals]&,
getMetricClosureOfGraphTerminals[graph, terminals]&
][];

Composition[
Sort/@#&,
FixedPoint[
DeleteCases[#, x_/;
(FreeQ[terminals, x[[1]]]\[And]vertexDegree[x[[1]],#]==1)
\[Or](FreeQ[terminals, x[[2]]]\[And]vertexDegree[x[[2]],#]==1)
]&,
#]&,

EdgeList@FindSpanningTree[#]&,
Subgraph[graph, #]&,

UndirectedEdge@@@#&,
DeleteDuplicates[#, ContainsExactly]&,
Flatten[#, 1]&,
Partition[#, 2, 1]&/@#&,
FindShortestPath[graph, #[[1]], #[[2]]]&/@#&
][minSpanningTreeEdges]
]


(* 2-approximation (Mehlhorn) *)


(* ::Input::Initialization::Plain:: *)
runMehlhorn[graph_, terminals_]:=
Module[
{dijVoronoi, vor, sp, auxilaryEdgeWeights},

dijVoronoi = dijkstraVoronoi[graph, terminals];
vor = dijVoronoi["voronoi"];
sp = dijVoronoi["distance"];

auxilaryEdgeWeights = 
(If[vor["Part", #[[1]]]!=vor["Part", #[[2]]],
UndirectedEdge[vor["Part", #[[1]]],vor["Part", #[[2]]], #]->sp["Part", #[[1]]] + sp["Part", #[[2]]] + edgeWeight[graph, #],
Nothing]&/@EdgeList[graph]);

Composition[
Sort/@#&,
FixedPoint[
DeleteCases[#, x_/;
(FreeQ[terminals, x[[1]]]\[And]vertexDegree[x[[1]],#]==1)
\[Or](FreeQ[terminals, x[[2]]]\[And]vertexDegree[x[[2]],#]==1)
]&,
#]&,

EdgeList@FindSpanningTree[#]&,
Subgraph[graph, #]&,

Flatten[#]&,
{dijkstraFindPath[#[[1]], dijVoronoi["ancestors"]], dijkstraFindPath[#[[2]], dijVoronoi["ancestors"]],#}&/@#&,
EdgeTags@EdgeList@FindSpanningTree@#&,
Graph[Keys@#, EdgeWeight->#]&
][auxilaryEdgeWeights]
]


End[];


EndPackage[]
