(* ::Package:: *)

BeginPackage["Steiner`Algorithms`Voronoi`"];


Needs["Steiner`Algorithms`GraphUtilities`", NotebookDirectory[]~~"\\packages\\steiner_algorithms_graph_utilities.wl"]


dijkstraVoronoi::usage = "Finds voronoi diagramm of graph. Returns: \[LeftAssociation]\"distance\" \[Rule] FixedArray[<distances>], \"ancestors\" \[Rule] FixedArray[<ancestors>], \"voronoi\" \[Rule] FixedArray[<voronoi centers>]\[RightAssociation]";
dijkstraFindPath::usage = "Finds path of edges from <vert>'s voronoi terminal to <vert> according to <anc>.";
voronoiBoundaryPath::usage = "Get path vor boundary edge.";
voronoiBoundaryPathCost::usage = "Get shortest path for boundary edge cost.";
repairVoronoi::usage = "Recalculates voronoi for the subset of current terminals (centers).";


Begin["`Private`"];


ClearAll[dijkstraVoronoi]

dijkstraVoronoi[graph_Graph, terminals_]:=
Module[
{
spValue       =   CreateDataStructure["FixedArray", Infinity, VertexCount[graph]],
ancestors     =   CreateDataStructure["FixedArray", Infinity, VertexCount[graph]],
voronoiCenter =   CreateDataStructure["FixedArray", Infinity, VertexCount[graph]],
used          =   CreateDataStructure["BitVector", VertexCount[graph] + 1],
heap          =   CreateDataStructure["PriorityQueue"],
orderOfVisit  =   CreateDataStructure["Stack"],
curVert, curWeight
},

Scan[
(spValue["SetPart", #, 0];
ancestors["SetPart", #, -1];
voronoiCenter["SetPart", #, #];
heap["Push", {0, #}];)&, terminals];

While[True,

If[heap["EmptyQ"], Break[]];
	
{curWeight, curVert} = heap["Pop"];
curWeight *= -1;

If[used["BitTest", curVert], Continue[]];
used["BitSet", curVert];
orderOfVisit["Push", curVert];

Scan[
If[!used["BitTest", #] \[And] spValue["Part", #] > curWeight + edgeWeight[graph, curVert\[UndirectedEdge]#],
(heap["Push", {-(edgeWeight[graph, curVert\[UndirectedEdge]#] + curWeight), #}];
ancestors["SetPart", #, curVert];
voronoiCenter["SetPart", #, voronoiCenter["Part", curVert]];
spValue["SetPart", #, curWeight + edgeWeight[graph, curVert\[UndirectedEdge]#]];)]&,
AdjacencyList[graph, curVert]]

];

<|"distance" -> spValue, "ancestors" -> ancestors, "voronoi" -> voronoiCenter, "order" -> Normal@orderOfVisit|>
]



(* ::Input::Initialization::Plain:: *)
ClearAll[dijkstraFindPath, dijkstraFindPathRec]

dijkstraFindPath[vert_, anc_] := UndirectedEdge@@@Partition[{dijkstraFindPathRec[vert, anc]}, 2, 1]

dijkstraFindPathRec[vert_, anc_] := Sequence[dijkstraFindPathRec[anc["Part", vert], anc], vert]
dijkstraFindPathRec[-1, anc_] := Nothing


(* ::Input::Initialization::Plain:: *)
ClearAll[repairVoronoi]

repairVoronoi[] := 1


ClearAll[voronoiBoundaryPath, voronoiBoundaryPathRec]

voronoiBoundaryPath[edge_, par_, centers_]/;centers["Part", First[edge]]!=centers["Part", Last[edge]]:=
Join[{voronoiBoundaryPathRec[First[edge], par]}, {edge}, {voronoiBoundaryPathRec[Last[edge], par]}]

voronoiBoundaryPathRec[vert_, par_] := Sequence[UndirectedEdge@@Sort[{vert, par["Part", vert]}], voronoiBoundaryPathRec[par["Part", vert], par]]
voronoiBoundaryPathRec[vert_, par_]/;par["Part", vert]==-1 := Nothing
voronoiBoundaryPathRec[-1, par_] := Nothing


ClearAll[voronoiBoundaryPathCost]

voronoiBoundaryPathCost[graph_, edge_, centers_, dist_]/;centers["Part", First[edge]]!=centers["Part", Last[edge]]:=
dist["Part", First[edge]]+dist["Part", Last[edge]]+edgeWeight[graph, edge]



End[];


EndPackage[]
