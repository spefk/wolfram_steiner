(* ::Package:: *)

BeginPackage["Steiner`Visualization`"];


Needs["Steiner`Utilities`", NotebookDirectory[]~~"\\packages\\steiner_utilities.wl"]


terminalVertexStyle::usage = "Makes all terminals Red.";
voronoiVertexStyle::usage  = "Colors vertex of one voronoi cell in one color picked from ColorData[\"SunsetColors\"], for each cell.";
drawGraph::usage           = "Draws graph and highlights needed vertices (Option VertexStyleFunc).";
drawGraphSubgraph::usage   = "Highlights subgraphEdges in graph.";
gridResult::usage          = "";


Begin["`Private`"];


(* ::Input::Initialization::Plain:: *)
ClearAll[terminalVertexStyle]

terminalVertexStyle[terminals_]:=(#->{Red}&/@terminals)


(* ::Input::Initialization::Plain:: *)
ClearAll[voronoiVertexStyle]

voronoiVertexStyle[vertexList_, snm_List]:=
Composition[
Thread[vertexList->#]&,
snm/.#&,
Thread[#->ColorData["SunsetColors"]/@Range[0.2, 0.9, 0.7/(Length@# - 1)]]&,
DeleteDuplicates[snm]&
][]


(* ::Input::Initialization::Plain:: *)
ClearAll[drawGraph]
clearAndProtect@{VertexStyleFunc, TerminalSize, TerminalShape};

Options[drawGraph] = Join[Options[Graph], {VertexStyleFunc -> {}, TerminalSize->0.5, TerminalShape->"Square"}];

drawGraph[graph_Graph, terminals_, opts:OptionsPattern[]] :=
    Graph[graph,
{FilterRules[{opts}, Options[Graph]],
VertexStyle -> Composition[If[#!={}, #,  terminalVertexStyle[terminals]]&][OptionValue[VertexStyleFunc]],
VertexSize -> (# -> OptionValue[TerminalSize]& /@ terminals),
VertexShapeFunction->(# -> OptionValue[TerminalShape]& /@ terminals),
VertexLabels -> Automatic,
VertexLabelStyle -> Directive[Black, 10],
EdgeLabels -> Placed["EdgeWeight", Tooltip],
ImageSize -> 300}]


(* ::Input::Initialization::Plain:: *)
ClearAll[drawGraphSubgraph]

Options[drawGraphSubgraph] = Options[Graph];

drawGraphSubgraph[graph_Graph, subgraphEdges_, terminalVertices_,  opts:OptionsPattern[]]:= 
drawGraph[graph,terminalVertices,
EdgeStyle->(#->{Orange, Thickness[0.015]}&/@subgraphEdges),
FilterRules[{opts}, Options[drawGraph]]]


(* ::Input::Initialization::Plain:: *)
ClearAll[gridResult]

gridResult[graph_Graph,  terminals_, tree_, treeWeight_]:=Grid[{{"Graph", "Tree" , "Tree weight"},
{drawGraphSubgraph[graph, tree, terminals],
drawGraph[Graph[tree], terminals, ImageSize->100],
treeWeight}}, Frame->All]


End[];


EndPackage[]
