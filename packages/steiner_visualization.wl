(* ::Package:: *)

BeginPackage["Steiner`Visualization`"];


Needs["Steiner`Utilities`", NotebookDirectory[]~~"\\packages\\steiner_utilities.wl"]


terminalVertexStyle::usage         = "Makes all terminals Red.";
voronoiVertexStyle::usage          = "Colors vertex of one voronoi cell in one color picked from ColorData[\"SunsetColors\"], for each cell.";
drawGraph::usage                   = "Draws graph and highlights needed vertices (Option VertexStyleFunc).";
drawGraphSubgraph::usage           = "Highlights subgraphEdges in graph.";
gridResult::usage                  = "";
gridResultCompare::usage           = "";
drawGraphSubgraphDifference::usage = "";


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

Options[drawGraphSubgraph] = Join[Options[Graph], {VertexStyleFunc -> {}, TerminalSize->0.5, TerminalShape->"Square"}];

drawGraphSubgraph[graph_Graph, subgraphEdges_, terminalVertices_,  opts:OptionsPattern[]]:= 
drawGraph[graph,terminalVertices,
EdgeStyle->(#->{Orange, Thickness[0.015]}&/@subgraphEdges),
FilterRules[{opts}, Options[drawGraph]]]


(* ::Input::Initialization::Plain:: *)
ClearAll[drawGraphSubgraphDifference]

Options[drawGraphSubgraphDifference] = Join[Options[Graph], {VertexStyleFunc -> {}, TerminalSize->0.5, TerminalShape->"Square"}];

drawGraphSubgraphDifference[graph_Graph, subgraphEdges_, subgraphEdgesOld_, terminalVertices_,  opts:OptionsPattern[]]:= 
drawGraph[graph,terminalVertices,
EdgeStyle->(Join[
#->{Orange, Thickness[0.015]}&/@subgraphEdges,
#->{Green, Thickness[0.015]}&/@Complement[subgraphEdges, subgraphEdgesOld, SameTest->(ContainsExactly[List@@#1, List@@#2]&)],
#->{Black, Thickness[0.015]}&/@Complement[subgraphEdgesOld, subgraphEdges, SameTest->(ContainsExactly[List@@#1, List@@#2]&)]]),
FilterRules[{opts}, Options[drawGraph]]]


(* ::Input::Initialization::Plain:: *)
ClearAll[gridResult]

Options[gridResult] = Join[Options[Graph], {VertexStyleFunc -> {}, TerminalSize->0.5, TerminalShape->"Square"}];

gridResult[graph_Graph,  terminals_, tree_, treeWeight_, opts:OptionsPattern[]]:=
Grid[{{"Graph", "Tree" , "Tree weight"},
{drawGraphSubgraph[graph, tree, terminals, opts],
drawGraph[Graph[tree], terminals, ImageSize->100],
treeWeight}}, Frame->All]


(* ::Input::Initialization::Plain:: *)
ClearAll[gridResultCompare]

Options[gridResultCompare] = Join[Options[Graph], {VertexStyleFunc -> {}, TerminalSize->0.5, TerminalShape->"Square"}];

gridResultCompare[graph_Graph,  terminals_, tree_, treeOld_, treeWeight_, opts:OptionsPattern[]]:=
Grid[{{"Graph", "Tree" , "Tree weight"},
{drawGraphSubgraphDifference[graph, tree, treeOld, terminals, opts],
drawGraph[Graph[tree], terminals, ImageSize->100],
treeWeight}}, Frame->All]


End[];


EndPackage[]
