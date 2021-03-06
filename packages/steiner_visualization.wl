(* ::Package:: *)

BeginPackage["Steiner`Visualization`"];


Needs["Steiner`Utilities`", "steiner_utilities.wl"]


terminalVertexStyle::usage         = "Makes all terminals Red.";
voronoiVertexStyle::usage          = "Colors vertex of one voronoi cell in one color picked from ColorData[\"SunsetColors\"], for each cell.";
drawGraph::usage                   = "Draws graph and highlights needed vertices (Option VertexStyleFunc).";
drawGraphSubgraph::usage           = "Highlights subgraphEdges in graph.";
gridResult::usage                  = "";
gridResultCompare::usage           = "";
drawGraphSubgraphDifference::usage = "";


Begin["`Private`"];


terminalVertexStyle[terminals_] := (#->{Red}&/@terminals)


voronoiVertexStyle[vertexList_, snm_List] :=
	Composition[
		Thread[vertexList->#]&,
		snm/.#&,
		Thread[#->(PadRight[Take[ColorData[35,"ColorList"], UpTo@Length@#], Length@#, ColorData[35,"ColorList"]])]&,
		DeleteDuplicates[snm]&
	][]


clearAndProtect@{VertexStyleFunc, TerminalSize, TerminalShape};

Options[drawGraph] = Join[Options[Graph], {VertexStyleFunc -> {}, TerminalSize->0.5, TerminalShape->"Square"}];

drawGraph[graph_Graph, terminals_, opts:OptionsPattern[]] :=
    Graph[graph,
		{FilterRules[{opts}, Options[Graph]],
		VertexStyle -> (If[#!={}, #,  terminalVertexStyle[terminals]]&[OptionValue[VertexStyleFunc]]),
		VertexSize -> (# -> OptionValue[TerminalSize]& /@ terminals),
		VertexShapeFunction->(# -> OptionValue[TerminalShape]& /@ terminals),
		VertexLabels -> Automatic,
		VertexLabelStyle -> Directive[Black, 10],
		EdgeLabels -> Placed["EdgeWeight", Tooltip],
		ImageSize -> 300}
	]


Options[drawGraphSubgraph] = Join[Options[Graph], {VertexStyleFunc -> {}, TerminalSize->0.5, TerminalShape->"Square"}];

drawGraphSubgraph[graph_Graph, subgraphEdges_, terminalVertices_,  opts:OptionsPattern[]] := 
	drawGraph[
		graph,
		terminalVertices,
		EdgeStyle->(#->{Orange, Thickness[0.015]}&/@subgraphEdges),
		FilterRules[{opts}, Options[drawGraph]]
	]


Options[drawGraphSubgraphDifference] = Join[Options[Graph], {VertexStyleFunc -> {}, TerminalSize->0.5, TerminalShape->"Square"}];

drawGraphSubgraphDifference[graph_Graph, subgraphEdges_, subgraphEdgesOld_, terminalVertices_,  opts:OptionsPattern[]] := 
	drawGraph[
		graph,
		terminalVertices,
		EdgeStyle->(Join[
		#->{Orange, Thickness[0.015]}&/@subgraphEdges,
		#->{Green, Thickness[0.015]}&/@Complement[subgraphEdges, subgraphEdgesOld, SameTest->(ContainsExactly[List@@#1, List@@#2]&)],
		#->{Black, Thickness[0.015]}&/@Complement[subgraphEdgesOld, subgraphEdges, SameTest->(ContainsExactly[List@@#1, List@@#2]&)]]),
		FilterRules[{opts}, Options[drawGraph]]
	]


Options[gridResult] = Join[Options[Graph], {VertexStyleFunc -> {}, TerminalSize->0.5, TerminalShape->"Square"}];

gridResult[graph_Graph, terminals_, tree_, treeWeight_, opts:OptionsPattern[]] :=
	Grid[
		{
			{"Graph", "Tree", "Tree weight"},
			{drawGraphSubgraph[graph, tree, terminals, opts],
			drawGraph[Graph[tree], terminals, ImageSize->100],
			treeWeight}
		},
		Frame->All
	]


Options[gridResultCompare] = Join[Options[Graph], {VertexStyleFunc -> {}, TerminalSize->0.5, TerminalShape->"Square"}];

gridResultCompare[graph_Graph,  terminals_, tree_, treeOld_, treeWeight_, opts:OptionsPattern[]] :=
	Grid[
		{
			{"Graph", "Tree" , "Tree weight"},
			{drawGraphSubgraphDifference[graph, tree, treeOld, terminals, opts],
			drawGraph[Graph[tree], terminals, ImageSize->100],
			treeWeight}
		},
		Frame->All
	]


End[];


EndPackage[]
