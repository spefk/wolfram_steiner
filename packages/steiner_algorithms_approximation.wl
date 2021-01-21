(* ::Package:: *)

BeginPackage["Steiner`Algorithms`Approximation`"];


Needs["Steiner`Algorithms`GraphUtilities`", "steiner_algorithms_graph_utilities.wl"]
Needs["Steiner`Utilities`", "steiner_utilities.wl"]
Needs["Steiner`Algorithms`Voronoi`", "steiner_algorithms_voronoi.wl"]


runKouMarkowskyBerman::usage = "
Asymptotic time of first step is O(|T||E| log|V|),
not recommended to use in solving instances with big |T|,
because it builds metric closure on T by recalculating shortest distance (Dijkstra) for each |T|.

Input: graph \[Dash] weighted Graph instance, terminals \[Dash] list of terminals.
Output: edges of 2-approximation of steiner tree.";


runMehlhorn::usage = "
Asymptotic time of algorithm's first step is O(|E| log|V|) (binary heap dijkstra)(could be better with Fibonacci heap),
unlike KouMarkowskyBerman, it is not building a metric closue on T,
and could be run on problem instances with big |T|.

Input: graph \[Dash] weighted Graph instance, terminals \[Dash] list of terminals.
Output: edges of 2-approximation of steiner tree.";


Begin["`Private`"];


(* 2-approximation (Kou \[Dash] Markowsky \[Dash] Berman) *)


getMetricClosureOfGraphTerminalsTedges[graph_, terminals_, t_]:=
	Composition[
		Function[weights,
			(UndirectedEdge[t, #]->weights[[#]])&/@terminals
			][#]&,
		GraphDistance[graph, #]&
		][t]


getMetricClosureOfGraphTerminals[graph_Graph, terminals_List]:=
	Composition[
		Graph[Keys@#, EdgeWeight->Values@#]&,
		DeleteDuplicates[#]&,
		Flatten@#&,
		getMetricClosureOfGraphTerminalsTedges[graph, terminals, #]&/@#&
	][terminals]


runKouMarkowskyBerman[graph_Graph, terminals_]:=
	Module[{minSpanningTreeEdges},

		minSpanningTreeEdges=
		Composition[
			EdgeList[#]&,
			FindSpanningTree[#]&,
			getMetricClosureOfGraphTerminals[graph, terminals]&
		][];

		Composition[
			FixedPoint[
				DeleteCases[#, x_/;
					(FreeQ[terminals, x[[1]]]\[And]VertexDegree[#, x[[1]]]==1)
					\[Or](FreeQ[terminals, x[[2]]]\[And]VertexDegree[#, x[[2]]]==1)
				]&,
			#]&,

			EdgeList@FindSpanningTree[#]&,
			Subgraph[graph, #]&,

			UndirectedEdge@@@#&,
			DeleteDuplicates[#, ContainsExactly[List@@#1, List@@#2]&]&,
			Flatten[#, 1]&,
			Partition[#, 2, 1]&/@#&,
			FindShortestPath[graph, #[[1]], #[[2]]]&/@#&
		][minSpanningTreeEdges]
	]


(* 2-approximation (Mehlhorn) *)


deleteBranches[tree_, terminals_]:=
Block[{dfsBranches, used},

	dfsBranches[vert_, prev_:Null]:=
		Module[{successors = Complement[AdjacencyList[tree, vert], {prev}], f},
			If[MatchQ[successors, {}],
				If[FreeQ[terminals, vert], Sow[vert];True],
				f = And@@(dfsBranches[#, vert]&/@successors);
				If[f\[And]FreeQ[terminals, vert], Sow[vert];True, False]
			]
		];
	
	Composition[
		VertexDelete[tree, #]&,
		Flatten@#&,
		Reap[dfsBranches[First[terminals], Null]][[2]]&
	][]
	]


runMehlhorn[graph_, terminals_]:=
	Module[{dijVoronoi, vor, sp, auxilaryEdgeWeights},

		dijVoronoi = dijkstraVoronoi[graph, terminals];
		vor        = dijVoronoi["voronoi"];
		sp         = dijVoronoi["distance"];

		auxilaryEdgeWeights = 
		If[vor["Part", #[[1]]]!=vor["Part", #[[2]]],
			(UndirectedEdge[vor["Part", #[[1]]], vor["Part", #[[2]]], #] ->
				sp["Part", #[[1]]] + sp["Part", #[[2]]] + edgeWeight[graph, #]),
			Nothing]&/@EdgeList[graph];

		Composition[
			(*FixedPoint[
			DeleteCases[#, x_/;
				(FreeQ[terminals, x[[1]]]\[And]vertexDegree[x[[1]],#]==1)
				\[Or](FreeQ[terminals, x[[2]]]\[And]vertexDegree[x[[2]],#]==1)]&,
			#]&,*)
			EdgeList[#]&,
			deleteBranches[#, terminals]&,
			FindSpanningTree[#]&,
			Subgraph[graph, #]&,

			Flatten[#]&,
			{dijkstraFindPath[#[[1]], dijVoronoi["ancestors"]],
				dijkstraFindPath[#[[2]], dijVoronoi["ancestors"]], #}&/@#&,
			EdgeTags@EdgeList@FindSpanningTree[#]&,
			Graph[Keys@#, EdgeWeight->Values@#]&
		][auxilaryEdgeWeights]
	]


End[];


EndPackage[]
