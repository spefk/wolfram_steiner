(* ::Package:: *)

BeginPackage["Steiner`Algorithms`Voronoi`"];


Needs["Steiner`Algorithms`GraphUtilities`", "steiner_algorithms_graph_utilities.wl"]


dijkstraVoronoi::usage         = "Finds voronoi diagramm of graph. Returns: \[LeftAssociation]\"distance\" \[Rule] FixedArray[<distances>], \"ancestors\" \[Rule] FixedArray[<ancestors>], \"voronoi\" \[Rule] FixedArray[<voronoi centers>]\[RightAssociation]";
dijkstraFindPath::usage        = "Finds path of edges from <vert>'s voronoi terminal to <vert> according to <anc>.";
voronoiBoundaryPath::usage     = "Get path vor boundary edge.";
voronoiBoundaryPathCost::usage = "Get shortest path for boundary edge cost.";
repairVoronoi::usage           = "Used in Key-path Exchange. Recalculates voronoi for the subset of current terminals (centers) using data from original voronoi and returns edge denoting a minmal path between set u belongs and other. Uses <borderEdges> to reinitialize algorithm from vertices on border.";


Begin["`Private`"];


dijkstraVoronoi[graph_Graph, terminals_] :=
	Module[
		{
			spValue            =   CreateDataStructure["FixedArray", Infinity, VertexCount[graph]],
			ancestors          =   CreateDataStructure["FixedArray", Infinity, VertexCount[graph]],
			voronoiCenter      =   CreateDataStructure["FixedArray", Infinity, VertexCount[graph]],
			used               =   CreateDataStructure["BitVector", VertexCount[graph] + 1],
			heap               =   CreateDataStructure["PriorityQueue"],
			orderOfVisit       =   CreateDataStructure["Stack"],
			curVert,
			curWeight
		},

		Scan[
			(spValue["SetPart", #, 0];
			ancestors["SetPart", #, -1];
			voronoiCenter["SetPart", #, #];
			heap["Push", {0, #}];)&,
		terminals];

		While[!heap["EmptyQ"],

			{curWeight, curVert} = heap["Pop"];
			curWeight *= -1;

			If[used["BitTest", curVert], Continue[]];
			used["BitSet", curVert];
			orderOfVisit["Push", curVert];

			Scan[
				If[!used["BitTest", #] \[And] spValue["Part", #] > curWeight
					+ edgeWeight[graph, curVert\[UndirectedEdge]#],
				(heap["Push", {-(edgeWeight[graph, curVert\[UndirectedEdge]#]
					+ curWeight), #}];
				ancestors["SetPart", #, curVert];
				voronoiCenter["SetPart", #, voronoiCenter["Part", curVert]];
				spValue["SetPart", #, curWeight
					+ edgeWeight[graph, curVert\[UndirectedEdge]#]];)]&,
			AdjacencyList[graph, curVert]]
		];

		<|"distance" -> spValue, "ancestors" -> ancestors, "voronoi" -> voronoiCenter, "order" -> Normal@orderOfVisit|>
	]



dijkstraFindPath[vert_, anc_]        := UndirectedEdge@@@Partition[{dijkstraFindPathRec[vert, anc]}, 2, 1]
dijkstraFindPathRec[vert_, anc_]     := Sequence[dijkstraFindPathRec[anc["Part", vert], anc], vert]
dijkstraFindPathRec[-1, anc_]        := Nothing


repairVoronoi[
graph_Graph, terminals_, lostCenters_,
distOld_, ancOld_, centOld_,
boundaryEdges_, disj_, uDown_] :=
	Block[
		{processEdge},
		Module[
			{
				dist           = distOld["Copy"],
				anc            = ancOld["Copy"],
				cent           = centOld["Copy"],
				heap           = CreateDataStructure["PriorityQueue"],
				used           = CreateDataStructure["BitVector", VertexCount[graph] + 1],
				bestPathWeight = Infinity,
				bestPath       = Null,
				curWeight, curVert
			},

			Do[used["BitSet", k], {k, VertexCount[graph]}];

			Scan[If[MemberQ[lostCenters, cent["Part", #]],
				(dist["Part", #] = Infinity;
				anc["Part", #] = Null;
				cent["Part", #] = Null;
				used["BitClear", #];)]&,
			VertexList@graph];

			processEdge[u_, v_] := 
				(If[!used["BitTest", u] \[And] used["BitTest", v] \[And] dist["Part", u] > dist["Part", v]
					+ edgeWeight[graph, v\[UndirectedEdge]u],
				(heap["Push", {-(dist["Part", v]
					+ edgeWeight[graph, v\[UndirectedEdge]u]), u}];
				anc["SetPart", u, v];
				cent["SetPart", u, cent["Part", v]];
				dist["SetPart", u, dist["Part", v]
					+ edgeWeight[graph, v\[UndirectedEdge]u]];)]);

			processEdge[u_, v_ ]/;
				used["BitTest", u]\[And]!used["BitTest", v] := processEdge[v, u];


			Scan[
				processEdge[Sequence@@#]&,
			Cases[boundaryEdges, x_/;
				Xor[used["BitTest", First[x]], used["BitTest", Last[x]]]]];

			While[!heap["EmptyQ"],
				{curWeight, curVert} = heap["Pop"];
				curWeight *= -1;

				If[used["BitTest", curVert],
					Continue[]];
				used["BitSet", curVert];

				Scan[
					(processEdge[#[[1]], #[[2]]];
					If[(used["BitTest", #[[1]]] \[And] used["BitTest", #[[2]]]) \[And]
						Xor[disj["CommonSubsetQ", uDown, cent["Part", #[[1]]]],
							disj["CommonSubsetQ", uDown, cent["Part", #[[2]]]]] \[And]
						Nor[
							disj["CommonSubsetQ", cent["Part", First[#]], "Forbidden"],
							disj["CommonSubsetQ", cent["Part", Last[#]], "Forbidden"]] \[And]
						(bestPathWeight > edgeWeight[graph, #] + dist["Part", #[[1]]] + dist["Part", #[[2]]]),

						(Sow[{"tested", #}, "keyPath"];
						Sow[{"path is", Join[{#}, dijkstraFindPath[#[[1]], anc], dijkstraFindPath[#[[2]], anc]]}, "keyPath"];
						bestPathWeight = edgeWeight[graph, #] + dist["Part", #[[1]]] + dist["Part", #[[2]]];
						bestPath = Join[{#}, dijkstraFindPath[#[[1]], anc], dijkstraFindPath[#[[2]], anc]])
					];)&,
				IncidenceList[graph, curVert]];
			];

			bestPath
		]
	]


voronoiBoundaryPath[Null, _, _] = Null
voronoiBoundaryPath[edge_, par_, centers_]/;
	centers["Part", First[edge]]!=centers["Part", Last[edge]] :=
	Join[{voronoiBoundaryPathRec[First[edge], par]}, {edge}, {voronoiBoundaryPathRec[Last[edge], par]}]

voronoiBoundaryPathRec[vert_, par_] :=
	Sequence[UndirectedEdge[vert, par["Part", vert]], voronoiBoundaryPathRec[par["Part", vert], par]]
voronoiBoundaryPathRec[vert_, par_]/;par["Part", vert]==-1 := Nothing
voronoiBoundaryPathRec[-1, par_]                           := Nothing


ClearAll[voronoiBoundaryPathCost]

voronoiBoundaryPathCost[graph_, edge_, centers_, dist_]/;
	centers["Part", First[edge]] != centers["Part", Last[edge]] :=
	dist["Part", First[edge]] + dist["Part", Last[edge]]+edgeWeight[graph, edge]



End[];


EndPackage[]
