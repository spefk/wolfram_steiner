(* ::Package:: *)

BeginPackage["Steiner`Algorithms`Greedy`"];


Needs["Steiner`Algorithms`GraphUtilities`", NotebookDirectory[]~~"\\packages\\steiner_algorithms_graph_utilities.wl"]
Needs["Steiner`Algorithms`Dijkstra`", NotebookDirectory[]~~"\\packages\\steiner_algorithms_dijkstra.wl"]


steinerShortestPathHeuristic::usage         = "\:0410\:043b\:0433\:043e\:0440\:0438\:0442\:043c \:043d\:0430\:0447\:0438\:043d\:0430\:0435\:0442 \:0441 \:0437\:0430\:0434\:0430\:043d\:043d\:043e\:0433\:043e \:0442\:0435\:0440\:043c\:0438\:043d\:0430\:043b\:0430, \:0438 \:0434\:043e\:0431\:0430\:0432\:043b\:044f\:0435\:0442 \:0432 \:0441\:0442\:0440\:043e\:0438\:043c\:044b\:0439 \:0433\:0440\:0430\:0444 \:0442\:0435\:043a\:0443\:0449\:0438\:0439 \:0431\:043b\:0438\:0437\:0436\:0430\:0439\:0448\:0438\:0439 \:0442\:0435\:0440\:043c\:0438\:043d\:0430\:043b (\:043e\:0442\:043d\:043e\:0441\:0438\:0442\:0435\:043b\:044c\:043d\:043e \:0432\:0441\:0435\:0445 \:0432\:0435\:0440\:0448\:0438\:043d \:0434\:0435\:0440\:0435\:0432\:0430) \:0434\:043e \:0442\:0435\:0445 \:043f\:043e\:0440, \:043f\:043e\:043a\:0430 \:043d\:0435 \:0434\:043e\:0431\:0430\:0432\:0438\:0442 \:0432\:0441\:0435 \:0442\:0435\:0440\:043c\:0438\:043d\:0430\:043b\:044b. \:0417\:0430\:0442\:0435\:043c \:0447\:0438\:0441\:0442\:0438\:0442 \:0433\:0440\:0430\:0444 \:0447\:0442\:043e\:0431\:044b \:043f\:043e\:043b\:0443\:0447\:0438\:0442\:044c \:0434\:0435\:0440\:0435\:0432\:043e.";
steinerRepeatedShortestPathHeuristic::usage = "SPH \:0437\:0430\:043f\:0443\:0441\:043a\:0430\:0435\:0442\:0441\:044f \:0437\:0430\:0434\:0430\:043d\:043d\:043e\:0435 \:043a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e \:0440\:0430\:0437 \:0438\:0437 \:0441\:043b\:0443\:0447\:0430\:0439\:043d\:043e\:0433\:043e \:0442\:0435\:0440\:043c\:0438\:043d\:0430\:043b\:0430 (\:0431\:0435\:0437 \:043f\:043e\:0432\:0442\:043e\:0440\:043e\:0432), \:0437\:0430\:0442\:0435\:043c \:0431\:0435\:0440\:0435\:0442\:0441\:044f \:043b\:0443\:0447\:0448\:0438\:0439 \:0438\:0437 \:0440\:0435\:0437\:0443\:043b\:044c\:0442\:0430\:0442\:043e\:0432.";


Begin["`Private`"];


(* SPH *)


steinerShortestPathHeuristic[graph_, terminals_, startTerminal_,
						     distPassed_:Null, ancPassed_:Null]:=
	Module[
		{n = VertexCount@graph, t = Length@terminals,
		dist, anc, tree, inTree, candidates, curEdge,
		curVert, curPath, curAnc, dij, pushed},

		If[MatchQ[distPassed, Null]\[Or]MatchQ[ancPassed, Null],
			dist            = CreateDataStructure["FixedArray"   ,     n];
			anc             = CreateDataStructure["FixedArray"   ,     n];,
			dist            = distPassed;
			anc             = ancPassed;
		];

		pushed              = CreateDataStructure["BitVector"    , 1 + n];
		inTree              = CreateDataStructure["BitVector"    , 1 + n];
		tree                = CreateDataStructure["Stack"               ];
		candidates          = CreateDataStructure["PriorityQueue"       ];

		inTree["BitSet", startTerminal];

		Do[
			Scan[
				(If[MatchQ[dist["Part", #], Null],
					(dij =  dijkstra[graph, #];
					{dist["Part", #], anc["Part", #]} =
						{dij["distance"], dij["ancestors"]};)];
				If[!pushed["BitTest", #],
					(pushed["BitSet", #];	
					Do[candidates["Push", {dist["Part", #]["Part", k], {#, k}}],
						{k, Complement[terminals, inTree["BitList"]]}];)])&,
			inTree["BitList"]];

			While[!candidates["EmptyQ"],
				curEdge = candidates["Pop"][[2]];
				If[Xor[inTree["BitTest", First[curEdge]], inTree["BitTest", Last[curEdge]]],
					Break[]]
			];

			If[candidates["EmptyQ"], Break[]];

			If[inTree["BitTest", First[curEdge]],
				curVert  = Last[curEdge];
				curAnc   = First[curEdge];,
				curVert  = First[curEdge];
				curAnc   = Last[curEdge];];

			curPath = dijkstraPath[curVert, anc["Part", curAnc]];

			Scan[inTree["BitSet", #]&, curPath];
			Scan[tree["Push", #]&, UndirectedEdge@@@Partition[curPath, 2, 1]];
			,
		t-1];
		
		Composition[
			FixedPoint[
				DeleteCases[#, x_/;
					(FreeQ[terminals, x[[1]]]\[And]vertexDegree[x[[1]],#]==1)
					\[Or](FreeQ[terminals, x[[2]]]\[And]vertexDegree[x[[2]],#]==1)
					]&,
				#]&,
			EdgeList@FindSpanningTree[#]&,
			Subgraph[graph, #]&,
			DeleteDuplicates[#, SameTest->(ContainsExactly[List@@#1, List@@#2]&)]&
		][Normal@tree]
	]


(* RSPH *)


steinerRepeatedShortestPathHeuristic[graph_, terminals_, it_:100]:=
	Module[{dist, anc},
		dist = CreateDataStructure["FixedArray", VertexCount@graph];
		anc  = CreateDataStructure["FixedArray", VertexCount@graph];

		Composition[
			MinimalBy[#, Last][[1, 1]]&,
			{#, edgeWeightSum[graph, #]}&/@#&,
			steinerShortestPathHeuristic[graph, terminals, #, dist, anc]&/@#&,
			RandomSample[terminals, UpTo[it]]&
		][]
	]


End[];


EndPackage[]
