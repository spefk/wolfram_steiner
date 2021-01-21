(* ::Package:: *)

BeginPackage["Steiner`Algorithms`LocalSearch`"];


Needs["Steiner`Algorithms`GraphUtilities`","steiner_algorithms_graph_utilities.wl"]
Needs["Steiner`Algorithms`Voronoi`", "steiner_algorithms_graph_utilities.wl"]
Needs["LeftistHeap`", "data_structures\\leftist_heap.wl"]


steinerVertexInsertion::usage             = "Local-search. Algorithm tries to add (insert) arbitrary vertex to tree and recalculate mst. It applies all possible insertion.";
steinerVertexInsertionFixedPoint::usage   = "Uses steinerVertexInsertion untill possible.";
steinerVertexElimination::usage           = "Local-search. Algorithm tries to eliminate arbitrary steiner(!)-vertex from tree and recalculate mst. It applies all possible eliminations.";
steinerVertexEliminationFixedPoint::usage = "Uses steinerVertexElimination untill possible.";
steinerVIVE::usage                        = "Uses steinerVertexInsertion and then steinerVertexElimination.";
steinerVIVEFixedPoint::usage              = "Uses VIVA untill possible.";
steinerKeyPathExchange::usage             = "Local-search. Tries to exchange path between each path-connected key-vertices to shorter.";
steinerKeyPathExchangeFixedPoint::usage   = "Uses steinerKeyPathExchange untill possible.";
steinerPEVIVE::usage                      = "Uses steinerKeyPathExchange, then steinerVertexInsertion and then steinerVertexElimination.";
steinerPEVIVEFixedPoint::usage            = "Uses steinerPEVIVE untill possible.";


Begin["`Private`"];


(* Steiner vertex insertion *)


steinerVertexInsertion[graph_, tree_]:=
	Block[{tryAddVertex},
		With[{graphVertices = VertexList@graph},
			Module[{treeVertices = VertexList@tree,
				solWeight, solution = tree},

				tryAddVertex[v_]:=
					Composition[
						If[ConnectedGraphQ[#[[1]]]\[And]solWeight > #[[2]], 
							Sow[{solution, EdgeList[#[[1]]], "VI"}, "SolutionChange"];
							solWeight        =  #[[2]];
							solution         = EdgeList[#[[1]]];
							treeVertices     =  VertexList[solution];
						]&,
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


steinerVertexElimination[graph_, tree_, terminals_] :=
	Block[{tryEliminateVertex},
		Module[{treeVertices = VertexList@tree, solution=tree, solWeight},
			solWeight = edgeWeightSum[graph, EdgeList[tree]];

			tryEliminateVertex[v_] :=
				Composition[
					If[ConnectedGraphQ[#[[1]]] \[And] solWeight > #[[2]],
						Sow[{solution, EdgeList[#[[1]]], "VE"}, "SolutionChange"];
						solWeight    = #[[2]];
						solution     = EdgeList[#[[1]]];
						treeVertices = VertexList @ solution;
					]&,
					{#, edgeWeightSum[graph, EdgeList[#]]}&,
					FindSpanningTree[#]&,
					Subgraph[graph, Complement[treeVertices, {#}]]&
				][v];

			Scan[tryEliminateVertex[#]&,
			Complement[treeVertices, terminals]];

			solution
		]
	]

steinerVertexEliminationFixedPoint[graph_, tree_, terminals_, maxSteps_:Infinity] := FixedPoint[steinerVertexElimination[graph, #, terminals]&, tree, maxSteps]


(* Steiner vertex elimination and steiner vertex insertion *)


steinerVIVE[graph_, tree_, terminals_] :=
	Composition[
		steinerVertexElimination[graph, #, terminals]&,
		steinerVertexInsertion[graph, #]&
	][tree]

steinerVIVEFixedPoint[graph_, tree_, terminals_, maxStep_:Infinity] :=
	FixedPoint[steinerVIVE[graph, #, terminals]&, tree, maxStep]


(* Key-path Exchange *)


crucialQ[v_, tree_, terminals_] := MemberQ[terminals, v]\[Or](VertexDegree[tree, v]>=3)


rootTreeKeyPath[tree_, graphVertexNumber_, terminals_]:=
	Module[{root, depth, parent, children},
		root        = FirstCase[VertexList@tree, x_/;crucialQ[x, tree, terminals]];
		depth       = CreateDataStructure["FixedArray", graphVertexNumber];
		parent      = CreateDataStructure["FixedArray", graphVertexNumber];
		children    = CreateDataStructure["FixedArray", graphVertexNumber];
		
		Scan[children["SetPart", #, CreateDataStructure["Stack"]]&, VertexList@tree];

		DepthFirstScan[EdgeList@tree, root,
			{"DiscoverVertex"->
			((depth["SetPart", #1, #3];
			If[#2!=#1, children["Part", #2]["Push", #1]];
			parent["SetPart", #1, #2];)&)}
		];

		{root, depth, parent, children}
	]



steinerKeyPathExchange[graph_, tree_, terminals_]:=
	Block[{lheap, dfs, tryEliminatePath,
		extractMinUntill, downUpEdgeQ, currentInternalKeyPath},
		currentInternalKeyPath = CreateDataStructure["Stack"];
		With[
			{
				treeVertexNumber  = VertexCount@tree,
				graphVertexNumber = VertexCount@graph,
				treeEdges         = EdgeList@tree,
				graphEdges        = EdgeList@graph,
				graphVertices     = VertexList@graph,
				treeVertices      = VertexList@tree
			},
			Module[
				{voronoi, dist, anc, cent, heapArray, solution,
				root, depth, parent, children, graphTreeEdges, disj,
				baseBoundaryEdges, eliminatedPath, boundaryEdges, toHeapify},

				solution       = treeEdges;
				graphTreeEdges = EdgeList@Subgraph[graph, treeVertices];

				(* voronoi *)
				voronoi = dijkstraVoronoi[graph, treeVertices];
				anc     = voronoi["ancestors"];
				dist    = voronoi["distance"];
				cent    = voronoi["voronoi"];

				(* boundary edges push to heaps and stacks *)
				baseBoundaryEdges = Select[graphEdges, cent["Part", First[#]] != cent["Part", Last[#]]&];

				boundaryEdges = CreateDataStructure["FixedArray", graphVertexNumber];
				Scan[boundaryEdges["SetPart", #, CreateDataStructure["Stack"]]&, treeVertices];

				(* Push is costly for current implementation of leftist heap. *)
				(* It is better now (and in general) to firstly create arrays of elements, and then heapify them (because push just melds old heap with new heap of size 1), if it is possible. *)
				toHeapify = CreateDataStructure["FixedArray", graphVertexNumber];
				Scan[toHeapify["SetPart", #, CreateDataStructure["Stack"]]&, treeVertices];

				Scan[
					(toHeapify["Part", cent["Part", First[#]]]["Push", {#, dist["Part", First[#]]
						+ dist["Part", Last[#]] + edgeWeight[graph, #]}];
					toHeapify["Part", cent["Part", Last[#]]]["Push", {#, dist["Part", First[#]]
						+ dist["Part", Last[#]] + edgeWeight[graph, #]}];
					boundaryEdges["Part", cent["Part", First[#]]]["Push", #];
					boundaryEdges["Part", cent["Part", Last[#]]]["Push", #];)&,
				baseBoundaryEdges];

				Scan[(lheap[#]=leftistHeapHeapify[Normal@toHeapify["Part", #]])&, treeVertices];

				(* Tree rooting *)
				{root, depth, parent, children} = rootTreeKeyPath[tree, graphVertexNumber, terminals];

				(* To keep all vertices of processed sub-tree in one set. *)
				disj = CreateDataStructure["DisjointSet"];
				Scan[disj["Insert", #]&, treeVertices];
				disj["Insert", "Forbidden"];

				downUpEdgeQ[v_, u_, down_, disj_, internalPath_]:=
					And[
						Xor[disj["CommonSubsetQ", cent["Part", v], down],
							disj["CommonSubsetQ", cent["Part", u], down]],
						And[FreeQ[internalPath, cent["Part", v]],
							FreeQ[internalPath, cent["Part", u]]],
						Nor[disj["CommonSubsetQ", cent["Part", v], "Forbidden"],
							disj["CommonSubsetQ", cent["Part", u], "Forbidden"]]
					];

				extractMinUntill[heapID_, internalPath_]:=
					NestWhile[
						leftistExtractMin[lheap[heapID]]&,
						Null,
						!MatchQ[#, Null]\[And]!downUpEdgeQ[cent["Part", First@#], cent["Part", Last@#],
							heapID, disj, internalPath]&,
						{2,1}
					];

				tryEliminatePath[pathStack_, u_, v_]:=
					Module[
						{
							edgeBase, brokenTree,
							baseBit, bottomTree, upperTree, prevSol, curPathWeight,
							deletedZones, curBoundaryEdges, path, pathFinal,
							currentInternalKeyPathNormal = Normal@currentInternalKeyPath
						},
						
						path = Normal[pathStack];
						
						brokenTree = VertexDelete[treeEdges, currentInternalKeyPathNormal];
						bottomTree = First@ConnectedGraphComponents[brokenTree, u];
						upperTree  = First@ConnectedGraphComponents[brokenTree, v];

						edgeBase = extractMinUntill[u, currentInternalKeyPathNormal];

						curBoundaryEdges = Normal[boundaryEdges["Part", #]]&/@currentInternalKeyPathNormal;
						curBoundaryEdges = Select[Flatten[Join@@curBoundaryEdges],
												And[True,
													Xor[
														MemberQ[currentInternalKeyPathNormal, cent["Part", First[#]]],
														MemberQ[currentInternalKeyPathNormal, cent["Part", Last[#]]]]
												]
											&];
						pathFinal =
							DeleteCases[
								{voronoiBoundaryPath[edgeBase, anc, cent],
								repairVoronoi[graph, terminals, currentInternalKeyPathNormal,
										      dist, anc, cent, curBoundaryEdges, disj, u]},
							Null];

						If[!MatchQ[pathFinal, {}],
							pathFinal = SortBy[pathFinal, edgeWeightSum[graph, #]&][[1]];
							If[edgeWeightSum[graph, pathFinal] < edgeWeightSum[graph, path],
								prevSol = solution;
								solution = Join[EdgeList[GraphDifference[solution, path]], pathFinal];
								disj["Unify", u, "Forbidden"];
								disj["Unify", #, "Forbidden"]&/@Cases[Flatten[List@@@pathFinal], x_/;VertexDegree[pathFinal, x] == 1];
								disj["Unify", #, "Forbidden"]&/@currentInternalKeyPathNormal;
								Sow[{prevSol, solution, "PE"}, "SolutionChange"];
							]
						];
					];

				dfs[v_]:=
					Module[
						{successors, prevCrus, path},

						successors = Normal[children["Part", v]];

						prevCrus = Null;
						path = Null;

						Scan[Function[child,
							({prevCrus, path} = dfs[child];
							Switch[
								{prevCrus, crucialQ[v, treeEdges, terminals]\[And]!disj["CommonSubsetQ", v, "Forbidden"]},
								{Null    , _}   , Nothing,
								{_Integer, True}, (tryEliminatePath[path, prevCrus, v];
			    								  leftistHeapMeldClearly[lheap[v], lheap[#]]&/@Normal[currentInternalKeyPath];
			    								  currentInternalKeyPath["DropAll"];), 
								_               , Nothing
							];
							If[!disj["CommonSubsetQ", #, "Forbidden"],
								disj["Unify", v, #];]
							;)][#]&,
						successors];

						Which[
							disj["CommonSubsetQ", v, "Forbidden"],
								currentInternalKeyPath["DropAll"];
								{Null, {}},
							crucialQ[v, treeEdges, terminals],
								path = CreateDataStructure["Stack"];
								path["Push", UndirectedEdge[v, parent["Part", v]]];
								{v, path},
							Length@successors == 1 \[And] !MatchQ[prevCrus, Null],
								currentInternalKeyPath["Push", v];
								path["Push", UndirectedEdge[v, parent["Part", v]]];
								{prevCrus, path},
							True,
								{Null, {}}
						]
					];

				dfs[root];
				Sow[Normal@disj, "keyPath"];
				solution
			]
		]
	]


steinerKeyPathExchangeFixedPoint[graph_, tree_, terminals_] :=
	FixedPoint[steinerKeyPathExchange[graph, #, terminals]&, tree]


steinerPEVIVE[graph_, tree_, terminals_]:=
	Composition[
		steinerVertexElimination[graph, #, terminals]&,
		steinerVertexInsertion[graph, #]&,
		steinerKeyPathExchange[graph, #, terminals]&
	][tree]



steinerPEVIVEFixedPoint[graph_, tree_, terminals_] :=
	FixedPoint[steinerPEVIVE[graph, #, terminals]&, tree]



End[];


EndPackage[]
