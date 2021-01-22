(* ::Package:: *)

BeginPackage["Steiner`Algorithms`Dijkstra`"];


Needs["Steiner`Algorithms`GraphUtilities`", "steiner_algorithms_graph_utilities.wl"]


dijkstra::usage     = "Dijkstra's algorithm. Returns <|\"distance\"\[Rule]<FixedArray[]>, \"ancestors\"\[Rule]<FixedArray[]>|>.";
dijkstraPath::usage = "Retrieve vertex path to vertex according to ancestors <FixedArray[]>.";


Begin["`Private`"];


dijkstra[graph_, start_] :=
	Module[{n = VertexCount@graph, curVert, curWeight, queue, anc, dist, used},
		queue          = CreateDataStructure["PriorityQueue"];
		anc            = CreateDataStructure["FixedArray", n];
		dist           = CreateDataStructure["FixedArray", Infinity, n];
		used           = CreateDataStructure["BitVector", n + 1];

		dist["SetPart", start, 0];
		queue["Push", {0, start}];

		While[!queue["EmptyQ"],
			{curWeight, curVert} = queue["Pop"];
			curWeight *= -1;


			If[used["BitTest", curVert], Continue[]];

			used["BitSet", curVert];

			Scan[
				If[!used["BitTest", #] \[And] dist["Part", #] > curWeight
					+ edgeWeight[graph, curVert\[UndirectedEdge]#],
				(queue["Push", {-(edgeWeight[graph, curVert\[UndirectedEdge]#] + curWeight), #}];
				anc["SetPart", #, curVert];
				dist["SetPart", #, curWeight + edgeWeight[graph, curVert\[UndirectedEdge]#]];)]&,
				AdjacencyList[graph, curVert]];
		];

		<|"distance"->dist, "ancestors"->anc|>
	]


dijkstraPath[curVert_Integer, anc_]    := {dijkstraPathRec[curVert, anc]}
dijkstraPathRec[curVert_Integer, anc_] := Sequence[dijkstraPathRec[anc["Part", curVert], anc], curVert]
dijkstraPathRec[Null, anc_] := Nothing


End[];


EndPackage[]
