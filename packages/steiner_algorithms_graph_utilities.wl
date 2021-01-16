(* ::Package:: *)

BeginPackage["Steiner`Algorithms`GraphUtilities`"];


vertexDegree::usage = "";
edgeWeight::usage = "";
edgeWeightSum::usage = "";


Begin["`Private`"];


(* ::Input::Initialization::Plain:: *)
ClearAll[vertexDegree]

vertexDegree[vert_, edges_] := Count[edges, x_/;MemberQ[x, vert]]


(* ::Code::Initialization::Plain:: *)
ClearAll[edgeWeight]

edgeWeight[graph_Graph, edge_]:=PropertyValue[{graph, edge}, EdgeWeight]


(* ::Input::Initialization::Plain:: *)
ClearAll[edgeWeightSum]

edgeWeightSum[edges_List, weights_Association]:=Total[Lookup[weights, #]&/@edges]


End[];


EndPackage[]
