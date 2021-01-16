(* ::Package:: *)

BeginPackage["Steiner`GraphUtilities`"];


Begin["`Private`"];


End[];


AppendTo[$ContextPath, "TestingSystem`Private`"];


(* ::Input::Initialization::Plain:: *)
ClearAll[vertexDegree]

vertexDegree[vert_, edges_] := Count[edges, x_/;MemberQ[x, vert]]


(* ::Code::Initialization::Plain:: *)
ClearAll[edgeWeight]

edgeWeight[graph_Graph, edge_] := PropertyValue[{graph, edge}, EdgeWeight]


(* ::Input::Initialization::Plain:: *)
ClearAll[edgeWeightSum]

edgeWeightSum[graph_Graph, edges_List]                   := Total[edgeWeight[graph, #]& /@ edges]
edgeWeightSum[edges_List, weights_Association] := Total[Lookup[weights, #]& /@ edges]


EndPackage[]
