(* ::Package:: *)

BeginPackage["Steiner`Algorithms`GraphUtilities`"];


vertexDegree::usage  = "";
edgeWeight::usage    = "";
edgeWeightSum::usage = "";


Begin["`Private`"];


vertexDegree[vert_, edges_]                    := Count[edges, x_/;MemberQ[x, vert]]


edgeWeight[graph_Graph, edge_]                 := PropertyValue[{graph, edge}, EdgeWeight]


edgeWeightSum[graph_Graph, edgeList_]          := Total[edgeWeight[graph, #]&/@edgeList]


End[];


EndPackage[]
