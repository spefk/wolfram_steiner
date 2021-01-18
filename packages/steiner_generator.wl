(* ::Package:: *)

BeginPackage["Steiner`Generator`"];


generateSteinerProblemInstanceRG::usage = "Like generateSteinerProblemInstance but customizable edges";
generateSteinerProblemInstance::usage =
"Input: n \[Dash] vertex number; t \[Dash] terminal number.
Options: BernoulliProbablility \[Dash] probability for BernoulliGraphDistribution, WeightBounds \[Dash] {x, y} \[Dash] random weight upper and lower bounds.
Output: {<Connected weighted graph>, edgeWeights_Association, terminals_List}";
generate2DSteinerProblemInstance::usage = "Same as generateSteinerProblemInstance, but 2D and reutrns coordinates in addition, and takes m = #E.";


Begin["`Private`"];


ClearAll[generateSteinerProblemInstanceRG]

Options[generateSteinerProblemInstanceRG] = {WeightBounds ->{1, 100}};

generateSteinerProblemInstanceRG[n_:10, m_:15, t_:3, opts:OptionsPattern[]]:=
Module[{graph, weightAssociation},

While[
graph =  RandomGraph[{n, m}];
!ConnectedGraphQ[graph]
];

weightAssociation = Association[#->RandomInteger[OptionValue[WeightBounds]]&/@EdgeList[graph]];

{Graph[graph, EdgeWeight->Values@weightAssociation],
weightAssociation,
RandomSample[VertexList[graph], t]}
]


ClearAll[generateSteinerProblemInstance]

Options[generateSteinerProblemInstance] = {BernoulliProbability -> 0.15, WeightBounds ->{1, 100}};

generateSteinerProblemInstance[n_:10, t_:3, opts:OptionsPattern[]]:=
Module[{graph, weightAssociation},

While[
graph =  RandomGraph[BernoulliGraphDistribution[n, OptionValue[BernoulliProbability]]];
!ConnectedGraphQ[graph]
];

weightAssociation = Association[#->RandomInteger[OptionValue[WeightBounds]]&/@EdgeList[graph]];

{Graph[graph, EdgeWeight->Values@weightAssociation],
weightAssociation,
RandomSample[VertexList[graph], t]}
]


ClearAll[generate2DSteinerProblemInstance, generate2DSteinerProblemInstanceStep]

generate2DSteinerProblemInstance[n_:15, t_:3]:=
Module[{out = {}},

While[True, 
out = generate2DSteinerProblemInstanceStep[n, t];
If[ConnectedGraphQ[First@out], Break[]];
];
out]

generate2DSteinerProblemInstanceStep[n_:15, t_:3] := 
Module[{vertsMap, edges, coords, weightAssociation},
coords = RandomReal[{-100, 100}, {n, 2}];
vertsMap = AssociationThread[Range@n, coords];
edges = Subsets[Range@n, {2}];
weightAssociation = Association@@((#->EuclideanDistance[vertsMap[#[[1]]], vertsMap[#[[2]]]])&/@edges);
{Graph[Range@n, UndirectedEdge@@@edges, EdgeWeight->Values@weightAssociation, VertexCoordinates->coords],
weightAssociation,
RandomSample[Range@n, t]}
]


End[];


EndPackage[]
