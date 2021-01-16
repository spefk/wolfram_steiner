(* ::Package:: *)

BeginPackage["Steiner`Generator`"];


generateSteinerProblemInstance::usage=
"Input: n \[Dash] vertex number; t \[Dash] terminal number.
Options: BernoulliProbablility \[Dash] probability for BernoulliGraphDistribution, WeightBounds \[Dash] {x, y} \[Dash] random weight upper and lower bounds.
Output: {<Connected weighted graph>, edgeWeights_Association, terminals_List}";


Begin["`Private`"];


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


End[];


EndPackage[]
