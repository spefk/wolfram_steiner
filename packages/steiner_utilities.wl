(* ::Package:: *)

BeginPackage["Steiner`Utilities`"];


clearAndProtect::usage  = "Unprotect[<symbol>] -> ClearAll[<symbol>] -> Protect[<symbol>].";
steinerSolutionQ::usage = "Test if presented solution is feasible solution of steiner tree problem.";
timeDecorator::usage    = "Generator of lambda-function, that gets computation AbsoluteTime and Sow's in with tag <tag>";


Begin["`Private`"];


(* ::Input::Initialization::Plain:: *)
ClearAll[clearAndProtect]

SetAttributes[clearAndProtect, {HoldAll, Listable}]

clearAndProtect[what_Symbol]:=
(Unprotect[what];
ClearAll[what];
Protect[what];)


(* ::Input::Initialization::Plain:: *)
ClearAll[timeDecorator]
timeDecorator[tag_]:=(Sow[#[[1]], tag];[[2]])&[AbsoluteTiming[#]]&


ClearAll[steinerSolutionQ]

steinerSolutionQ[tree_, terminals_]:= ConnectedGraphQ@Graph[tree]\[And]ContainsAll[VertexList@tree, terminals]


End[];


EndPackage[]
