(* ::Package:: *)

BeginPackage["Steiner`Utilities`"];


clearAndProtect::usage  = "Unprotect[<symbol>] -> ClearAll[<symbol>] -> Protect[<symbol>].";
steinerSolutionQ::usage = "Test if presented solution is feasible solution of steiner tree problem.";


Begin["`Private`"];


(* ::Input::Initialization::Plain:: *)
ClearAll[clearAndProtect]

SetAttributes[clearAndProtect, {HoldAll, Listable}]

clearAndProtect[what_Symbol]:=
(Unprotect[what];
ClearAll[what];
Protect[what];)


ClearAll[steinerSolutionQ]

steinerSolutionQ[tree_, terminals_]:= ConnectedGraphQ@Graph[tree]\[And]ContainsAll[VertexList@tree, terminals]


End[];


EndPackage[]
