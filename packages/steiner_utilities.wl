(* ::Package:: *)

BeginPackage["Steiner`Utilities`"];


clearAndProtect::usage = "Unprotect[<symbol>] -> ClearAll[<symbol>] -> Protect[<symbol>]"


Begin["`Private`"];


(* ::Input::Initialization::Plain:: *)
ClearAll[clearAndProtect]

SetAttributes[clearAndProtect, {HoldAll, Listable}]

clearAndProtect[what_Symbol]:=
(Unprotect[what];
ClearAll[what];
Protect[what];)


End[];


EndPackage[]
