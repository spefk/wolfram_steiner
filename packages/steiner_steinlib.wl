(* ::Package:: *)

BeginPackage["Steiner`SteinLib`"];


Begin["`Private`"];


(* ::Input::Initialization::Plain:: *)
ClearAll[stlibFormat]
stlibFormat = "*.stp";


End[];


AppendTo[$ContextPath, "TestingSystem`Private`"];


ClearAll[getInstancesList]
getInstancesList::usage = "Gives list of paths to all .stp files in <path>";
getInstancesList[path_] := FileNames[stlibFormat, Directory[]~~path]


(* ::Code::Initialization::Plain:: *)
ClearAll[importSteinLibInstance]

importSteinLibInstance::usage = "Reads .stp file at <libPath> and returns {Graph[], List[ edge->weight], List[ terminals ]}";
importSteinLibInstance[libPath_]:=
Composition[
{Graph[Keys@First[#], EdgeWeight->Values@First[#]], Association@First[#], Last[#]}&,
{(Min[#1, #2]\[UndirectedEdge]Max[#1, #2]->#3)&[##]&@@@#["E"], Flatten@#["T"]}&,
MapAt[ToExpression, #, {All, All, All}]&,
GroupBy[#, First->Rest]&,
StringSplit/@#&,
Cases[#, x_/;StringMatchQ[ToString@x, "E *"|"T *"]]&,
Flatten@#&,
Import[#, "Data"]&
][Directory[]~~libPath]


(* ::Input::Initialization::Plain:: *)
ClearAll[importSteinLibInstanceList]

importSteinLibInstanceList[pathList:{__String}]:=importSteinLibInstance[#]&/@pathList


EndPackage[]
