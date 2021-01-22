(* ::Package:: *)

BeginPackage["Steiner`SteinLib`"];


getInstancesList::usage           = "Gives list of paths to all .stp files in <path>.";
importSteinLibInstance::usage     = "Reads .stp file at <libPath> and returns {Graph[], List[ edge->weight], List[ terminals ]}.";
importSteinLibInstanceList::usage = "TEST Returns an imported steiner problem instance for each path.";
importInfoTable::usage            = "Imports steinlib info-table as associtaion.";


Begin["`Private`"];


stlibFormat = "*.stp";


getInstancesList[path_] := FileNames[stlibFormat, path]


importSteinLibInstance[libPath_] :=
	Composition[
		{
		Graph[
			Sort@VertexList@Keys@First[#],
			Keys@First[#],
			EdgeWeight->Values@First[#]],
		Association@First[#],
		Last[#]
		}&,
		{(Min[#1, #2]\[UndirectedEdge]Max[#1, #2]->#3)&[##]&@@@#["E"], Flatten@#["T"]}&,
		MapAt[ToExpression, #, {All, All, All}]&,
		GroupBy[#, First->Rest]&,
		StringSplit/@#&,
		Cases[#, x_/;StringMatchQ[ToString@x, "E *"|"T *"]]&,
		Flatten@#&,
		Import[#, "Data"]&
	][libPath]


importSteinLibInstanceList[pathList:{__String}] := importSteinLibInstance[#]&/@pathList


importInfoTable[path_] :=
Composition[
Map[Quiet@ToExpression@#&, #, {2}]&,
Function[info,
AssociationThread[Rest@First[info], #]&/@
Association[StringTrim[First[#]]->Rest[#]&/@Rest[info]]][#]&,
 Import[#, "Data"][[1]]&
][path]


End[];


EndPackage[]
