(* ::Package:: *)

BeginPackage["TestingSystem`"];


testFunction::usage                = "Evaluates <function> on <arguments> <it> times, and returns mean AbsoluteTime and value of final solution.";
testFunctionRandom::usage          = "Evaluates <function> on <arguments> <it> times, where element <PLHDR> in List <arguments> marks argument, that must be filled (positionaly) by output of <randomGenerator>, that is evaluated on <generatorArguments> on each test iteration. Returns mean AbsoluteTime of computations.";
testFunctionUndivided::usage       = "Breaks input to fit input of testFunction.";
testFunctionRandomUndivided::usage = "Breaks input to fit input of testFunctionRandom. Where <fArgs> contains PLCHDR on positions, that must be filled by <generator> output.";
tester::usage                      = ""
testerRandom::usage                = ""


Begin["`Private`"];


Unprotect[PLCHDR];
ClearAll[PLCHDR];
PLCHDR=PLCHDR;
Protect[PLCHDR];


ClearAll[dismemberFunction]
SetAttributes[dismemberFunction, HoldAll]

dismemberFunction[f_[args___, opts___Rule]] := {f, List@args, List@opts}


ClearAll[setMarked]

setMarked[where_, values_] := ReplacePart[where, Thread[Position[where, PLCHDR]->values]]

setMarked[where_, values_] := Throw["WrongMarksNumber"]/;Count[where, PLCHDR]!=Length[values]


ClearAll[testFunction]

testFunction[
function : _Symbol|_Function,
arguments_List,
it_:1000]:=
Composition[
{Mean@First@#, Min@Last@#}&,
Transpose@#&,
Table[AbsoluteTiming@function[Sequence@@arguments], it]&
][]



ClearAll[testFunctionRandom]

testFunctionRandom[
function        : _Symbol|_Function, arguments_List,
randomGenerator : _Symbol|_Function, generatorArguments_List,
it_:1000, identicalOut_:False]:=
Block[{func, rndGen, result},
rndGen[]            := randomGenerator[Apply[Sequence, generatorArguments]];
func[rndValue_List] := function[Apply[Sequence, setMarked[arguments,rndValue]]];
func[rndValue_]     := function[Apply[Sequence, setMarked[arguments,{rndValue}]]];
result[x_, False]   := Mean@First[x];
result[x_, True]    := If[Equal@@Last[x], Mean@First[x], Throw["BadResult"]];

Composition[
result[#, identicalOut]&,
Transpose@#&,
Table[AbsoluteTiming@func[rndGen[]], it]&
][]
]


ClearAll[testFunctionUndivided]

SetAttributes[testFunctionUndivided, HoldAll]
testFunctionUndivided[function_[fArgs___, fOpts___Rule], it_Integer:1000]:=
Module[{func, funcArgs, funcOpts},
{func, funcArgs, funcOpts} = dismemberFunction[function[fArgs, fOpts]];
testFunction[func[##, Sequence@@funcOpts]&, funcArgs, it]
]


ClearAll[testFunctionRandomUndivided]

SetAttributes[testFunctionRandomUndivided, HoldAll]
testFunctionRandomUndivided[function_[fArgs___, fOpts___Rule],
                            generator_[gArgs___, gOpts___Rule],
                            it_Integer:1000, identicalOut_:False]:=
Module[{func, funcArgs, funcOpts, gen, genArgs, genOpts},
{func, funcArgs, funcOpts} = dismemberFunction[function[fArgs, fOpts]];
{gen, genArgs, genOpts}    = dismemberFunction[generator[gArgs, gOpts]];
testFunctionRandom[func[##, Sequence@@funcOpts]&, funcArgs, gen[##, Sequence@@genOpts]&, genArgs, it, identicalOut]
]


ClearAll[tester]

tester[function : _Symbol|_Function, functionListOfInputs : {__List}, it_:1000]:=
Table[{f, testFunction[function, f, it]}, {f, functionListOfInputs}]



ClearAll[testerRandom]

testerRandom[function        : _Symbol|_Function,        functionListOfInputs : {__List},
	         randomGenerator : _Symbol|_Function,        generatorListOfInputs : {__List},
	         it_:1000, identicalOut_:False]:=
Flatten[
Table[{{f, r}, testFunctionRandom[function, f, randomGenerator, r, it, identicalOut]},
{f, functionListOfInputs}, {r, generatorListOfInputs}],
1]


End[];


EndPackage[]
