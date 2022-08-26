(* ::Package:: *)

BeginPackage["StatSolver`"];


MyFunc::usage = "This is a test function. \[Sum] x."


Begin["`Private`"];


MyFunc[param_]:=Print[square[param]]


(* Creates control used in Manipulate body to round the numbers dynamically*)
createRoundingFunction[roundingFuncName_, decPlacesVar_, defaultDecPlaces_, roundByDefault_:False]:=Dynamic@Row@{
	Control@{
		{roundingFuncName, If[roundByDefault, Round[#, decPlacesVar]&, Sequence], ""},
		{Sequence, Round[#, decPlacesVar]&},
		ControlType->Checkbox
	},
	Dynamic@If[
		roundingFuncName =!= Sequence,
		Control@{
			{
				decPlacesVar,
				If[!NumericQ[decPlacesVar], defaultDecPlaces, decPlacesVar],
				"to"
			},
			FieldSize->4.5
		},
		""
	]
}


End[];


EndPackage[];
