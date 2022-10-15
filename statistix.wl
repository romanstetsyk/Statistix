(* ::Package:: *)

BeginPackage["Statistix`"];


z1CI::usage = "One Sample Z Confidence Interval"


t1CI::usage = "One Sample T Confidence Interval"


p1CI::usage = "One Sample Proportion Confidence Interval"


Begin["`Private`"];


z1CI[]:=Manipulate[
	DynamicModule[{zcrit, stderr, marerr, llci, ulci, roundZcrit, decZcrit, roundSE, decSE, roundME, decME, roundCI, decCI},
		zcrit:=Dynamic@If[
			useRounded,
			roundZcrit@RealAbs@N@Quantile[NormalDistribution[],(1-conflevel)/2],
			RealAbs@N@Quantile[NormalDistribution[],(1-conflevel)/2]
		];
		stderr:=Dynamic@If[
			useRounded,
			roundSE@N[\[Sigma]/Sqrt[n]],
			N[\[Sigma]/Sqrt[n]]
		];
		marerr:=Dynamic@If[
			useRounded,
			roundME[Setting[zcrit]*Setting[stderr]],
			Setting[zcrit]*Setting[stderr]
		];
		llci:=Dynamic@If[
			useRounded,
			roundCI[xbar-Setting[marerr]],
			xbar-Setting[marerr]
		];
		ulci:=Dynamic@If[
			useRounded,
			roundCI[xbar+Setting[marerr]],
			xbar+Setting[marerr]
		];
		
		Style[
			Column[
				{
					Grid[
						Transpose@{
							{"","","Round?"},
							{"\!\(\*SubscriptBox[\(Z\), \(crit\)]\)",Dynamic@roundZcrit@Setting@zcrit,createRoundingFunction[roundZcrit,decZcrit,0.001, True]},
							{"Std. Error",Dynamic@roundSE@Setting@stderr,createRoundingFunction[roundSE,decSE,0.0001]},
							{"ME",Dynamic@roundME@Setting@marerr,createRoundingFunction[roundME,decME,0.01]},
							{"Lower.L.",Dynamic@roundCI@Setting@llci,createRoundingFunction[roundCI,decCI,0.0001]},
							{"Upper.L.",Dynamic@roundCI@Setting@ulci,SpanFromLeft}
						},
						Alignment->Left,
						Dividers->Center,
						Spacings->{Automatic, 1.5}
					],
					
					Dynamic@If[
						CIShowWork,
						(* if CIShowWork is true *)
						Dynamic@TraditionalForm@Column[
							{
								Spacer[{0,20}],
								StringForm[
									"\!\(\*OverscriptBox[\(x\), \(\[LongDash]\)]\) \[PlusMinus] \!\(\*SubscriptBox[\(z\), \(\[Alpha]/2\)]\)\!\(\*FractionBox[\(\[Sigma]\), SqrtBox[\(n\)]]\) \[LongEqual] `1` \[PlusMinus] `2` \[Times]\!\(\*FractionBox[\(`3`\), SqrtBox[\(`4`\)]]\) \[LongEqual] `1` \[PlusMinus] `2` \[Times] `5` \[LongEqual] `1` \[PlusMinus] `6`",
									xbar, Dynamic@roundZcrit@Setting@zcrit, \[Sigma], n, Dynamic@roundSE@Setting@stderr, Dynamic@roundME@Setting@marerr
								],
								StringForm[
									"(`1` - `2`, `1` + `2`) \[LongEqual] (`3`, `4`)",
									xbar, Dynamic@roundME@Setting@marerr, Dynamic@roundCI@Setting@llci, Dynamic@roundCI@Setting@ulci
								]
							},
							AllowScriptLevelChange->False,
							Spacings->1.5
						],
						(* if CIShowWork is false *)
						""
					]
				},
				AllowScriptLevelChange->False
			],
			"DialogStyle"
		]
	],
	
	(*Manipulate controllers*)
	Style["One Sample Z Confidence Interval", Bold, Medium],
	OpenerView@{
		"Conditions",
		Column@{
			"The test statistic follows the normal distribution",
			"The population standard deviation, \[Sigma], is known or estimated with high accuracy",
			"The sample is a simple random sample from its population"
		}
	},
	Spacer[1],
	Style["Sample data:", Bold],
	Spacer[0],
	
	{{xbar, 10, StringJoin["Sample Mean, ", ToString[Overscript["x", "-"], StandardForm]]}},
	{{\[Sigma], 1, "Population Std., \[Sigma]"}},
	{{n, 10, "Sample Size, n"}},
	Delimiter,
	{{conflevel, 0.9, "Confidence Level"}},
	Delimiter,
	Row[
		{
			Control@{{CIShowWork, False, "Show work"}, {False, True}},
			Control@{{useRounded, False, "Use rounded values for internal computations"}, {False, True}}
		},
		"|"
	],
	ControlPlacement->Top
]


t1CI[]:=Manipulate[
	DynamicModule[{df, tcrit, stderr, marerr, llci, ulci, roundTcrit, decTcrit, roundSE, decSE, roundME, decME, roundCI, decCI},
		df:=Dynamic[n - 1];
		tcrit:=Dynamic@If[
			useRounded,
			roundTcrit@RealAbs@N@Quantile[StudentTDistribution[Setting[df]],(1-conflevel)/2],
			RealAbs@N@Quantile[StudentTDistribution[Setting[df]],(1-conflevel)/2]
		];
		stderr:=Dynamic@If[
			useRounded,
			roundSE@N[s/Sqrt[n]],
			N[s/Sqrt[n]]
		];
		marerr:=Dynamic@If[
			useRounded,
			roundME[Setting[tcrit]*Setting[stderr]],
			Setting[tcrit]*Setting[stderr]
		];
		llci:=Dynamic@If[
			useRounded,
			roundCI[xbar-Setting[marerr]],
			xbar-Setting[marerr]
		];
		ulci:=Dynamic@If[
			useRounded,
			roundCI[xbar+Setting[marerr]],
			xbar+Setting[marerr]
		];
		
		Style[
			Column[
				{
					Grid[
						Transpose@{
							{"","","Round?"},
							{"d.f.", Dynamic@df,""},
							{"\!\(\*SubscriptBox[\(t\), \(crit\)]\)",Dynamic@roundTcrit@Setting@tcrit,createRoundingFunction[roundTcrit,decTcrit,0.001, True]},
							{"Std. Error",Dynamic@roundSE@Setting@stderr,createRoundingFunction[roundSE,decSE,0.0001]},
							{"ME",Dynamic@roundME@Setting@marerr,createRoundingFunction[roundME,decME,0.01]},
							{"Lower.L.",Dynamic@roundCI@Setting@llci,createRoundingFunction[roundCI,decCI,0.0001]},
							{"Upper.L.",Dynamic@roundCI@Setting@ulci,SpanFromLeft}
						},
						Alignment->Left,
						Dividers->Center,
						Spacings->{Automatic, 1.5}
					],
					
					Dynamic@If[
						CIShowWork,
						(* if CIShowWork is true *)
						Dynamic@TraditionalForm@Column[
							{
								Spacer[{0,20}],
								StringForm["d.f. = n - 1 = `1` - 1 = `2`", n, Dynamic@df],
								StringForm[
									"\!\(\*OverscriptBox[\(x\), \(\[LongDash]\)]\) \[PlusMinus] \!\(\*SubscriptBox[\(t\), \(\[Alpha]/2\)]\)\!\(\*FractionBox[\(s\), SqrtBox[\(n\)]]\) \[LongEqual] `1` \[PlusMinus] `2` \[Times]\!\(\*FractionBox[\(`3`\), SqrtBox[\(`4`\)]]\) \[LongEqual] `1` \[PlusMinus] `2` \[Times] `5` \[LongEqual] `1` \[PlusMinus] `6`",
									xbar, Dynamic@roundTcrit@Setting@tcrit, s, n, Dynamic@roundSE@Setting@stderr, Dynamic@roundME@Setting@marerr
								],
								StringForm[
									"(`1` - `2`, `1` + `2`) \[LongEqual] (`3`, `4`)",
									xbar, Dynamic@roundME@Setting@marerr, Dynamic@roundCI@Setting@llci, Dynamic@roundCI@Setting@ulci
								]
							},
							AllowScriptLevelChange->False,
							Spacings->1.5
						],
						(* if CIShowWork is false *)
						""
					]
				},
				AllowScriptLevelChange->False
			],
			"DialogStyle"
		]
	],
	
	(*Manipulate controllers*)
	Style["One Sample T Confidence Interval", Bold, Medium],
	OpenerView@{
		"Conditions",
		Column@{
			"The test statistic follows the normal distribution",
			"The sample is a simple random sample from its population"
		}
	},
	Spacer[1],
	Style["Sample data:", Bold],
	Spacer[0],
	
	{{xbar, 10, StringJoin["Sample Mean, ", ToString[Overscript["x", "-"], StandardForm]]}},
	{{s, 1, "Sample Std., s"}},
	{{n, 10, "Sample Size, n"}},
	Delimiter,
	{{conflevel, 0.9, "Confidence Level"}},
	Delimiter,
	Row[
		{
			Control@{{CIShowWork, False, "Show work"}, {False, True}},
			Control@{{useRounded, False, "Use rounded values for internal computations"}, {False, True}}
		},
		"|"
	],
	ControlPlacement->Top
]


p1CI[]:=Manipulate[
	DynamicModule[{zcrit, phat, stderr, marerr, llci, ulci, roundPhat, decPhat, roundZcrit, decZcrit, roundSE, decSE, roundME, decME, roundCI, decCI},
		zcrit:=Dynamic@If[
			useRounded,
			roundZcrit@RealAbs@N@Quantile[NormalDistribution[],(1-conflevel)/2],
			RealAbs@N@Quantile[NormalDistribution[],(1-conflevel)/2]
		];
		phat:=Dynamic@If[
			useRounded,
			roundPhat@N[x/n],
			N[x/n]
		];
		stderr:=Dynamic@If[
			useRounded,
			roundSE@N[Sqrt[Setting[phat](1-Setting[phat])/n]],
			N[Sqrt[Setting[phat](1-Setting[phat])/n]]
		];
		marerr:=Dynamic@If[
			useRounded,
			roundME[Setting[zcrit]*Setting[stderr]],
			Setting[zcrit]*Setting[stderr]
		];
		llci:=Dynamic@If[
			useRounded,
			roundCI[Setting[phat]-Setting[marerr]],
			Setting[phat]-Setting[marerr]
		];
		ulci:=Dynamic@If[
			useRounded,
			roundCI[Setting[phat]+Setting[marerr]],
			Setting[phat]+Setting[marerr]
		];
		
		Style[
			Column[
				{
					Grid[
						Transpose@{
							{"","","Round?"},
							{"\!\(\*OverscriptBox[\(p\), \(^\)]\)",Dynamic@roundPhat@Setting@phat,createRoundingFunction[roundPhat,decPhat, 0.00001, False]},
							{"\!\(\*SubscriptBox[\(Z\), \(crit\)]\)",Dynamic@roundZcrit@Setting@zcrit,createRoundingFunction[roundZcrit,decZcrit,0.001, True]},
							{"Std. Error",Dynamic@roundSE@Setting@stderr,createRoundingFunction[roundSE,decSE,0.00001]},
							{"ME",Dynamic@roundME@Setting@marerr,createRoundingFunction[roundME,decME,0.00001]},
							{"Lower.L.",Dynamic@roundCI@Setting@llci,createRoundingFunction[roundCI,decCI,0.00001]},
							{"Upper.L.",Dynamic@roundCI@Setting@ulci,SpanFromLeft}
						},
						Alignment->Left,
						Dividers->Center,
						Spacings->{Automatic, 1.5}
					],
					
					Dynamic@If[
						CIShowWork,
						(* if CIShowWork is true *)
						Dynamic@TraditionalForm@Column[
							{
								Spacer[{0,20}],
								StringForm["\!\(\*OverscriptBox[\(p\), \(^\)]\)\[MediumSpace]\[LongEqual]\[MediumSpace]\!\(\*FractionBox[\(\(x\)\(\[MediumSpace]\)\), \(n\)]\)\[MediumSpace]\[LongEqual]\[MediumSpace]\!\(\*FractionBox[\(`1`\), \(`2`\)]\)\[MediumSpace]\[LongEqual]\[MediumSpace]`3`", x, n, Dynamic@roundPhat@Setting@phat],
								StringForm["`1`\[MediumSpace]\[LongEqual]\[MediumSpace]\!\(\*SqrtBox[FractionBox[
StyleBox[
RowBox[{OverscriptBox[\"p\", \"^\"], 
RowBox[{\"(\", 
RowBox[{\"1\", \"-\", OverscriptBox[\"p\", \"^\"]}], \")\"}]}],\nShowSpecialCharacters->False,\nShowStringCharacters->True,\nNumberMarks->True], \"n\"]]\)\[MediumSpace]\[LongEqual]\[MediumSpace]\!\(\*SqrtBox[FractionBox[\(`2` \((1 - `2`)\)\), \(`3`\)]]\)\[MediumSpace]\[LongEqual]\[MediumSpace]`4`",Subscript["\[Sigma]","\!\(\*OverscriptBox[\(p\), \(^\)]\)"], Dynamic@roundPhat@Setting@phat, n, Dynamic@roundSE@Setting@stderr],
								StringForm["ME\[MediumSpace]\[LongEqual]\[MediumSpace]\!\(\*SubscriptBox[\(z\), \(crit\)]\)\[MediumSpace]`1`\[MediumSpace]\[LongEqual]\[MediumSpace]`2`\[MediumSpace]\[Times]\[MediumSpace]`3`\[MediumSpace]\[LongEqual]\[MediumSpace]`4`", Subscript["\[Sigma]","\!\(\*OverscriptBox[\(p\), \(^\)]\)"], Dynamic@roundZcrit@Setting@zcrit, Dynamic@roundSE@Setting@stderr, Dynamic@roundME@Setting@marerr],
								StringForm["(\!\(\*OverscriptBox[\(p\), \(^\)]\)\[MediumSpace]-\[MediumSpace]ME, \!\(\*OverscriptBox[\(p\), \(^\)]\)\[MediumSpace]+\[MediumSpace]ME)\[MediumSpace]\[LongEqual]\[MediumSpace](`1`\[MediumSpace]-\[MediumSpace]`2`, `1`\[MediumSpace]+\[MediumSpace]`2`)\[MediumSpace]\[LongEqual]\[MediumSpace](`3`, `4`)", Dynamic@roundPhat@Setting@phat, Dynamic@roundME@Setting@marerr, Dynamic@roundCI@Setting@llci, Dynamic@roundCI@Setting@ulci]
							},
							AllowScriptLevelChange->False,
							Spacings->1.5
						],
						(* if CIShowWork is false *)
						""
					]
				},
				AllowScriptLevelChange->False
			],
			"DialogStyle"
		]
	],
	
	(*Manipulate controllers*)
	Style["One Sample Proportion Confidence Interval", Bold, Medium],
	OpenerView@{
		"Conditions",
		Column@{
			"The sample is a simple random sample",
			"The conditions for the binomial distribution are satisfied",
			"There are at least 5 successes and at least 5 failures"
		}
	},
	Spacer[1],
	Style["Sample data:", Bold],
	Spacer[0],
	
	{{x, 50, "# of Successes, x"}},
	{{n, 100, "# of Observations, n"}},
	Delimiter,
	{{conflevel, 0.9, "Confidence Level"}},
	Delimiter,
	Row[
		{
			Control@{{CIShowWork, False, "Show work"}, {False, True}},
			Control@{{useRounded, False, "Use rounded values for internal computations"}, {False, True}}
		},
		"|"
	],
	ControlPlacement->Top
]


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