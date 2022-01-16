(* ::Package:: *)

(* ::Section::Initialization:: *)
(*Header comments*)


(* ::Input::Initialization:: *)
(* :Title: FigShape *)
(* :Context: SciDraw` *)
(* :Summary: Basic drawing shapes *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Copyright: Copyright FIGYEAR, Mark A. Caprio *)
(* :Package Version: FIGVERSION *)
(* :Mathematica Version: MATHVERSION *)
(* :Discussion: FIGDISCUSSION *)
(* :History: See package information file. *)


(* ::Section::Initialization:: *)
(*Begin package*)


(* ::Subsection::Initialization:: *)
(*Package context definition*)


(* ::Input::Initialization:: *)
BeginPackage["SciDraw`",SciDraw`Private`$ExternalContexts];


(* ::Input::Initialization:: *)
Unprotect[Evaluate[$Context<>"*"]];


(* ::Subsection::Initialization:: *)
(*Object usage*)


(* ::Input::Initialization:: *)
FigTest1::usage="FIGURE OBJECT.";


(* ::Subsection::Initialization:: *)
(*Begin private context*)


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ::Subsection::Initialization:: *)
(*Dependencies*)


(* ::Input::Initialization:: *)



(* ::Section::Initialization:: *)
(*FigLine*)


(* ::Subsection::Initialization:: *)
(*Object declaration*)


(* ::Input::Initialization:: *)
DeclareFigClass[
FigTest1,
{"Points"},  (* data members *)
{},  (* member functions *)
{}  (* attached labels *)
];
DefineFigClassOptions[
FigTest1,
{
(* curve/arrowhead *)
FigArrowheadOptions[False,False],
FigCurveOptions,
"Draw"->True
}
];


(* ::Subsection::Initialization:: *)
(*Constructor*)


(* ::Input::Initialization:: *)
Constructor[Class:FigTest1,Self_Object][Curve:FigCurvePattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
Module[
{CanvasPoints,InterpolationFunction},

(* validate extra options *)
FigCheckArrowheadOptions[Self];
FigCheckCurveOptions[Self];

(* prerequisite calculations *)
CanvasPoints=FigResolveCurve[Self,Curve,FigOptions];

(* save data needed for anchor generation *)
Self@SetPoints[CanvasPoints];

(* make graphics elements *)
If[
TrueQ[("Draw"/.FigOptions)],
(* curve *)
FigLineElement[
{Line[CanvasPoints]},
FigOptions
];
(* arrowheads *)
FigLineElement[
{Line[FigCurveArrowheadPoints[
Self@MakeAnchor[Tail,None],
Self@MakeAnchor[Head,None],
FigOptions
]]},
Flatten[{LineDashing->None,FigOptions}]
]
];

]
];


(* ::Subsection::Initialization:: *)
(*Methods*)


(* ::Input::Initialization:: *)
MakeAnchor[Class:FigTest1,Self_Object][Name_,Arg:_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]
];


(* ::Input::Initialization:: *)
MakeBoundingBox[Class:FigTest1,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(* ::Section::Initialization:: *)
(*End package*)


(* ::Subsection::Initialization:: *)
(*Exit private context*)


(* ::Input::Initialization:: *)
End[];


(* ::Subsection::Initialization:: *)
(*Exit package context*)


(* ::Input::Initialization:: *)
Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
