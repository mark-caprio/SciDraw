(* ::Package:: *)

(*Header comments*)


(* :Title: FigShape *)
(* :Context: SciDraw` *)
(* :Summary: Basic drawing shapes *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Copyright: Copyright FIGYEAR, Mark A. Caprio *)
(* :Package Version: FIGVERSION *)
(* :Mathematica Version: MATHVERSION *)
(* :Discussion: FIGDISCUSSION *)
(* :History: See package information file. *)


(*Begin package*)


(*Package context definition*)


BeginPackage["SciDraw`",SciDraw`Private`$ExternalContexts];


Unprotect[Evaluate[$Context<>"*"]];


(*Object usage*)


FigTest1::usage="FIGURE OBJECT.";


(*Begin private context*)


Begin["`Private`"];


(*Dependencies*)





(*FigLine*)


(*Object declaration*)


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


(*Constructor*)


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


(*Methods*)


MakeAnchor[Class:FigTest1,Self_Object][Name_,Arg:_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                          FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]
                                                     ];


MakeBoundingBox[Class:FigTest1,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
