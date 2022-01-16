(* ::Package:: *)

(*Header comments*)


(* :Title: FigWindow *)
(* :Context: SciDraw`FigWindow` *)
(* :Summary: Coordinate system infrastructure *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Copyright: Copyright FIGYEAR, Mark A. Caprio *)
(* :Package Version: FIGVERSION *)
(* :Mathematica Version: MATHVERSION *)
(* :Discussion: FIGDISCUSSION *)
(* :History: See main package file. *)


(*Begin package*)


(*Package context definition*)


BeginPackage["SciDraw`",SciDraw`Private`$ExternalContexts];


Unprotect[Evaluate[$Context<>"*"]];


(*Begin private context*)


Begin["`Private`"];


(*Dependencies*)





(*Coordinate utilities*)


(*Transformation of region*)


(*This function is applicable to a Cartesian rectangular region.  It would need to be extended if rotations are to be included.*)


(*Useful idiom: Transpose[] interconverts*)
(*	region {{x1,x2},{y1,y2}}  <--> corners {{x1,y1},{x2,y2}}*)
(*	*)
(*DEBUGGING: Under Mathematica 8.0.1.0 sporadic incomplete evaluation occurs, resulting in partially evaluated output*)
(*	Transpose[{ScalingTransform[{1,1}][{0,0.`}],ScalingTransform[{1,1}][{576,432.`}]}]*)
(*and subsequent errors in caller functions.  Reason for incomplete evaluation is unknown.*)
(*An episode of this problem may be fixed just by reloading FigWindow.*)
(**)
(*RECOLLECTION: May have been related to accidental scoping of System`Names*)


TransformRegion[TransformationFunction_,r:{{x1_?NumericQ,x2_?NumericQ},{y1_?NumericQ,y2_?NumericQ}}]:=Module[
  {Corners,ScaledCorners},
  Corners=Transpose[r];
  ScaledCorners=Map[TransformationFunction,Corners];
  (*Global`xsc=ScaledCorners;*)
  (*Print[Global`xsc];*)
  (*Print[Evaluate[ScaledCorners]];*)
  Transpose[ScaledCorners]
                                                                                                      ]


(*Window object definition*)


(*Constructor*)


(*root window constructor*)


Constructor[FigWindow,Self_Object][
  r:{{x1_?NumericQ,x2_?NumericQ},{y1_?NumericQ,y2_?NumericQ}}
                                  ]:=Module[
                                    {},

                                    (* region *)
                                    Self@SetRegion[r];

                                    (* transformation *)
                                    Self@SetTFunction[ScalingTransform[{1,1}]];

                                     ];


(*window covering given canvas range*)


Constructor[FigWindow,Self_Object][
  r:{{x1_?NumericQ,x2_?NumericQ},{y1_?NumericQ,y2_?NumericQ}},
  rp:{{x1p_?NumericQ,x2p_?NumericQ},{y1p_?NumericQ,y2p_?NumericQ}}
                                  ]:=Module[
                                    {},

                                    (* region *)
                                    Self@SetRegion[rp];

                                    (* transformation *)
                                    Self@SetTFunction[RescalingTransform[rp,r]];

                                     ];


(*Region methods*)


UserRegion[FigWindow,Self_Object][]:=(Self@GetRegion[]);
CanvasRegion[FigWindow,Self_Object][]:=TransformRegion[(Self@GetTFunction[]),(Self@GetRegion[])];


(*Homogeneous part extraction utility*)


HomogenizeTransform[tfcn_TransformationFunction]:=AffineTransform[TransformationMatrix[tfcn][[1;;2,1;;2]]];


(*Transformation function methods*)


(*Point and displacement transformation functions*)


TFunction[FigWindow,Self_Object][]:=(Self@GetTFunction[]);
InverseTFunction[FigWindow,Self_Object][]:=InverseFunction[Self@GetTFunction[]];
DeltaTFunction[FigWindow,Self_Object][]:=HomogenizeTransform[Self@GetTFunction[]];
ScaledTFunction[FigWindow,Self_Object][]:=Composition[
  (Self@GetTFunction[]),
  RescalingTransform[{{0,1},{0,1}},Self@GetRegion[]]
                                          ];
ScaledDeltaTFunction[FigWindow,Self_Object][]:=HomogenizeTransform[Self@ScaledTFunction[]];


(*Coordinate adjustment*)


(*WithOrigin*)


SetAttributes[WithOrigin,{HoldRest}];


WithOrigin[
  p:FigPointPattern,
  Body_
]:=Module[
  {Window,CanvasShift,NewOrigin},

  (* validation *)
  FigCheckInFigure[WithOrigin];

  (* create window covering same canvas region as current window *)
  Window=FigWindow[CurrentWindow[]@GetRegion[]];

  (* calculate shift amount on canvas *)
  (* must shift user {0,0} to new canvas point p origin *)
  NewOrigin=FigResolvePoint[p];
  CanvasShift=NewOrigin-FigResolvePoint[{0,0}];
  Window@SetTFunction[Composition[TranslationTransform[CanvasShift],CurrentWindow[]@GetTFunction[]]];

  Block[
    {$CurrentWindow=Window},
    (* evaluate body *)
    Body
  ]

   ];
WithOrigin[x_?NumericQ,Body_]:=WithOrigin[{x,0},Body];
DeclareFigFallThroughError[WithOrigin];


(*WithClipping -- UNDER development*)


(*how treat layer?  alas must flatten...*)
(*consider revert to object-by-object layering*)
(**)
(*TO DEBUG -- doesn't clip*)
(**)


SetAttributes[WithClipping,{HoldRest}];


WithClipping[
  r:FigRegionPattern,
  Body_
]:=Module[
  {Window,NewCanvasRegion,NewUserRegion},

  (* validation *)
  FigCheckInFigure[WithClipping];

  (* create window covering new canvas region, but with same transformation functions as current window *)
  NewCanvasRegion=FigResolveRegion[r];
  NewUserRegion=TransformRegion[CurrentWindow[]@InverseTFunction[],NewCanvasRegion];
  Window=FigWindow[NewUserRegion];
  Window@SetTFunction[CurrentWindow[]@GetTFunction[]];

  ShowObject[$CurrentWindow];
  Block[
    {$CurrentWindow=Window},
    ShowObject[$CurrentWindow];
    (* evaluate body *)
    (* all that wonderfull flattening/clipping/rasterization is imposed *)
    (* note: could use FigureGroup-like call*)
    FigCompositeElement[
      CollectGraphicalElements[
        Body,
        CurrentWindow[],CurrentBackground[]
      ],
      CurrentWindow[],$FigDrawingLayer
                      (*FilterRules[FigOptions,Options[FigCompositeElement]]*)
    ]
  ]

   ];
DeclareFigFallThroughError[WithClipping];


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
