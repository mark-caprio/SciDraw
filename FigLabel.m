(* ::Package:: *)

(*Header comments*)


(* :Title: FigLabel *)
(* :Context: SciDraw` *)
(* :Summary: Label object *)
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


FigLabel::usage="FIGURE OBJECT: FigLabel[point,text] generates a label at the given point.  If point is given as an anchor object, the default text offset and orientation information is taken from the anchor.  FigLabel[object,name,text] or FigLabel[object,name,arg,text] attaches the label to an anchor extracted from the given object.  FigLabel[text] takes its position from the option Position.";


(*Other usage*)


Displacement::usage="Option name for use with figure objects.";
DisplacedPoint::usage="Option name for use with figure objects.";
IntermediatePoints::usage="Option name for use with figure objects.";


(*Begin private context*)


Begin["`Private`"];


(*Dependencies*)





(*FigLabel*)


(*Object declaration*)


DeclareFigClass[
  FigLabel,
  {"TheAnchor","TextElement","Points"},
  {},
  {}
];
DefineFigClassOptions[
  FigLabel,
  {ShowLine->False},  (* callout is off by default *)
  {
    (* given position *)
    Point->{0,0},

    (* anchor position *)
    Position->Automatic,

    (* label displacement *)
    DisplacedPoint->None,
    Displacement->None,

    (* callout *)
    IntermediatePoints->None,

    (* curve/arrowhead *)
    FigArrowheadOptions[False,True],
    FigCurveOptions
  }
];


(*Constructor*)


(*Note: Because of the optional first argument, syntactic ambiguity occurs in the very unlikely scenario in which the text art expression also matches the criteria for an option list, e.g., is a rule expression, a list of rule expressions, or (more plausibly) the null list.*)


BasicLabel[Self_Object][GivenAnchor_Object,TextArt_]:=Module[
  {
    RelativeAnchor,Anchor,UsedIntermediatePoints,Curve,CanvasPoints,TextElement,GivenDisplacedPoint
  },

  (* validate options *)
  FigCheckOption[Self,DisplacedPoint,None|FigPointPattern,FigOptions];
  FigCheckOption[Self,Displacement,FigDisplacementPattern,FigOptions];
  FigCheckOption[Self,IntermediatePoints,None|{FigCurvePointPattern...},FigOptions];
  FigCheckArrowheadOptions[Self];
  FigCheckCurveOptions[Self];

  (* generate anchor point for text *)
  RelativeAnchor=DisplacePoint[FigAnchor[GivenAnchor],(Displacement/.FigOptions)];
  GivenDisplacedPoint=ResolveOption[DisplacedPoint,{None->RelativeAnchor},FigOptions];
  Anchor=FigAnchor[GivenDisplacedPoint];

  (* construct callout curve specification *)
  UsedIntermediatePoints=ResolveOption[IntermediatePoints,{None->{}},FigOptions];
  Curve=Join[{Anchor},UsedIntermediatePoints,{GivenAnchor}];
  CanvasPoints=FigResolveCurve[Self,Curve,FigOptions];

  (* save data needed for anchor generation on callout curve *)
  Self@SetPoints[CanvasPoints];

  (* make graphics elements *)
  (* short circuit if no displaced point -- else have divide-by-zero errors on arrowheads *)
  If[((Displacement/.FigOptions)=!=None)||((DisplacedPoint/.FigOptions)=!=None),
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
     ];
  ];

  (* make text *)
  TextElement=FigTextElement[
    Anchor,
    TextArt,
    FigOptions
              ];

  (* save references to spawned objects *)
  Self@SetTheAnchor[Anchor];
  Self@SetTextElement[TextElement];

                                                      ];


Constructor[Class:FigLabel,Self_Object][TextArt_,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},

                                                                                    (* validate special option *)
                                                                                    FigCheckOption[Self,Point,FigPointPattern,FigOptions];

                                                                                    BasicLabel[Self][FigAnchor[(Point/.FigOptions)],TextArt]

                                                                   ];


Constructor[Class:FigLabel,Self_Object][p:FigPointPattern,TextArt_,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                      BasicLabel[Self][FigAnchor[p],TextArt]
                                                                                     ];


(*With argument pattern*)
(*	FigLabel[n,Name,Arg:_:None,Text_,Opts___]*)
(*unfortunately*)
(*	FigLabel[obj,name,arg,text,opt->value]*)
(*matches n=obj, Name=name, Arg=text, Text=(opt->value), Opts={}...*)
(*Overcome by forbidding Text from matching OptionQ, at cost of possibly misinterpreting text in the form of a rule or list rules, including the null list.*)
(*Note that use of Longest on Opts empirically fails to accomplish goal.*)


Constructor[Class:FigLabel,Self_Object][(n:ObjectNamePattern[FigObject]),Name_,AnchorArg:_:None,TextArt:Except[_?OptionQ],Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                                                                             (*Print[n," ",Name," ",TextArt," ",{Opts}];*)
                                                                                                                                                             BasicLabel[Self][
                                                                                                                                                               FigAnchor[n,Name,ResolveOption[Position,{Automatic->AnchorArg},FigOptions]],
                                                                                                                                                               TextArt
                                                                                                                                                                             ]
                                                                                                                                            ];


(*Methods*)


(* rectangle-style anchor points, deduced from text element *)
MakeAnchor[Class:FigLabel,Self_Object][Name:Except[(Head|Tail)],Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                            FigRectangleAnchor[
                                                                                              (Self@GetTextElement[])@GetCenter[],(Self@GetTextElement[])@GetRadius[],(Self@GetTextElement[])@GetPivot[],(Self@GetTextElement[])@GetRotation[],
                                                                                              Name,Arg
                                                                                            ]
                                                                       ];


(* curve-style anchor points, from callout curve -- needed for arrowhead generation -- only define Head/Tail*)
MakeAnchor[Class:FigLabel,Self_Object][Name:(Head|Tail),Arg:_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                     FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]
                                                                ];


MakeBoundingBox[Class:FigLabel,Self_Object][]:=FigRectangleBoundingBox[
  (Self@GetTextElement[])@GetCenter[],(Self@GetTextElement[])@GetRadius[],(Self@GetTextElement[])@GetPivot[],(Self@GetTextElement[])@GetRotation[]
                                               ];


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
