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


FigLine::usage="FIGURE OBJECT: FigLine[curve] generates a curve.";
FigPolygon::usage="FIGURE OBJECT: FigPolygon[curve] generates a polygon.";
FigCircle::usage="FIGURE OBJECT: FigCircle[center] generates a circle or, more generally, an ellipse.";
FigRectangle::usage="FIGURE OBJECT: FigRectangle[center] or FigRectangle[corner1,corner2] or FigRectangle[region] generates a rectangular box.";
FigPoint::usage="FIGURE OBJECT: FigPoint[point] generates a point.";
FigAnchorMarker::usage="FIGURE OBJECT: FigAnchorMarker[point] generates an anchor marker (point plus direction) for diagnostic or illustrative purposes.";
FigBracket::usage="FIGURE OBJECT: FigBracket[side,coordinate,range] generates an annotation bracket.";
FigRule::usage="FIGURE OBJECT: FigRule[orientation,coordinate,range] ";
FigBSpline::usage="FIGURE OBJECT: FigBSpline[{pt1,pt2,...}] generates a B-spline curve with the given control points.";
FigBezier::usage="FIGURE OBJECT: FigBezier[{pt1,pt2,...}] generates a (cubic) Bezier curve with the given control points.";


(*Other usage*)


FillTexture::usage="Option name for use with figure objects.";
AngleRange::usage="Option name for use with figure objects.";
InvertAngleRange::usage="Option name for use with figure objects.";
Shift::usage="Option name for use with figure objects.";


(*Begin private context*)


Begin["`Private`"];


(*Dependencies*)





(*FigLine*)


(*Object declaration*)


DeclareFigClass[
  FigLine,
  {"Points"},
  {},
  {Center,Left,Right,Tail,Head}
];
DefineFigClassOptions[
  FigLine,
  {
    (* curve/arrowhead *)
    FigArrowheadOptions[False,False],
    FigCurveOptions  
  }
];


(*Constructor*)


Constructor[Class:FigLine,Self_Object][Curve:FigCurvePattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
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

                                                                                                ]
                                                                               ];


(*Methods*)


MakeAnchor[Class:FigLine,Self_Object][Name_,Arg:_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                         FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]
                                                    ];


MakeBoundingBox[Class:FigLine,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*FigPolygon*)


(*Object declaration*)


DeclareFigClass[
  FigPolygon,
  {"Points"},
  {},
  {Center,Left,Right}
];
DefineFigClassOptions[
  FigPolygon,
  {
    (* curve *)
    FigCurveOptions ,

    (* Mathematica polygon *)
    VertexColors->Automatic,FillTexture->None,VertexTextureCoordinates->None
  }
];


(*Constructor*)


Constructor[Class:FigPolygon,Self_Object][Curve:FigCurvePattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                   Module[
                                                                                                     {CanvasPoints},

                                                                                                     (* validate extra options *)
                                                                                                     FigCheckCurveOptions[Self];

                                                                                                     (* generate curve points *)
                                                                                                     CanvasPoints=FigResolveCurve[Self,Curve,FigOptions];
                                                                                                     (* some polar plots may make quasi-closed curve, which disrupts interpolation, leading to error messages *)
                                                                                                     (* so drop last point if is approximately same as first point *)
                                                                                                     If[
                                                                                                       Chop[First[CanvasPoints]-Last[CanvasPoints]]=={0,0},
                                                                                                       CanvasPoints=Most[CanvasPoints]
                                                                                                     ];

                                                                                                     (* make polygon *)
                                                                                                     FigPolygonElement[
                                                                                                       {
                                                                                                         If[(FillTexture/.FigOptions)=!=None,Texture[(FillTexture/.FigOptions)],{}],
                                                                                                         Polygon[CanvasPoints,FilterRules[FigOptions,Options[Polygon]]]},
                                                                                                       FigOptions
                                                                                                     ];

                                                                                                     (* save curve data *)
                                                                                                     (* saved closed set of points for anchor generation *)
                                                                                                     Self@SetPoints[Append[CanvasPoints,First[CanvasPoints]]];
                                                                                                   ]
                                                                                  ]


(*Methods*)


MakeAnchor[Class:FigPolygon,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                           FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]
                                                      ];
MakeBoundingBox[Class:FigPolygon,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*FigRectangle*)


(*Object declaration*)


DeclareFigClass[
  FigRectangle,
  {"Center","Radius","Pivot","Rotation"},
  {},
  {Center,Left,Right,Bottom,Top}
];
DefineFigClassOptions[
  FigRectangle,
  {
    (* "circle" geometry *)
    FigRectangleOptions,

    (* rounding *)
    RoundingRadius->None
  }
];


(*Constructor*)


(*shared code common to all forms of the constructor*)


BasicRectangle[Self_Object][CanvasCenter_,CanvasPivot_,CanvasRadius_,RotationAngle_,UsedRoundingRadius_]:=Module[
  {},

  (* make rectangle *)
  FigPolygonElement[
    {Rotate[
      Rectangle[(CanvasCenter-CanvasRadius),(CanvasCenter+CanvasRadius),RoundingRadius->UsedRoundingRadius],
      RotationAngle,CanvasPivot
     ]},
    FigOptions
  ];

  (* save object data *)
  Self@SetCenter[CanvasCenter];
  Self@SetRadius[CanvasRadius];
  Self@SetPivot[CanvasPivot];
  Self@SetRotation[RotationAngle];
                                                                                                          ];


(*given center (with option Radius)*)


Constructor[Class:FigRectangle,Self_Object][p:FigPointPattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                 Module[
                                                                                                   {CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedRoundingRadius},

                                                                                                   (* validate extra options *)
                                                                                                   FigCheckRectangleOptions[Self];
                                                                                                   FigCheckOption[Self,RoundingRadius,FigRadiusPattern,FigOptions];

                                                                                                   (* generate geometry *)
                                                                                                   {CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle}=MakeRectangleGeometry [p,FigOptions];
                                                                                                   UsedRoundingRadius=FigResolveRadius[(RoundingRadius/.FigOptions)];

                                                                                                   BasicRectangle[Self][CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedRoundingRadius];

                                                                                                 ]
                                                                                ];


(*given region specification*)


Constructor[Class:FigRectangle,Self_Object][r:FigRegionPattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                  Module[
                                                                                                    {CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedRoundingRadius},

                                                                                                    (* validate extra options *)
                                                                                                    FigCheckRectangleOptions[Self];

                                                                                                    (* generate geometry *)
                                                                                                    {CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle}=MakeRectangleGeometry [r,FigOptions];
                                                                                                    UsedRoundingRadius=FigResolveRadius[(RoundingRadius/.FigOptions)];

                                                                                                    BasicRectangle[Self][CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedRoundingRadius];

                                                                                                  ]
                                                                                 ];


(*given diametric corners*)


Constructor[Class:FigRectangle,Self_Object][p1:FigPointPattern,p2:FigPointPattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                                     Module[
                                                                                                                       {CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedRoundingRadius},

                                                                                                                       (* validate extra options *)
                                                                                                                       FigCheckRectangleOptions[Self];

                                                                                                                       (* generate geometry *)
                                                                                                                       {CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle}=MakeRectangleGeometry [p1,p2,FigOptions];
                                                                                                                       UsedRoundingRadius=FigResolveRadius[(RoundingRadius/.FigOptions)];

                                                                                                                       BasicRectangle[Self][CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedRoundingRadius];

                                                                                                                     ]
                                                                                                    ];


(*Methods*)


MakeAnchor[Class:FigRectangle,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                             FigRectangleAnchor[
                                                                               Self@GetCenter[],Self@GetRadius[],Self@GetPivot[],Self@GetRotation[],
                                                                               Name,Arg
                                                                             ]
                                                        ];
MakeBoundingBox[Class:FigRectangle,Self_Object][]:=FigRectangleBoundingBox[
  Self@GetCenter[],Self@GetRadius[],Self@GetPivot[],Self@GetRotation[]
                                                   ];


(*FigCircle*)


(*Object declaration*)


DeclareFigClass[
  FigCircle,
  {"Center","Radius","AngleRange","Pivot","Rotation"},
  {},
  {Center,Left,Right,Bottom,Top,Tangent,Normal,Head,Tail,HeadRadius,TailRadius}
];
DefineFigClassOptions[
  FigCircle,
  {
    (* arrowhead *)
    FigArrowheadOptions[False,False],

    (* "circle" geometry *)
    FigRectangleOptions,

    (* arc control *)
    AngleRange->None,
    InvertAngleRange->False,
    CurveClosed->False

  }
];


(*Constructor*)


(*shared code common to all forms of the constructor*)


BasicCircle[Self_Object][CanvasCenter_,CanvasPivot_,CanvasRadius_,RotationAngle_]:=Module[
  {UsedAngleRange,ArrowheadCurve},

  (* how Mathematica Circle/Disk apparently decides to draw the arc the "long way" or "short way" -- Mathematica ignores given order of angles -- apparently sorts angles into ascending numerical order, *then* draws counterclockwise arc *)
  (*UsedAngleRange=SortBy[AngleRange,N];*) (* put angles in counterclockwise order; SortBy so sorted as numbers not expressions *)
  UsedAngleRange=Replace[
    (AngleRange/.FigOptions),
    None->{0,2*Pi}
                 ];
  UsedAngleRange=N[Replace[
    UsedAngleRange,
    pr:FigPointPattern:>CanvasRayAngle[{CanvasCenter,FigResolvePoint[pr]}],
    {1}]];
  If[
    (InvertAngleRange/.FigOptions),
    (* add 2*Pi to lesser angle *)
    UsedAngleRange=UsedAngleRange+Switch[
      Sign[Last[UsedAngleRange]-First[UsedAngleRange]],
      +1,{2*Pi,0},
      -1|0,{0,2*Pi}
                   ]
  ];

  (* save object data *)
  Self@SetCenter[CanvasCenter];
  Self@SetRadius[CanvasRadius];
  Self@SetAngleRange[UsedAngleRange];
  Self@SetPivot[CanvasPivot];
  Self@SetRotation[RotationAngle];

  (* make circle *)
  (* use Disk edge for outline only if CurveClosed\[Rule]True *)
  FigPolygonElement[
    {Rotate[Disk[CanvasCenter,CanvasRadius,UsedAngleRange],RotationAngle,CanvasPivot]},
    Flatten[{
      If[!(CurveClosed/.FigOptions),{ShowLine->False},{}],
      FigOptions
      }]
  ];
  (* use Circle for outline only if CurveClosed\[Rule]False *)
  FigLineElement[
    {Rotate[Circle[CanvasCenter,CanvasRadius,UsedAngleRange],RotationAngle,CanvasPivot]},
    Flatten[{
      If[(CurveClosed/.FigOptions),{Show->False},{}],
      FigOptions
      }]
  ];

  (* make arrowhead lines *)
  FigLineElement[
    {Line[FigCurveArrowheadPoints[
      Self@MakeAnchor[Tail,None],
      Self@MakeAnchor[Head,None],
      FigOptions
          ]]},
    Flatten[{LineDashing->None,FigOptions}]
  ];


                                                                                   ];


(*given center (with option Radius)*)


Constructor[Class:FigCircle,Self_Object][p:FigPointPattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                              Module[
                                                                                                {CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedRoundingRadius},

                                                                                                (* validate extra options *)
                                                                                                FigCheckRectangleOptions[Self];
                                                                                                FigCheckArrowheadOptions[Self];
                                                                                                FigCheckOption[Self,AngleRange,None|{(_?NumericQ)|FigPointPattern,(_?NumericQ)|FigPointPattern},FigOptions];
                                                                                                FigCheckOption[Self,InvertAngleRange,LogicalPattern,FigOptions];
                                                                                                FigCheckOption[Self,CurveClosed,LogicalPattern,FigOptions];

                                                                                                (* generate geometry *)
                                                                                                {CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle}=MakeRectangleGeometry [p,FigOptions];

                                                                                                BasicCircle[Self][CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle];

                                                                                              ]
                                                                             ];


(*given region specification*)


Constructor[Class:FigCircle,Self_Object][r:FigRegionPattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                               Module[
                                                                                                 {CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedRoundingRadius},

                                                                                                 (* validate extra options *)
                                                                                                 FigCheckRectangleOptions[Self];
                                                                                                 FigCheckArrowheadOptions[Self];
                                                                                                 FigCheckOption[Self,AngleRange,None|{(_?NumericQ)|FigPointPattern,(_?NumericQ)|FigPointPattern},FigOptions];
                                                                                                 FigCheckOption[Self,InvertAngleRange,LogicalPattern,FigOptions];
                                                                                                 FigCheckOption[Self,CurveClosed,LogicalPattern,FigOptions];

                                                                                                 (* generate geometry *)
                                                                                                 {CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle}=MakeRectangleGeometry [r,FigOptions];

                                                                                                 BasicCircle[Self][CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle];

                                                                                               ]
                                                                              ];


(*Methods*)


MakeAnchor[Class:FigCircle,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                          FigCircleAnchor[
                                                                            Self@GetCenter[],Self@GetRadius[],Self@GetAngleRange[],Self@GetPivot[],Self@GetRotation[],
                                                                            Name,Arg
                                                                          ]
                                                     ];
MakeBoundingBox[Class:FigCircle,Self_Object][]:=FigCircleBoundingBox[
  Self@GetCenter[],Self@GetRadius[],Self@GetAngleRange[],Self@GetPivot[],Self@GetRotation[]
                                                ];


(*FigPoint*)


(*Object declaration*)


DeclareFigClass[
  FigPoint,
  {"Point","PointRadius"},
  {},
  {Center,Left,Right,Bottom,Top}
];
DefineFigClassOptions[
  FigPoint,
  {
  }
];


(*Constructor*)


Constructor[Class:FigPoint,Self_Object][p:FigPointPattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                             Module[
                                                                                               {CanvasPoint,d},

                                                                                               (* generate point *)
                                                                                               CanvasPoint=FigResolvePoint[p];

                                                                                               (* make point *)
                                                                                               FigPointElement[
                                                                                                 {Point[CanvasPoint]},
                                                                                                 FigOptions
                                                                                               ];

                                                                                               (* save point data *)
                                                                                               Self@SetPoint[CanvasPoint];
                                                                                               d=First[FigResolvePointSize[(PointSize/.FigOptions)]];  (* may not be numeric if size directive such as Large used for PointSize *)
                                                                                               Self@SetPointRadius[If[NumericQ[d],d/2,0]];

                                                                                             ]
                                                                            ];


(*Methods*)


MakeAnchor[Class:FigPoint,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                         FigCircleAnchor[Self@GetPoint[],Self@GetPointRadius[]*{1,1},{0,2*Pi},{0,0},0,Name,Arg]
                                                    ];
MakeBoundingBox[Class:FigPoint,Self_Object][]:=FigPointBoundingBox[Self@GetPoint[]];


(*FigAnchorMarker*)


(*Object declaration*)


DeclareFigClass[
  FigAnchorMarker,
  {"TheAnchor"},
  {},
  {}
];
DefineFigClassOptions[
  FigAnchorMarker,
  {PointSize->5,LineColor->Red,TextFrame->True,TextBackground->LightGray,Layer->4}, (* inheritance overrides *)
  {
    Length->12,Text->None
  }
];


(*Constructor*)


Constructor[Class:FigAnchorMarker,Self_Object][p:FigPointPattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                    Module[
                                                                                                      {Anchor,CanvasPoint,d,TextArt},

                                                                                                      (* validate options *)
                                                                                                      FigCheckOption[Self,Length,_?NumericQ,FigOptions];

                                                                                                      (* generate point *)
                                                                                                      Anchor=FigAnchor[p];
                                                                                                      CanvasPoint=Anchor@GetPoint[];

                                                                                                      (* make text *)
                                                                                                      (* draw text first so that -- with a layer override putting all elements in the same layer -- text box won't hide point and line *)
                                                                                                      TextArt=(Text/.FigOptions);
                                                                                                      FigTextElement[
                                                                                                        Anchor,
                                                                                                        TextArt,
                                                                                                        FigOptions
                                                                                                      ];

                                                                                                      (* make line *)
                                                                                                      FigLineElement[
                                                                                                        {Line[{CanvasPoint,CanvasPoint+FromPolar[{(Length/.FigOptions),Anchor@GetAngle[]}]}]},
                                                                                                        FigOptions
                                                                                                      ];

                                                                                                      (* make point *)
                                                                                                      FigPointElement[
                                                                                                        {Point[CanvasPoint]},
                                                                                                        FigOptions
                                                                                                      ];


                                                                                                      (* save point data *)
                                                                                                      Self@SetTheAnchor[Anchor];

                                                                                                    ]
                                                                                   ];


(*Methods*)


MakeAnchor[Class:FigAnchorMarker,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,Null];
MakeBoundingBox[Class:FigAnchorMarker,Self_Object][]:=FigPointBoundingBox[(Self@GetTheAnchor[])@GetPoint[]];


(*FigBracket*)


(*Object declaration*)


DeclareFigClass[
  FigBracket,
  {
    "Points", (* canvas endpoints *)
    "Side",  (* name of bracket orientation *)
    "LeftIsOutside"  (* whether or not curve "left" is outside (label side) of bracket *)
  },
  {},
  {Center,Left,Right,Tail,Head,"Bracket"}
];
DefineFigClassOptions[
  FigBracket,
  {
    (* curve/arrowhead *)
    FigArrowheadOptions[True,0,{3,0},End],

    (*FigCurveOptions  *)

    (* geometry *)
    Shift->0
  }
];


(*Constructor*)


Constructor[Class:FigBracket,Self_Object][
  Side:(Left|Right|Bottom|Top)|(Horizontal|Vertical),
  Coordinate:FigCoordinatePattern,
  Range:{FigCoordinatePattern,FigCoordinatePattern}|FigRegionPattern,
  Opts___?OptionQ
                                         ]:=FigObjectWrapper[Class,Self,{Opts},
                                                             Module[
                                                               {CoordinateIndex,CanvasCoordinate,CanvasCoordinateShiftSense,CanvasCoordinateShift,CanvasInterval,CanvasPoints,LeftIsOutside},

                                                               (* common error: chastize user for accidentally giving side as Horizontal or Vertical *)
                                                               (* better than generic syntax error *)
                                                               FigCheckValue[Self,Side,Except[Horizontal|Vertical],"side"];

                                                               (* validate extra options *)
                                                               (* validate options *)
                                                               FigCheckOption[Self,Shift,FigCoordinatePattern,FigOptions];
                                                               FigCheckArrowheadOptions[Self,EndLength];

                                                               (*FigCheckCurveOptions[Self];*)

                                                               (* determine horizontal/vertical direction *)
                                                               CoordinateIndex=Switch[
                                                                 Side,
                                                                 Left|Right,1,
                                                                 Bottom|Top,2
                                                                               ];

                                                               (* convert coordinate and interval to canvas values *)
                                                               CanvasCoordinate=FigResolveCoordinate[Coordinate,CoordinateIndex];
                                                               CanvasCoordinateShift=FigResolveCoordinateDisplacement[(Shift/.FigOptions),CoordinateIndex];
                                                               CanvasCoordinateShiftSense=Switch[
                                                                 Side,
                                                                 Left|Bottom,-1,
                                                                 Right|Top,+1
                                                                                          ];
                                                               CanvasCoordinate+=CanvasCoordinateShiftSense*CanvasCoordinateShift;
                                                               CanvasInterval=Sort@Replace[
                                                                 Range,
                                                                 {
                                                                   {x1:FigCoordinatePattern,x2:FigCoordinatePattern}:>{
                                                                     FigResolveCoordinate[x1,AntiCoordinateIndex[CoordinateIndex]],
                                                                     FigResolveCoordinate[x2,AntiCoordinateIndex[CoordinateIndex]]
                                                                                                                    },
                                                                   r:FigRegionPattern:>FigResolveRegion[r][[AntiCoordinateIndex[CoordinateIndex]]]
                                                                 }
                                                                                   ];


                                                               (* generate curve points *)
                                                               (* curve always goes in "positive" coordinate direction *)
                                                               CanvasPoints=Switch[
                                                                 Side,
                                                                 Left|Right,{{CanvasCoordinate,CanvasInterval[[1]]},{CanvasCoordinate,CanvasInterval[[2]]}},
                                                                 Bottom|Top,{{CanvasInterval[[1]],CanvasCoordinate},{CanvasInterval[[2]],CanvasCoordinate}}
                                                                            ];

                                                               (* save curve data *)
                                                               Self@SetPoints[CanvasPoints];
                                                               Self@SetSide[Side];

                                                               (* make curve line *)
                                                               FigLineElement[
                                                                 {Line[CanvasPoints]},
                                                                 FigOptions
                                                               ];

                                                               (* make arrowhead lines *)
                                                               (* need to flip head lip specification on Left/Top brackets so "left" entry in head width specification becomes "inside" width *)
                                                               LeftIsOutside=Switch[
                                                                 Side,
                                                                 Left|Top,True,
                                                                 Bottom|Right,False
                                                                             ];
                                                               Self@SetLeftIsOutside[LeftIsOutside];
                                                               FigLineElement[
                                                                 {Line[
                                                                   FigCurveArrowheadPoints[
                                                                     Self@MakeAnchor[Tail,None],
                                                                     Self@MakeAnchor[Head,None],
                                                                     LeftIsOutside,
                                                                     FigOptions,End
                                                                   ]
                                                                  ]},
                                                                 Flatten[{LineDashing->None,FigOptions}]
                                                               ];

                                                             ]
                                            ]


Constructor[Class:FigBracket,Self_Object][
  Side:Above|Below,
  Range:{r1:FigPointPattern,r2:FigPointPattern},
  Opts___?OptionQ
                                         ]:=FigObjectWrapper[Class,Self,{Opts},
                                                             Module[
                                                               {CoordinateIndex,CanvasCoordinate,CanvasInterval,CanvasPoints,RightwardGoing,LeftIsOutside},

                                                               (* validate extra options *)
                                                               FigCheckArrowheadOptions[Self,EndLength];



                                                               (* generate curve points *)
                                                               CanvasPoints=FigResolvePoint/@Range;

                                                               (* save curve data *)
                                                               Self@SetPoints[CanvasPoints];
                                                               Self@SetSide[Side];

                                                               (* make curve line *)
                                                               FigLineElement[
                                                                 {Line[CanvasPoints]},
                                                                 FigOptions
                                                               ];

                                                               (* make arrowhead lines *)
                                                               (* need to flip head lip specification so "left" entry in head width specification becomes "inside" width *)
                                                               (* for a pair of endpoints with positive x displacement (i.e., in NE or SE quadrants), "above" is curve's "left", so "outside" is left, and "inside" is right *)
                                                               RightwardGoing=NonNegative[-First[Subtract@@CanvasPoints]];
                                                               LeftIsOutside=Switch[
                                                                 Side,
                                                                 Above,RightwardGoing,
                                                                 Below,Not[RightwardGoing]
                                                                             ];
                                                               Self@SetLeftIsOutside[LeftIsOutside];
                                                               FigLineElement[
                                                                 {Line[
                                                                   FigCurveArrowheadPoints[
                                                                     Self@MakeAnchor[Tail,None],
                                                                     Self@MakeAnchor[Head,None],
                                                                     LeftIsOutside,
                                                                     FigOptions,End
                                                                   ]
                                                                  ]},
                                                                 Flatten[{LineDashing->None,FigOptions}]
                                                               ];

                                                             ]
                                            ]


(*Methods*)


MakeAnchor[Class:FigBracket,Self_Object][Name:"Bracket",Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                    FigCurveAnchorFromPoints[
                                                                                      Self@GetPoints[],
                                                                                      If[Self@GetLeftIsOutside[],Left,Right],
                                                                                      Arg
                                                                                    ]
                                                               ]; 


(*define standard curve anchors (needed, e.g., for arrowheads)*)


MakeAnchor[Class:FigBracket,Self_Object][Name:Except["Bracket"],Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                            FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]
                                                                       ];


(*define standard bounding box*)


MakeBoundingBox[Class:FigBracket,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*FigRule*)


(*similar to FigAxis with following differences:*)
(*	arrowheads default ShowHead->False*)
(*	convenience labels like Lev*)
(*	might ultimately want to define Bottom and Top anchors*)
(*	range should be specified *numerically* (though positioning coordinate can still be specified in generic fashion)*)


(*Object declaration*)


DeclareFigClass[
  FigRule,
  {"Points","Orientation"},
  {},
  {Center,Left,Right,Tail,Head}
];
DefineFigClassOptions[
  FigRule,
  {
    (* curve/arrowhead *)
    FigArrowheadOptions[False,False]
  }
];


(*Constructor*)


Constructor[Class:FigRule,Self_Object][
  Orientation:(Horizontal|Vertical),
  Coordinate:FigCoordinatePattern,
  Range:{FigCoordinatePattern,FigCoordinatePattern}|FigRegionPattern,
  Opts___?OptionQ
                                      ]:=FigObjectWrapper[Class,Self,{Opts},
                                                          Module[
                                                            {
                                                              CoordinateIndex,CanvasCoordinate,CanvasInterval,CanvasPoints,CanvasTail,CanvasHead
                                                            },

                                                            (* validate extra options *)
                                                            FigCheckArrowheadOptions[Self];

                                                            (* determine horizontal/vertical direction *)
                                                            (* CoordinateIndex -- coordinate for *positioning* axis *)
                                                            CoordinateIndex=Switch[
                                                              Orientation,
                                                              Vertical,1,
                                                              Horizontal,2
                                                                            ];

                                                            (* convert coordinate and interval to canvas values *)
                                                            CanvasCoordinate=FigResolveCoordinate[Coordinate,CoordinateIndex];
                                                            CanvasInterval=Sort@Replace[
                                                              Range,
                                                              {
                                                                {x1:FigCoordinatePattern,x2:FigCoordinatePattern}:>{
                                                                  FigResolveCoordinate[x1,AntiCoordinateIndex[CoordinateIndex]],
                                                                  FigResolveCoordinate[x2,AntiCoordinateIndex[CoordinateIndex]]
                                                                                                                 },
                                                                r:FigRegionPattern:>FigResolveRegion[r][[AntiCoordinateIndex[CoordinateIndex]]]
                                                              }
                                                                                ];

                                                            (* generate curve points *)
                                                            (* curve always goes in "positive" coordinate direction *)
                                                            CanvasPoints=Switch[
                                                              Orientation,
                                                              Vertical,{{CanvasCoordinate,CanvasInterval[[1]]},{CanvasCoordinate,CanvasInterval[[2]]}},
                                                              Horizontal,{{CanvasInterval[[1]],CanvasCoordinate},{CanvasInterval[[2]],CanvasCoordinate}}
                                                                         ];

                                                            (* save curve data *)
                                                            Self@SetPoints[CanvasPoints];
                                                            Self@SetOrientation[Orientation];

                                                            (* make curve line *)
                                                            FigLineElement[
                                                              {Line[CanvasPoints]},
                                                              FigOptions
                                                            ];

                                                            (* make arrowhead lines *)
                                                            FigLineElement[
                                                              {Line[
                                                                FigCurveArrowheadPoints[
                                                                  Self@MakeAnchor[Tail,None],
                                                                  Self@MakeAnchor[Head,None],
                                                                  Flatten[{
                                                                    FigOptions
                                                                    }]
                                                                ]
                                                               ]},
                                                              Flatten[{Dashing->None,FigOptions}]
                                                            ];


                                                          ]
                                         ];


(*Methods*)


(*define standard curve anchors (needed, e.g., for arrowheads)*)


MakeAnchor[Class:FigRule,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                        FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]
                                                   ];


(*define standard bounding box*)


MakeBoundingBox[Class:FigRule,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*FigBSpline*)


(*Object declaration*)


DeclareFigClass[
  FigBSpline,
  {"Points","InterpolationFunction","TangentFunction"},
  {},
  {Center,Left,Right,Tail,Head}
];


DefineFigClassOptions[
  FigBSpline,
  {
    (* curve/arrowhead *)
    FigArrowheadOptions[False,False],
    FigCurveOptions,

    (* spline-specific options *)
    SplineClosed->False,SplineDegree->Automatic,SplineKnots->Automatic,SplineWeights->Automatic

  }
];


(*Constructor*)


Constructor[Class:FigBSpline,Self_Object][Curve:FigCurvePattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                   Module[
                                                                                                     {CanvasPoints},

                                                                                                     (* validate extra options *)
                                                                                                     FigCheckArrowheadOptions[Self];
                                                                                                     FigCheckCurveOptions[Self];
                                                                                                     FigCheckOption[Self,SplineClosed,LogicalPattern,FigOptions];
                                                                                                     FigCheckOption[Self,SplineDegree,Automatic|((_Integer)?Positive),FigOptions];
                                                                                                     FigCheckOption[Self,SplineKnots,(Automatic|{((_Integer)?Positive)..}|"Clamped"|"Unclamped"),FigOptions];
                                                                                                     FigCheckOption[Self,SplineWeights,Automatic|(_List),FigOptions];

                                                                                                     (* prerequisite calculations *)
                                                                                                     CanvasPoints=FigResolveCurve[Self,Curve,FigOptions];

                                                                                                     (* save data needed for anchor generation *)
                                                                                                     (* CAVEAT -- these interpolation functions are not linear in the canvas metric *)
                                                                                                     Self@SetPoints[CanvasPoints];
                                                                                                     Self@SetInterpolationFunction[BSplineFunction[CanvasPoints,FilterRules[FigOptions,Options[BSplineCurve]]]];
                                                                                                     Self@SetTangentFunction[Derivative[1][BSplineFunction[CanvasPoints,FilterRules[FigOptions,Options[BSplineCurve]]]]];

                                                                                                     (* make graphics elements *)
                                                                                                     (* curve *)
                                                                                                     FigLineElement[
                                                                                                       {BSplineCurve[CanvasPoints,FilterRules[FigOptions,Options[BSplineCurve]]]},
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

                                                                                                   ]
                                                                                  ];


(*Methods*)


(*standard curve anchors*)


MakeAnchor[Class:FigBSpline,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                           FigCurveAnchor[Self@GetPoints[],{Self@GetInterpolationFunction[],Self@GetTangentFunction[]},Name,Arg]
                                                      ];


(*bounding box *)
(*Note: For a spline, we will just crudely approximate the bounding box by the bounding box of the control points.  With much more computational effort (e.g., numerical extremization of the coordinates with respect to the curve parameter) a tighter bounding box could be obtained, but the calculation would also likely not be completely robust for exotic spline curves.*)


MakeBoundingBox[Class:FigBSpline,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*FigBezier*)


(*Object declaration*)


DeclareFigClass[
  FigBezier,
  {"Points","InterpolationFunction","TangentFunction"},
  {},
  {Center,Left,Right,Tail,Head}
];


DefineFigClassOptions[
  FigBezier,
  {
    (* curve/arrowhead *)
    FigArrowheadOptions[False,False],
    FigCurveOptions,

    (* spline-specific options *)
    SplineDegree->Automatic 

  }
];


(*Constructor*)


Constructor[Class:FigBezier,Self_Object][Curve:FigCurvePattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                  Module[
                                                                                                    {CanvasPoints},

                                                                                                    (* validate extra options *)
                                                                                                    FigCheckArrowheadOptions[Self];
                                                                                                    FigCheckCurveOptions[Self];
                                                                                                    (*FigCheckOption[Self,SplineDegree,Automatic|((_Integer)?Positive),FigOptions];*)
                                                                                                    FigCheckOption[Self,SplineDegree,Automatic|3,FigOptions];

                                                                                                    (* prerequisite calculations *)
                                                                                                    CanvasPoints=FigResolveCurve[Self,Curve,FigOptions];

                                                                                                    (* save data needed for anchor generation *)
                                                                                                    Self@SetPoints[CanvasPoints];
                                                                                                    Self@SetInterpolationFunction[BezierFunction[CanvasPoints,FilterRules[FigOptions,Options[BezierCurve]]]];
                                                                                                    Self@SetTangentFunction[Derivative[1][BezierFunction[CanvasPoints,FilterRules[FigOptions,Options[BezierCurve]]]]];

                                                                                                    (* make graphics elements *)
                                                                                                    (* curve *)
                                                                                                    FigLineElement[
                                                                                                      {BezierCurve[CanvasPoints,FilterRules[FigOptions,Options[BezierCurve]]]},
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

                                                                                                  ]
                                                                                 ];


(*Methods*)


(*standard curve anchors*)


MakeAnchor[Class:FigBezier,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                          FigCurveAnchor[Self@GetPoints[],{Self@GetInterpolationFunction[],Self@GetTangentFunction[]},Name,Arg]
                                                     ];


(*bounding box *)
(*Note: For a spline, we will just crudely approximate the bounding box by the bounding box of the control points.  With much more computational effort (e.g., numerical extremization of the coordinates with respect to the curve parameter) a tighter bounding box could be obtained, but the calculation would also likely not be completely robust for exotic spline curves.*)


MakeBoundingBox[Class:FigBezier,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
