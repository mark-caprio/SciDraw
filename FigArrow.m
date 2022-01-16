(* ::Package:: *)

(*Header comments*)


(* :Title: FigArrow *)
(* :Context: SciDraw` *)
(* :Summary: Arrow shape *)
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


FigArrow::usage="FIGURE OBJECT: FigArrow[curve] generates an arrow.";


(*Other usage*)


(*Registration functions*)


$ArrowTypeRegistry::usage="Global registry of curve shapes for use with DataPlot.";
DefineArrowType::usage="DefineArrowType[name,function] defines a new value to be accepted for the ArrowType option.  The function should take a list of two or more {x,y} points (as canvas coordinates) and return a new list of points.";


(*Utilities*)


DisplaceCurve::usage="DisplaceCurve[points,d,joinform] displaces the curve by a distance d (positive normal is to right).  The joinform may be \"Bevel\" or \"Miter\".  Note: If the curve is beveled, obtuse angle joints are truncated, so acute joints get a single point but obtuse joints get two points. This changes the number of segments in the curve, which can be important if anchors are later generated from the curve by segment number.";
FigCheckArrowOptions::usage="FigCheckArrowOptions[obj] validates the corresponding set of options.";


(*Options*)


ArrowType::usage="Option name for use with figure objects.";
ArrowJoinForm::usage="Option name for use with figure objects.";
HeadFlush::usage="Option name for use with figure objects.";
TailFlush::usage="Option name for use with figure objects.";


(*Special arrow type options*)


SquiggleBuffer::usage="Option name for use with figure objects.";
SquiggleWavelength::usage="Option name for use with figure objects.";
SquiggleSide::usage="Option name for use with figure objects.";


(*Begin private context*)


Begin["`Private`"];


(*Dependencies*)





(*Utilities*)


(*Curve displacement*)


(*Zero displacement handled specially to avoid unnecessary doubling of joint points.*)


DisplaceCurve[Points_List,d:0,JoinForm:("Bevel"|"Miter")]:=Points;
DisplaceCurve[Points_List,Except[0,d_?NumericQ],JoinForm:("Bevel"|"Miter")]:=Module[
  {NumSegments,JointList,s1,s2,u1,u2,n1,n2,alpha,beta,p,p1,p2,pp},

  NumSegments=Length[Points]-1;
  JointList=Table[
    (* segments *)
    s1=CurveSegment[Points,Joint];
    s2=CurveSegment[Points,Joint+1];
    (* tangents *)
    u1=SegmentTangent[s1];
    u2=SegmentTangent[s2];
    (* normals *)
    n1=-Cross[u1];
    n2=-Cross[u2];
    (* displaced points *)
    p=Points[[Joint+1]];
    p1=p+d*n1;
    p2=p+d*n2;
    (* intersection *)
    {alpha,beta}=LinearSolve[Transpose[{u1,-u2}],-d*(n1-n2)];
    pp=p+d*n1+alpha*u1;
    (* choice of beveling *)
    If[
      (alpha<0)||(JoinForm=="Miter"),
      (* acute *)
      {pp},
      (* obtuse *)
      {p1,p2}
    ],
    {Joint,1,NumSegments-1}
            ];

  (* start and end points *)
  (* segments *)
  s1=CurveSegment[Points,1];
  s2=CurveSegment[Points,-1];
  (* tangents *)
  u1=SegmentTangent[s1];
  u2=SegmentTangent[s2];
  (* normals *)
  n1=-Cross[u1];
  n2=-Cross[u2];
  (* displaced points *)
  p1=Points[[1]]+d*n1;
  p2=Points[[-1]]+d*n2;

  (* assemble list of points *)
  Join[{p1},Flatten[JointList,1],{p2}]
                                                                             ];


(*FigArrow*)


(*Object declaration*)


(*This code must *precede* any calls to the accessor/mutator methods, e.g., in the bodies of the function definitions within DefineArrowType.*)


DeclareFigClass[
  FigArrow,
  {"Points","LeftPoints","RightPoints"},
  {},
  {Center,Left,Right,Tail,Head}
];
DefineFigClassOptions[
  FigArrow,
  {
    (* curve/arrowhead *)
    FigArrowheadOptions[False,True],
    FigCurveOptions ,

    (* shape *)
    ArrowType->"Line",
    Width->5,
    ArrowJoinForm->"Miter",
    TailFlush->False,HeadFlush->False,

    (* squiggle -- applicable to general repeating curves *)
    SquiggleBuffer->2,
    SquiggleWavelength->10,
    SquiggleSide->Right,
    PlotPoints->32

  }
];


(*Constructor*)


(*REMINDER: Any edits to the methods of FigArrow should probably be propagated to the definition of Trans.*)


(*shared code common to all forms of the constructor -- including daughter Trans types*)


BasicArrow[Self_Object][Curve:FigCurvePattern]:=Module[
  {CanvasPoints,LeftCanvasPoints,RightCanvasPoints,UsedWidth,a1,a2},

  (* validate extra options *)
  FigCheckArrowheadOptions[Self];
  FigCheckCurveOptions[Self];
  FigCheckOption[Self,Width,_?NonNegative,FigOptions];
  FigCheckOption[Self,HeadFlush,LogicalPattern,FigOptions];
  FigCheckOption[Self,TailFlush,LogicalPattern,FigOptions];
  FigCheckOption[Self,ArrowType,Alternatives@@$ArrowTypeRegistry,FigOptions];
  FigCheckOption[Self,ArrowJoinForm,"Bevel"|"Miter",FigOptions];

  (* prerequisite calculations *)
  (* Note: for line-like arrows, arrow type definition should override these left and right curves *)
  UsedWidth=(Width/.FigOptions);
  CanvasPoints=FigResolveCurve[Self,Curve,FigOptions];
  If[(Debug/.FigOptions),Print["Canvas points: ",CanvasPoints]];
  LeftCanvasPoints=DisplaceCurve[CanvasPoints,-UsedWidth/2,(ArrowJoinForm/.FigOptions)];
  RightCanvasPoints=DisplaceCurve[CanvasPoints,+UsedWidth/2,(ArrowJoinForm/.FigOptions)];

  (* recover full anchors for endpoints *)
  (* but can only do if curve was specified as list of points (not graphics) and if relevant endpoint is a self-sufficient point, not a specification relative to the head or tail *)
  a1=If[
    MatchQ[Curve,FigCurvePointSetPattern]&&MatchQ[First[Curve],FigPointPattern],
    FigAnchor[First[Curve]],
    FigAnchor[Canvas[First[CanvasPoints]]]
     ];
  a2=If[
    MatchQ[Curve,FigCurvePointSetPattern]&&MatchQ[Last[Curve],FigPointPattern],
    FigAnchor[Last[Curve]],
    FigAnchor[Canvas[Last[CanvasPoints]]]
     ];

  (* save data needed for anchor generation *)
  Self@SetPoints[CanvasPoints];
  Self@SetLeftPoints[LeftCanvasPoints];
  Self@SetRightPoints[RightCanvasPoints];

  (* make graphics elements *)
  ArrowTypeFunction[(ArrowType/.FigOptions)][Self,a1,a2,CanvasPoints,UsedWidth,FigOptions];
                                                ];


Constructor[Class:FigArrow,Self_Object][Curve:FigCurvePattern,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                 BasicArrow[Self][Curve]
                                                                                ];


(*Methods*)


(*REMINDER: Any edits to the methods of FigArrow should probably be propagated to the definition of Trans.*)


MakeAnchor[Class:FigArrow,Self_Object][Name:(Head|Tail|Center),Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                           FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]
                                                                      ];
MakeAnchor[Class:FigArrow,Self_Object][Name:Left,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                             FigCurveAnchorFromPoints[Self@GetLeftPoints[],Name,Arg]
                                                        ];
MakeAnchor[Class:FigArrow,Self_Object][Name:Right,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                              FigCurveAnchorFromPoints[Self@GetRightPoints[],Name,Arg]
                                                         ];


MakeBoundingBox[Class:FigArrow,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*Arrow types*)


(*Arrow type registration*)


$ArrowTypeRegistry={};


SetAttributes[DefineArrowType,{HoldRest}];


DefineArrowType[Name_,Function_]:=Module[
  {},
  AppendTo[$ArrowTypeRegistry,Name];
  ArrowTypeFunction[Name]:=Function;
                                  ];


(*Common error generation parameter*)


FlushLimit=5*Degree;


(*Predefined arrow types*)


DefineArrowType[
  Line|"Line",
  Function[
    {Self,a1,a2,CanvasPoints,UsedWidth},

    (* override stored curves to force zero effective width for Line arrow *)
    Self@SetLeftPoints[CanvasPoints];
    Self@SetRightPoints[CanvasPoints];

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


General::arrownearflush="The direction of the arrow is close to the direction with which you are trying to make its tail (or head) flush.  Consider setting TailFlush->False (or HeadFlush->False).";
General::arrowflush="The direction of the arrow is exactly along the direction with which you are trying to make its tail (or head) flush.  Please set TailFlush->False (or HeadFlush->False).";


(*Shape arrow geometry and kink geometry*)
(**)
(*Base curve segments, unit vectors, and endpoints*)
(*     *-----*-----*---*----*>                 *)
(*     p1 s1             s2 p2 *)
(*        u1 >           u2 >*)
(*        n1 v           n1 v*)
(**)
(*Then n1f and n2f are flushed against endpoint anchor line (but not necessarily in the same sense as endpoint anchor).*)
(*                *)
(*"Shape" arrow points*)
(*(shown for case of no arrowhead on tail but arrowhead on head)*)
(*                                                  **)
(*                      |\                       *)
(*     *------ ... -----* \                      *)
(*     |                   \          *)
(*     |                    **)
(*     |                   /          *)
(*     *------ ... -----* /                      *)
(*                      |/                       *)
(*                                                  **)
(*   Points1        Points2*)
(**)
(*"DoubleLine" arrow points*)
(*(shown for case of no arrowhead on tail but arrowhead on head)*)
(*                                                  **)
(*                       \                       *)
(*     *------ ... -------*                      *)
(*                         \          *)
(*     X                    **)
(*                         /          *)
(*     *------ ... -------*                      *)
(*                       /                       *)
(*                                                  **)
(*   Points1        Points2*)
(*   *)
(*Calculation is more easily conceptualized on tail, where sense of u1 is "into" arrow shaft:*)
(*	Arrow lip point: p1+h*u1-(d+l)*n1*)
(*	"Shape" arrow upper waist point: p1+h*u1-d*n1*)
(*	"DoubleLine" arrow lower waist point: p1+d/(d+l)*( h*u1-(d+l)*n1 )*)
(*One "DoubleLine", curve must be split for outline but not for fill.*)
(*	*)
(*Head is then obtained by 1->2 and reversing sign on tangent vector (+h -> -h).*)


DefineArrowType[
  "Block"|Block,
  Function[
    {Self,a1,a2,CanvasPoints,UsedWidth},
    Module[
      {s1,s2,p1,p2,u1,u2,n1,n2,n1f,n2f,d,h,l,FlushFactor,FlushLimit,Points1,Points2,Points},

      (* extract geometry from end segments *)
      (* segments *)
      s1=CurveSegment[CanvasPoints,1];
      s2=CurveSegment[CanvasPoints,-1];
      (* endpoints *)
      p1=s1[[1]];
      p2=s2[[-1]];
      (* tangents *)
      u1=SegmentTangent[s1];
      u2=SegmentTangent[s2];
      (* normals *)
      n1=-Cross[u1];
      n2=-Cross[u2];

      (* flush normals -- to same side as *)
      n1f=FromPolar[{1,AnchorAngle[a1]}];
      n1f*=Sign[n1f.n1];
      n2f=FromPolar[{1,AnchorAngle[a2]}];
      n2f*=Sign[n2f.n2];

      (* calculate displaced points for arrowheads *)
      (* note: points arranged curve-left to curve-right -- will use Reverse later to ensure counterclockwise sense *)
      d=UsedWidth/2;

      (* Points1: tail *)
      h=(TailLength/.FigOptions);
      l=(TailLip/.FigOptions);
      Points1=Which[
        (* case: bare, no flush *)
        !(ShowTail/.FigOptions)&&!(TailFlush/.FigOptions),
        {p1-d*n1,p1+d*n1},
        (* case: bare, flush *)
        !(ShowTail/.FigOptions)&&(TailFlush/.FigOptions),
        FlushFactor=(n1f.n1);
        If[Chop[FlushFactor]==0,FigError[Self,"arrowflush"]];
        If[ArcSin[FlushFactor]<FlushLimit,FigMessage[Self,"arrownearflush"]];
        {p1-d*n1f/FlushFactor,p1+d*n1f/FlushFactor},
        (* case: arrowhead *)
        (ShowTail/.FigOptions),
        {p1+h*u1-d*n1,p1+h*u1-(d+l)*n1,p1,p1+h*u1+(d+l)*n1,p1+h*u1+(d)*n1}
              ];
      (* Points2: head *)
      (* related by 1\[Rule]2 and reversed sign on tangent vector (+h \[Rule] -h)*)
      h=(HeadLength/.FigOptions);
      l=(HeadLip/.FigOptions);
      Points2=Which[
        (* case: bare, no flush *)
        ((ShowHead/.FigOptions)==False)&&((HeadFlush/.FigOptions)==False),
        {p2-d*n2,p2+d*n2},
        (* case: bare, flush *)
        ((ShowHead/.FigOptions)==False)&&((HeadFlush/.FigOptions)==True),
        FlushFactor=(n2f.n2);
        If[Chop[FlushFactor]==0,FigError[Self,"arrowflush"]];
        If[ArcSin[FlushFactor]<FlushLimit,FigMessage[Self,"arrownearflush"]];
        {p2-d*n2f/FlushFactor,p2+d*n2f/FlushFactor},
        ((ShowHead/.FigOptions)==True),
        (* case: arrowhead *)
        {p2-h*u2-d*n2,p2-h*u2-(d+l)*n2,p2,p2-h*u2+(d+l)*n2,p2-h*u2+(d)*n2}
              ];

      (* amalgamated points*)
      Points=Join[
        Points1,
        Take[Self@GetRightPoints[],{2,-2}],
        Reverse@Points2,
        Reverse@Take[Self@GetLeftPoints[],{2,-2}]
             ];

      (* emit graphics *)
      FigPolygonElement[
        {Polygon[Points]},
        FigOptions
      ];

    ]
  ]
];


DefineArrowType[
  "DoubleLine",
  Function[
    {Self,a1,a2,CanvasPoints,UsedWidth},
    Module[
      {s1,s2,p1,p2,u1,u2,n1,n2,n1f,n2f,d,h,l,FlushFactor,FlushLimit,Points1,Points2,Points,CurvePoints,FillPoints},

      (* extract geometry from end segments *)
      (* segments *)
      s1=CurveSegment[CanvasPoints,1];
      s2=CurveSegment[CanvasPoints,-1];
      (* endpoints *)
      p1=s1[[1]];
      p2=s2[[-1]];
      (* tangents *)
      u1=SegmentTangent[s1];
      u2=SegmentTangent[s2];
      (* normals *)
      n1=-Cross[u1];
      n2=-Cross[u2];

      (* flush normals -- to same side as *)
      n1f=FromPolar[{1,AnchorAngle[a1]}];
      n1f*=Sign[n1f.n1];
      n2f=FromPolar[{1,AnchorAngle[a2]}];
      n2f*=Sign[n2f.n2];

      (* calculate displaced points for arrowheads *)
      (* note: points arranged curve-left to curve-right -- will use Reverse later to ensure counterclockwise sense *)
      d=UsedWidth/2;
      h=(HeadLength/.FigOptions);
      l=(HeadLip/.FigOptions);

      (* Points1: tail *)
      h=(TailLength/.FigOptions);
      l=(TailLip/.FigOptions);
      Points1=Which[
        (* case: bare, no flush *)
        !(ShowTail/.FigOptions)&&!(TailFlush/.FigOptions),
        {p1-d*n1,Split,p1+d*n1},
        (* case: bare, flush *)
        !(ShowTail/.FigOptions)&&(TailFlush/.FigOptions),
        FlushFactor=(n1f.n1);
        If[Chop[FlushFactor]==0,FigError[Self,"arrowflush"]];
        If[ArcSin[FlushFactor]<FlushLimit,FigMessage[Self,"arrownearflush"]];
        {p1-d*n1f/FlushFactor,Split,p1+d*n1f/FlushFactor},
        (* case: arrowhead *)
        (ShowTail/.FigOptions),
        {p1+d/(d+l)*( h*u1-(d+l)*n1 ),p1+h*u1-(d+l)*n1,p1,p1+h*u1+(d+l)*n1,p1+d/(d+l)*( h*u1+(d+l)*n1 )}
              ];
      (* Points2: head *)
      (* related by 1\[Rule]2 and reversed sign on tangent vector (+h \[Rule] -h)*)
      h=(HeadLength/.FigOptions);
      l=(HeadLip/.FigOptions);
      Points2=Which[
        (* case: bare, no flush *)
        ((ShowHead/.FigOptions)==False)&&((HeadFlush/.FigOptions)==False),
        {p2-d*n2,Split,p2+d*n2},
        (* case: bare, flush *)
        ((ShowHead/.FigOptions)==False)&&((HeadFlush/.FigOptions)==True),
        FlushFactor=(n2f.n2);
        If[Chop[FlushFactor]==0,FigError[Self,"arrowflush"]];
        If[ArcSin[FlushFactor]<FlushLimit,FigMessage[Self,"arrownearflush"]];
        {p2-d*n2f/FlushFactor,Split,p2+d*n2f/FlushFactor},
        ((ShowHead/.FigOptions)==True),
        (* case: arrowhead *)
        {p2+d/(d+l)*(- h*u2-(d+l)*n2 ),p2-h*u2-(d+l)*n2,p2,p2-h*u2+(d+l)*n2,p2+d/(d+l)*( -h*u2+(d+l)*n2 )}
              ];

      (* amalgamated points -- to be split *)
      Points=Join[
        Points1,
        Take[Self@GetRightPoints[],{2,-2}],
        Reverse@Points2,
        Reverse@Take[Self@GetLeftPoints[],{2,-2}]
             ];
      FillPoints=PurgeOfDelimiter[Points,Split];
      (* must close curve for line -- but then eliminate any one-point subcurves which are left since they will appear as a dark dot *)
      CurvePoints=Select[SplitByDelimiter[AppendFirst[Points],Split],(Length[#]>=2)&];
      If[(Debug/.FigOptions),
         Print["Points: ",Points];
         Print["FillPoints: ",FillPoints];
         Print["CurvePoints: ",CurvePoints];
      ];

      (* emit graphics *)
      FigPolygonElement[
        {Polygon[FillPoints]},
        Flatten[{ShowLine->False,FigOptions}]
      ];
      FigLineElement[
        {Line[CurvePoints]},
        FigOptions
      ];


    ]
  ]
];


SquigglePoints[{Pia_List,Pfa_List},{MinBuffer1_,MinBuffer2_},HalfWidth_,SquiggleWavelength_,SquiggleSide_,PlotPoints_]:=Module[
  {theta,d,dp,Delta,x,PointList,s},
  s=Switch[SquiggleSide,Right,+1,Left,-1];
  theta=VectorArcTan[Pfa-Pia];
  d=VectorLength[Pfa-Pia];
  dp=(SquiggleWavelength/2)*Floor[Max[(d-MinBuffer1-MinBuffer2),0]/(SquiggleWavelength/2)];
  Delta=(d-MinBuffer1-MinBuffer2-dp)/2;
  PointList=Table[
    Pia+RotationTransform[theta,{0,0}][{x+MinBuffer1+Delta,s*HalfWidth*Sin[2*Pi/SquiggleWavelength*x]}],
    {x,0,dp,SquiggleWavelength/PlotPoints}
            ];
  Join[{Pia},PointList,{Pfa}]
                                                                                                                        ];


General::squiggletwopoint="A squiggle arrow can only be drawn for a single line segment, but the given curve has `1` points.";
DefineArrowType[
  "Squiggle",
  Function[
    {Self,a1,a2,CanvasPoints,UsedWidth},
    Module[
      {
        CurvePoints
      },

      (* validate special options *)
      FigCheckOption[Self,SquiggleBuffer,_?NonNegative,FigOptions];
      FigCheckOption[Self,SquiggleWavelength,_?Positive,FigOptions];
      FigCheckOption[Self,SquiggleSide,Left|Right,FigOptions];
      FigCheckOption[Self,PlotPoints,(_Integer)?Positive,FigOptions];
      If[
        Length[CanvasPoints]!=2,
        FigError[Self,"squiggletwopoint",Length[CanvasPoints]]
      ];

      (* curve *)
      CurvePoints=SquigglePoints[
        (* endpoints *)
        CanvasPoints,
        (* required insets before squiggling *)
        {
          If[(ShowTail/.FigOptions),(TailLength/.FigOptions),0]+(SquiggleBuffer/.FigOptions),
          If[(ShowHead/.FigOptions),(HeadLength/.FigOptions),0]+(SquiggleBuffer/.FigOptions)
        },
        (* half-width, as squiggle amplitude *)
        UsedWidth/2,
        (* squiggle wavelength *)
        (SquiggleWavelength/.FigOptions),
        (* squiggle side *)
        (SquiggleSide/.FigOptions),
        (* points per squiggle wavelength *)
        (PlotPoints/.FigOptions)
                  ];
      FigLineElement[
        {Line[CurvePoints]},
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
  ]
];


DefineArrowType[
  {"Conversion",TypeOptions___?OptionQ},
  Function[
    {Self,a1,a2,CanvasPoints,UsedWidth},
    Module[
      {SpecialOptions=Flatten[{TypeOptions,{ConversionSide->Left,ConversionCoefficient->None,ConversionFillColor->Default}}]},

      (* validate special options *)
      (*FigCheckOption[Self,"SquiggleBuffer",_?NonNegative,SpecialOptions];*)
      (*If[((ConversionCoeff/.FullOpts)=!=None)&&((HeadLip/.FullOpts)=!=0),Message[Caller::conversionlip]];*)

      (* TODO *)
      Abort[]
    ]
  ]
];


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
