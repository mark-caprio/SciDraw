(* ::Package:: *)

(*Header comments*)


(* :Title: FigScheme *)
(* :Context: SciDraw` *)
(* :Summary: Level scheme drawing *)
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


Lev::usage="FIGURE OBJECT: Lev[E,{c1,c2}] or Lev[c1,c2,E] generates an energy level.";
Resonance::usage="FIGURE OBJECT: Resonance[E,{c1,c2}] generates an energy level.  Child of Lev.";
ExtensionLine::usage="FIGURE OBJECT: ExtensionLine[level,side,extension] generates an extension line attached to an energy level.";
Connector::usage="FIGURE OBJECT: Connector[level1,level2] generates a connectingline between levels.";BandLabel::usage="FIGURE OBJECT: BandLabel[level,text] attaches a label to a level at the Bottom position.";
Trans::usage="FIGURE OBJECT: Trans[level1,level2] generates a transition arrow.";


(*Other usage*)


(*Utilities*)


LastLevel::usage="LastLevel[] refers to the last Lev object drawn, or None if none have been drawn.";
LevelEnergyLabel::usage="LevelEnergyLabel[level] refers to the default energy label text for the level lev.";
AutoLevelInit::usage="AutoLevelInit[position,space,bigspace] initializes the positioning parameters for automatic decay scheme generation."; 
AutoLevel::usage="AutoLevel[level1] selects a new starting level for transitions."; 
AutoTrans::usage="AutoTrans[level2] draws a transition to the designated ending level, in automatic decay scheme generation.  Any options taken by Trans may be given to AutoTrans."; 
LevelShellPoints::usage="LevelShellPoints[level,numpoints] generates list of points (in the form Canvas[...]) at which to place symbols to indicate shell occupancy."; 


(*Options*)


Margin::usage="Option name for use with figure objects.";
VerticalShift::usage="Option name for use with figure objects.";
EnergyLabelFunction::usage="Option name for use with figure objects.";
WingHeight::usage="Option name for use with figure objects.";
WingSlopeWidth::usage="Option name for use with figure objects.";
WingTipWidth::usage="Option name for use with figure objects.";
MakeWing::usage="Option name for use with figure objects.";


(*IntermediatePoints::usage="Option name for use with figure objects."; -- defined by FigLabel *)
EndPositions::usage="Option name for use with figure objects.";
ToWing::usage="Option name for use with figure objects.";


(*Begin private context*)


Begin["`Private`"];


(*Dependencies*)





(*Lev*)


(*Object declaration*)


DeclareFigClass[
  Lev,
  {
    "NominalPoints",  (* canvas coordinates of nominal endpoints  -- those for marginless level *)
    "BasePoints",  (* canvas coordinates of base endpoints  -- those for wingless level *)
    "Points",  (* canvas coordinates of true endpoints -- at wing roots if there are wings *)
    "UserXWidth",  (* nominal coordinate width -- difference of endpoint x coordinates given by user *)
    "UserMargins",  (* margins given by user -- always expanded out to list {m1,m2} *)
    "EnergyLabel"  (* label constructred for use as automatic energy label *)
  },
  {},
  {Center,Left,Right,Bottom,Top}
];
DefineFigClassOptions[
  Lev,
  {
    (* line geometry*)
    Margin->0.1,
    VerticalShift->None,

    (* automatic energy label  *)
    EnergyLabelFunction->Automatic,
    DecimalDigits->Automatic,

    (* gull wings *)
    WingHeight->0,
    WingSlopeWidth->10,
    WingTipWidth->40,
    MakeWing->True
  }
];


(*Last level storage*)


$LastLevel=None;


(*Constructor*)


Lev::xorder="Level has zero or negative length, originally given coordinates `1`, and ultimately having endpoints `2` after applying margins.";


Constructor[Class:Lev,Self_Object][x1_?NumericQ,x2_?NumericQ,EnergyStr:(_?NumericQ|_String),Opts___?OptionQ]:=Constructor[Class,Self][EnergyStr,{x1,x2},Opts];


Constructor[Class:Lev,Self_Object][EnergyStr:(_?NumericQ|_String),{x1_?NumericQ,x2_?NumericQ},Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                                                 Module[
                                                                                                                                   {
                                                                                                                                     CanvasPoints,NominalCanvasPoints,BaseCanvasPoints,Energy,UsedVerticalShift,NudgeShift,x1m,x2m,p1m,p2m,
                                                                                                                                     UsedMargin,UsedWingHeight,UsedWingSlopeWidth,UsedWingTipWidth,UsedMakeWing,
                                                                                                                                     DerivedEnergyLabelFunction,DerivedEnergyLabel,LabelName,Side
                                                                                                                                   },

                                                                                                                                   (* validate extra options *)
                                                                                                                                   FigCheckOption[Self,Margin,IntervalParametersPattern,FigOptions];
                                                                                                                                   FigCheckOption[Self,VerticalShift,ScalarParameterPattern,FigOptions];
                                                                                                                                   FigCheckOption[Self,EnergyLabelFunction,_,FigOptions];
                                                                                                                                   FigCheckOption[Self,DecimalDigits,Automatic|((_Integer)?NonNegative)|{((_Integer)?NonNegative),((_Integer)?NonNegative)},FigOptions];
                                                                                                                                   FigCheckOption[Self,WingHeight,IntervalParametersPattern,FigOptions];
                                                                                                                                   FigCheckOption[Self,WingSlopeWidth,IntervalParametersPattern,FigOptions];
                                                                                                                                   FigCheckOption[Self,WingTipWidth,IntervalParametersPattern,FigOptions];
                                                                                                                                   FigCheckOption[Self,MakeWing,LogicalPattern|{LogicalPattern,LogicalPattern},FigOptions];
                                                                                                                                   If[x2<x1,FigMessage[]];

                                                                                                                                   (* option expansion *)
                                                                                                                                   UsedMargin=UpgradePairEqual[(Margin/.FigOptions)];
                                                                                                                                   UsedVerticalShift=UpgradeScalar[(VerticalShift/.FigOptions)];
                                                                                                                                   UsedWingHeight=UpgradePairEqual[(WingHeight/.FigOptions)];
                                                                                                                                   UsedWingSlopeWidth=UpgradePairEqual[(WingSlopeWidth/.FigOptions)];
                                                                                                                                   UsedWingTipWidth=UpgradePairEqual[(WingTipWidth/.FigOptions)];
                                                                                                                                   UsedMakeWing=UpgradePair[(MakeWing/.FigOptions)];  (* not numeric, so use UpgradePair *)

                                                                                                                                   (* prerequisite calculations *)
                                                                                                                                   Energy=Switch[EnergyStr,
                                                                                                                                                 _?NumericQ,EnergyStr,
                                                                                                                                                 _String,ToExpression[EnergyStr]
                                                                                                                                          ];

                                                                                                                                   (* nominal canvas points -- those for marginless level *)
                                                                                                                                   NudgeShift={{0,UsedVerticalShift},{0,UsedVerticalShift}};
                                                                                                                                   NominalCanvasPoints=FigResolvePoint/@{{x1,Energy},{x2,Energy}}+NudgeShift;
                                                                                                                                   Self@SetNominalPoints[NominalCanvasPoints];
                                                                                                                                   Self@SetUserXWidth[x2-x1];

                                                                                                                                   (* base canvas points -- those for wingless level *)
                                                                                                                                   {x1m,x2m}=ExtendInterval[{x1,x2},-UsedMargin,Absolute];
                                                                                                                                   If[
                                                                                                                                     x1m>=x2m,
                                                                                                                                     FigError[Self,"xorder",{x1,x2},{x1m,x2m}]
                                                                                                                                   ];
                                                                                                                                   BaseCanvasPoints={p1m,p2m}=FigResolvePoint/@{{x1m,Energy},{x2m,Energy}}+NudgeShift;
                                                                                                                                   Self@SetBasePoints[BaseCanvasPoints];
                                                                                                                                   Self@SetUserMargins[UsedMargin];

                                                                                                                                   (* true canvas points -- at wing roots if there are wings *)
                                                                                                                                   CanvasPoints=Join[
                                                                                                                                     If[
                                                                                                                                       (UsedWingHeight[[1]]==0)||(!UsedMakeWing[[1]]),
                                                                                                                                       {p1m},
                                                                                                                                       {p1m+{0,UsedWingHeight[[1]]},p1m+{UsedWingTipWidth[[1]],UsedWingHeight[[1]]},p1m+{UsedWingTipWidth[[1]]+UsedWingSlopeWidth[[1]],0}}
                                                                                                                                     ],
                                                                                                                                     Reverse@If[
                                                                                                                                       (UsedWingHeight[[2]]==0)||(!UsedMakeWing[[2]]),
                                                                                                                                       {p2m},
                                                                                                                                       {p2m+{0,UsedWingHeight[[1]]},p2m+{-UsedWingTipWidth[[1]],UsedWingHeight[[1]]},p2m+{-UsedWingTipWidth[[1]]-UsedWingSlopeWidth[[1]],0}}
                                                                                                                                             ]
                                                                                                                                                ];
                                                                                                                                   Self@SetPoints[CanvasPoints];

                                                                                                                                   (* make graphics elements *)
                                                                                                                                   FigLineElement[
                                                                                                                                     {Line[CanvasPoints]},
                                                                                                                                     FigOptions
                                                                                                                                   ];

                                                                                                                                   (* define energy label text *)
                                                                                                                                   (* energy label priority: (1) string, (2) specified function, (3) specified decimal digits, (4) simple pass through of number*)
                                                                                                                                   DerivedEnergyLabelFunction=If[
                                                                                                                                     (EnergyLabelFunction/.FigOptions)===Automatic,
                                                                                                                                     If[
                                                                                                                                       (DecimalDigits/.FigOptions)===Automatic,
                                                                                                                                       Identity,
                                                                                                                                       (FixedPointForm[#,(DecimalDigits/.FigOptions)]&)
                                                                                                                                     ],
                                                                                                                                     (EnergyLabelFunction/.FigOptions)
                                                                                                                                                              ];
                                                                                                                                   DerivedEnergyLabel=Switch[
                                                                                                                                     EnergyStr,
                                                                                                                                     _String,EnergyStr,
                                                                                                                                     _?NumericQ,(DerivedEnergyLabelFunction/.FigOptions)@EnergyStr
                                                                                                                                                      ];
                                                                                                                                   Self@SetEnergyLabel[DerivedEnergyLabel];

                                                                                                                                   (* push energy label text in place of Automatic label values *) 
                                                                                                                                   FigOptions=Join[
                                                                                                                                     Table[
                                                                                                                                       LabelName=SidifyOptionName[Side][Label];
                                                                                                                                       LabelName->ResolveOption[LabelName,{Automatic->DerivedEnergyLabel},FigOptions],
                                                                                                                                       {Side,$FigClassAttachedLabels[ObjectClass[Self]]}
                                                                                                                                     ],
                                                                                                                                     FigOptions
                                                                                                                                              ];

                                                                                                                                   (* record self as last level *)
                                                                                                                                   $LastLevel=Self;
                                                                                                                                 ]
                                                                                                                ];


(*Methods*)


MakeAnchor[Class:Lev,Self_Object][Name:Left,Arg:None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                            FigAnchor[Absolute[First[Self@GetPoints[]]],Right]
                                                       ];
MakeAnchor[Class:Lev,Self_Object][Name:Right,Arg:None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                             FigAnchor[Absolute[Last[Self@GetPoints[]]],Left]
                                                        ];
MakeAnchor[Class:Lev,Self_Object][Name:Bottom,Arg:None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                              FigAnchor[CentroidPoint[Absolute/@(Self@GetBasePoints[])],Top]
                                                         ];
MakeAnchor[Class:Lev,Self_Object][Name:Top,Arg:None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                           FigAnchor[CentroidPoint[Absolute/@(Self@GetBasePoints[])],Bottom]
                                                      ];
MakeAnchor[Class:Lev,Self_Object][Name:Center,Arg:None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                              FigAnchor[CentroidPoint[Absolute/@(Self@GetBasePoints[])]]
                                                         ];


(*Anchor: Level*)
(*Argument:*)
(*	x -- at level height, x user units from nominal left endpoint*)
(*	{Left,x} -- at left wing height, x user units from nominal left endpoint*)
(*	{Right,x} -- at right wing height, x user units from nominal left endpoint*)


MakeAnchor[Class:Lev,Self_Object][Name:Level,Arg:(x_?NumericQ)]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                      Module[
                                                                                        {u=x/(Self@GetUserXWidth[])},
                                                                                        FigAnchor[Absolute[InterpolateSegment[Self@GetNominalPoints[],Tail,u]]]
                                                                                      ]
                                                                 ];
MakeAnchor[Class:Lev,Self_Object][Name:Level,Arg:{Side:(Left|Right),(x_?NumericQ)}]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                                          Module[
                                                                                                            {u=x/(Self@GetUserXWidth[])},
                                                                                                            FigAnchor[Absolute[{
                                                                                                              First[InterpolateSegment[Self@GetNominalPoints[],Tail,u]],
                                                                                                              Last[(Self@GetPoints[])[[Switch[Side,Left,1,Right,-1]]]]
                                                                                                              }]]
                                                                                                          ]
                                                                                     ];


MakeBoundingBox[Class:Lev,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*Level utilities*)


(*LastLevel*)


LastLevel[]:=$LastLevel;
DeclareFigFallThroughError[LastLevel];


(*LevelEnergyLabel*)


LevelEnergyLabel[Object[LevelName:ObjectNamePattern[Lev]]|(LevelName:ObjectNamePattern[Lev])]:=(Object[LevelName]@GetEnergyLabel[]);
DeclareFigFallThroughError[LevelEnergyLabel];


(*Resonance*)


(*Object declaration*)


Resonance::xorder=Lev::xorder;


DeclareFigClass[
  Resonance,
  Lev,
  {
    "CanvasRegion"  (* canvas bounding box region *)
  },
  {},
  {Center,Left,Right,Bottom,Top}
];
DefineFigClassOptions[
  Resonance,
  {
    Width->0,
    PlotPoints->1,
    "DensityRange"->{0.5,1}
  }
];


Constructor[Class:Resonance,Self_Object][EnergyStr:(_?NumericQ|_String),{x1_?NumericQ,x2_?NumericQ},Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                                                       Module[
                                                                                                                                         {
                                                                                                                                           CanvasPoints,NominalCanvasPoints,BaseCanvasPoints,Energy,UsedVerticalShift,NudgeShift,x1m,x2m,p1m,p2m,f,df,i,LeafDensityFraction,LeafCanvasPoints,CanvasRegion,LeafCanvasRegion,
                                                                                                                                           UsedMargin,UsedWingHeight,UsedWingSlopeWidth,UsedWingTipWidth,UsedMakeWing,UsedWidth,UsedPlotPoints,UsedDensityRange,TheColor,
                                                                                                                                           DerivedEnergyLabelFunction,DerivedEnergyLabel,LabelName,Side
                                                                                                                                         },

                                                                                                                                         (* validate extra options -- inherited from Lev *)
                                                                                                                                         FigCheckOption[Self,Margin,IntervalParametersPattern,FigOptions];
                                                                                                                                         FigCheckOption[Self,VerticalShift,ScalarParameterPattern,FigOptions];
                                                                                                                                         FigCheckOption[Self,EnergyLabelFunction,_,FigOptions];
                                                                                                                                         FigCheckOption[Self,DecimalDigits,Automatic|((_Integer)?NonNegative)|{((_Integer)?NonNegative),((_Integer)?NonNegative)},FigOptions];
                                                                                                                                         FigCheckOption[Self,WingHeight,IntervalParametersPattern,FigOptions];
                                                                                                                                         FigCheckOption[Self,WingSlopeWidth,IntervalParametersPattern,FigOptions];
                                                                                                                                         FigCheckOption[Self,WingTipWidth,IntervalParametersPattern,FigOptions];
                                                                                                                                         FigCheckOption[Self,MakeWing,LogicalPattern|{LogicalPattern,LogicalPattern},FigOptions];
                                                                                                                                         If[x2<x1,FigMessage[]];

                                                                                                                                         (* validate extra options -- new *)
                                                                                                                                         (*FigCheckOption[Self,Width,NonNegativeScalarParameterPattern,FigOptions];*)
                                                                                                                                         (*FigCheckOption[Self,Width,((_NumericQ)?Positive),FigOptions];*)
                                                                                                                                         FigCheckOption[Self,Width,NonNegativeScalarParameterPattern,FigOptions];
                                                                                                                                         FigCheckOption[Self,PlotPoints,((_Integer)?Positive),FigOptions];
                                                                                                                                         FigCheckOption[Self,"DensityRange",{NonNegativePattern,NonNegativePattern},FigOptions];

                                                                                                                                         (* option expansion -- inherited *)
                                                                                                                                         UsedMargin=UpgradePairEqual[(Margin/.FigOptions)];
                                                                                                                                         UsedVerticalShift=UpgradeScalar[(VerticalShift/.FigOptions)];
                                                                                                                                         (*
UsedWingHeight=UpgradePairEqual[(WingHeight/.FigOptions)];
UsedWingSlopeWidth=UpgradePairEqual[(WingSlopeWidth/.FigOptions)];
UsedWingTipWidth=UpgradePairEqual[(WingTipWidth/.FigOptions)];
UsedMakeWing=UpgradePair[(MakeWing/.FigOptions)];  (* not numeric, so use UpgradePair *)
                                                                                                                                          *)

                                                                                                                                         (* option expansion -- new *)
                                                                                                                                         UsedWidth=UpgradeScalar[(Width/.FigOptions)];
                                                                                                                                         UsedPlotPoints=(PlotPoints/.FigOptions);
                                                                                                                                         UsedDensityRange=("DensityRange"/.FigOptions);

                                                                                                                                         (* prerequisite calculations *)
                                                                                                                                         Energy=Switch[EnergyStr,
                                                                                                                                                       _?NumericQ,EnergyStr,
                                                                                                                                                       _String,ToExpression[EnergyStr]
                                                                                                                                                ];

                                                                                                                                         (* nominal canvas points -- those for marginless level *)
                                                                                                                                         NudgeShift={{0,UsedVerticalShift},{0,UsedVerticalShift}};
                                                                                                                                         NominalCanvasPoints=FigResolvePoint/@{{x1,Energy},{x2,Energy}}+NudgeShift;
                                                                                                                                         Self@SetNominalPoints[NominalCanvasPoints];
                                                                                                                                         Self@SetUserXWidth[x2-x1];

                                                                                                                                         (* base canvas points -- those for wingless level *)
                                                                                                                                         {x1m,x2m}=ExtendInterval[{x1,x2},-UsedMargin,Absolute];
                                                                                                                                         If[
                                                                                                                                           x1m>=x2m,
                                                                                                                                           FigError[Self,"xorder",{x1,x2},{x1m,x2m}]
                                                                                                                                         ];
                                                                                                                                         BaseCanvasPoints={p1m,p2m}=FigResolvePoint/@{{x1m,Energy},{x2m,Energy}}+NudgeShift;
                                                                                                                                         Self@SetBasePoints[BaseCanvasPoints];
                                                                                                                                         Self@SetUserMargins[UsedMargin];

                                                                                                                                         (* true canvas points -- at wing roots if there are wings -- N/A for resonance*)
                                                                                                                                         CanvasPoints=BaseCanvasPoints;
                                                                                                                                         Self@SetPoints[CanvasPoints];

                                                                                                                                         (* region canvas points *)
                                                                                                                                         CanvasRegion=FigResolveRegion[{{x1m,x2m},{Energy-UsedWidth/2,Energy+UsedWidth/2}}+NudgeShift];
                                                                                                                                         Self@SetCanvasRegion[CanvasRegion];

                                                                                                                                         (* make graphics elements *)
                                                                                                                                         TheColor=ResolveOption[FillColor,{Default:>(Color/.FigOptions)},FigOptions];
                                                                                                                                         FigPolygonElement[
                                                                                                                                           Table[
                                                                                                                                             f=i/UsedPlotPoints;  (* i=1\[Rule]f=1/n, i=n\[Rule]f=1 *)
                                                                                                                                             df=(1-f)/2; (* i=1\[Rule]df=1/2*(n-1)/n, i=n\[Rule]df=0 *)
                                                                                                                                             LeafDensityFraction= (1-f)*UsedDensityRange[[2]]+f*UsedDensityRange[[1]]; (*f=0\[Rule]max,f=1\[Rule]min*)
                                                                                                                                             LeafCanvasRegion=ExtendRegion[CanvasRegion,{{0,0},{-df,-df}},Scaled];
                                                                                                                                             LeafCanvasPoints={{LeafCanvasRegion[[1,1]],LeafCanvasRegion[[2,1]]},{LeafCanvasRegion[[1,2]],LeafCanvasRegion[[2,2]]}};
                                                                                                                                             {Lighter[TheColor,1-LeafDensityFraction],Rectangle@@LeafCanvasPoints},
                                                                                                                                             {i,UsedPlotPoints,1,-1}
                                                                                                                                           ],
                                                                                                                                           Flatten[{ShowLine->False,FigOptions}]
                                                                                                                                         ];

                                                                                                                                         (* define energy label text *)
                                                                                                                                         (* energy label priority: (1) string, (2) specified function, (3) specified decimal digits, (4) simple pass through of number*)
                                                                                                                                         DerivedEnergyLabelFunction=If[
                                                                                                                                           (EnergyLabelFunction/.FigOptions)===Automatic,
                                                                                                                                           If[
                                                                                                                                             (DecimalDigits/.FigOptions)===Automatic,
                                                                                                                                             Identity,
                                                                                                                                             (FixedPointForm[#,(DecimalDigits/.FigOptions)]&)
                                                                                                                                           ],
                                                                                                                                           (EnergyLabelFunction/.FigOptions)
                                                                                                                                                                    ];
                                                                                                                                         DerivedEnergyLabel=Switch[
                                                                                                                                           EnergyStr,
                                                                                                                                           _String,EnergyStr,
                                                                                                                                           _?NumericQ,(DerivedEnergyLabelFunction/.FigOptions)@EnergyStr
                                                                                                                                                            ];
                                                                                                                                         Self@SetEnergyLabel[DerivedEnergyLabel];

                                                                                                                                         (* push energy label text in place of Automatic label values *) 
                                                                                                                                         FigOptions=Join[
                                                                                                                                           Table[
                                                                                                                                             LabelName=SidifyOptionName[Side][Label];
                                                                                                                                             LabelName->ResolveOption[LabelName,{Automatic->DerivedEnergyLabel},FigOptions],
                                                                                                                                             {Side,$FigClassAttachedLabels[ObjectClass[Self]]}
                                                                                                                                           ],
                                                                                                                                           FigOptions
                                                                                                                                                    ];

                                                                                                                                         (* record self as last level *)
                                                                                                                                         $LastLevel=Self;
                                                                                                                                       ]
                                                                                                                      ];


(*Methods*)


(* Anchors are based on the central "line", as if this were a Lev, while bounding box respects full rectangular region of resonance. *)


MakeAnchor[Class:Resonance,Self_Object][Name:Left,Arg:None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                  FigAnchor[Absolute[First[Self@GetPoints[]]],Right]
                                                             ];
MakeAnchor[Class:Resonance,Self_Object][Name:Right,Arg:None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                   FigAnchor[Absolute[Last[Self@GetPoints[]]],Left]
                                                              ];
MakeAnchor[Class:Resonance,Self_Object][Name:Bottom,Arg:None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                    FigAnchor[CentroidPoint[Absolute/@(Self@GetBasePoints[])],Top]
                                                               ];
MakeAnchor[Class:Resonance,Self_Object][Name:Top,Arg:None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                 FigAnchor[CentroidPoint[Absolute/@(Self@GetBasePoints[])],Bottom]
                                                            ];
MakeAnchor[Class:Resonance,Self_Object][Name:Center,Arg:None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                    FigAnchor[CentroidPoint[Absolute/@(Self@GetBasePoints[])]]
                                                               ];


(*Anchor: Level*)
(*Argument:*)
(*	x -- at level height, x user units from nominal left endpoint*)
(*	{Left,x} -- at left wing height, x user units from nominal left endpoint*)
(*	{Right,x} -- at right wing height, x user units from nominal left endpoint*)


MakeAnchor[Class:Resonance,Self_Object][Name:Level,Arg:(x_?NumericQ)]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                            Module[
                                                                                              {u=x/(Self@GetUserXWidth[])},
                                                                                              FigAnchor[Absolute[InterpolateSegment[Self@GetNominalPoints[],Tail,u]]]
                                                                                            ]
                                                                       ];
MakeAnchor[Class:Resonance,Self_Object][Name:Level,Arg:{Side:(Left|Right),(x_?NumericQ)}]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                                                Module[
                                                                                                                  {u=x/(Self@GetUserXWidth[])},
                                                                                                                  FigAnchor[Absolute[{
                                                                                                                    First[InterpolateSegment[Self@GetNominalPoints[],Tail,u]],
                                                                                                                    Last[(Self@GetPoints[])[[Switch[Side,Left,1,Right,-1]]]]
                                                                                                                    }]]
                                                                                                                ]
                                                                                           ];


MakeBoundingBox[Class:Resonance,Self_Object][]:=(Self@GetCanvasRegion[]);


(*ExtensionLine*)


(*Object declaration*)


DeclareFigClass[
  ExtensionLine,
  {"Points"},
  {},
  {Left,Right}
];
DefineFigClassOptions[
  ExtensionLine,
  {
    (* geometry *)
    ToWing->True
  }
];


(*Constructor*)


Constructor[Class:ExtensionLine,Self_Object][
  Object[LevelName:ObjectNamePattern[Lev]]|(LevelName:ObjectNamePattern[Lev]),
  Side:(Left|Right),
  (x_?NonNegative),
  Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                     Module[
                                       {CanvasPoints,x1,x2,Arg1,Arg2,UsedMargin},

                                       (* validate extra options *)
                                       FigCheckOption[Self,ToWing,LogicalPattern,FigOptions];

                                       (* construct full curve specification *)
                                       UsedMargin=(Object[LevelName]@GetUserMargins[]);
                                       {x1,x2}=Switch[Side,
                                                      Left,
                                                      {UsedMargin[[1]]-x,UsedMargin[[1]]},
                                                      Right,
                                                      (Object[LevelName]@GetUserXWidth[])+
                                                      {-UsedMargin[[2]],-UsedMargin[[2]]+x}
                                               ];
                                       {Arg1,Arg2}=If[
                                         (ToWing/.FigOptions),
                                         {Side,#}&/@{x1,x2},
                                         {x1,x2}
                                                   ];
                                       CanvasPoints=FigResolvePoint/@{FigAnchor[LevelName,Level,Arg1],FigAnchor[LevelName,Level,Arg2]};
                                       Self@SetPoints[CanvasPoints];

                                       (* make graphics elements *)
                                       FigLineElement[
                                         {Line[CanvasPoints]},
                                         FigOptions
                                       ];


                                     ]
                    ];


(*Methods*)


MakeAnchor[Class:ExtensionLine,Self_Object][Name:Left,None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                  FigCurveAnchorFromPoints[Self@GetPoints[],Tail,None]
                                                             ];
MakeAnchor[Class:ExtensionLine,Self_Object][Name:Right,None]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                   FigCurveAnchorFromPoints[Self@GetPoints[],Head,None]
                                                              ];


MakeBoundingBox[Class:ExtensionLine,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*Connector*)


(*Object declaration*)


DeclareFigClass[
  Connector,
  {"Points"},
  {},
  {Bottom,Center,Top}
];
DefineFigClassOptions[
  Connector,
  {
    (* curve specification *)
    IntermediatePoints->None,
    FigCurveOptions
  }
];


(*Constructor*)


Constructor[Class:Connector,Self_Object][
  Object[LevelName1:ObjectNamePattern[Lev]]|(LevelName1:ObjectNamePattern[Lev]),
  Object[LevelName2:ObjectNamePattern[Lev]]|(LevelName2:ObjectNamePattern[Lev]),
  Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                     Module[
                                       {UsedIntermediatePoints,Curve,CanvasPoints},

                                       (* validate options *)
                                       FigCheckOption[Self,IntermediatePoints,None|{FigCurvePointPattern...},FigOptions];
                                       FigCheckCurveOptions[Self];

                                       (* construct callout curve specification *)
                                       UsedIntermediatePoints=ResolveOption[IntermediatePoints,{None->{}},FigOptions];
                                       Curve=Join[{FigAnchor[LevelName1,Right]},UsedIntermediatePoints,{FigAnchor[LevelName2,Left]}];
                                       CanvasPoints=FigResolveCurve[Self,Curve,FigOptions];
                                       (*CanvasPoints=FigResolvePoint/@{FigAnchor[LevelName1,Right],FigAnchor[LevelName2,Left]};*)
                                       Self@SetPoints[CanvasPoints];

                                       (* make graphics elements *)
                                       FigLineElement[
                                         {Line[CanvasPoints]},
                                         FigOptions
                                       ];


                                     ]
                    ];


(*Methods*)


MakeAnchor[Class:Connector,Self_Object][Name:Bottom,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                FigCurveAnchorFromPoints[Self@GetPoints[],Right,Arg]
                                                           ];
MakeAnchor[Class:Connector,Self_Object][Name:Center,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                FigCurveAnchorFromPoints[Self@GetPoints[],Center,Arg]
                                                           ];
MakeAnchor[Class:Connector,Self_Object][Name:Top,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                             FigCurveAnchorFromPoints[Self@GetPoints[],Left,Arg]
                                                        ];


MakeBoundingBox[Class:Connector,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*BandLabel*)


(*Object declaration*)


DeclareFigClass[
  BandLabel,
  {"TheAnchor","TextElement"},
  {},
  {}
];
DefineFigClassOptions[
  BandLabel,
  {
    VerticalShift->-3
  }
];


(*Constructor*)


(*Note: Because of the optional first argument, syntactic ambiguity occurs in the very unlikely scenario in which the text art expression also matches the criteria for an option list, e.g., is a rule expression, a list of rule expressions, or (more plausibly) the null list.*)


Constructor[Class:BandLabel,Self_Object][
  Object[LevelName:ObjectNamePattern[Lev]]|(LevelName:ObjectNamePattern[Lev]),
  TextArt_,
         Opts___?OptionQ
                                        ]:=FigObjectWrapper[Class,Self,{Opts},
                                                            Module[
                                                              {
                                                                GivenPoint,Anchor,TextElement,
                                                                CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedVerticalShift
                                                              },

                                                              (* validate options *)
                                                              FigCheckOption[Self,VerticalShift,ScalarParameterPattern,FigOptions];

                                                              (* generate point *)
                                                              UsedVerticalShift=UpgradeScalar[(VerticalShift/.FigOptions)];
                                                              Anchor=DisplacePoint[
                                                                FigAnchor[LevelName,Bottom],
                                                                Absolute[{0,UsedVerticalShift}]
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

                                                            ]
                                           ];


(*Methods*)


MakeAnchor[Class:BandLabel,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                          FigRectangleAnchor[
                                                                            (Self@GetTextElement[])@GetCenter[],(Self@GetTextElement[])@GetRadius[],(Self@GetTextElement[])@GetPivot[],(Self@GetTextElement[])@GetRotation[],
                                                                            Name,Arg
                                                                          ]
                                                     ];
MakeBoundingBox[Class:BandLabel,Self_Object][]:=FigRectangleBoundingBox[
  (Self@GetTextElement[])@GetCenter[],(Self@GetTextElement[])@GetRadius[],(Self@GetTextElement[])@GetPivot[],(Self@GetTextElement[])@GetRotation[]
                                                ];


(*Trans*)


(*Object declaration*)


DeclareFigClass[
  Trans,
  FigArrow,
  {"Points","LeftPoints","RightPoints"},
  {},
  {Center,Left,Right,Tail,Head}
];
DefineFigClassOptions[
  Trans,
  {TailFlush->True,HeadFlush->True},
  {
    (* geometry *)
    IntermediatePoints->None,
    EndPositions->0.5
  }
];


(*Constructor*)


(*Note: Trans accepts either level name (usual form of input from user) or level object reference (as, e.g., returned by LastLevel[]).*)


Trans::twoautos="You cannot specify both initial and final horizontal positions as Automatic.";
RealizeTransEndpoints[Self_Object,LevelName1_,LevelName2_,EndPositions:{x1_,x2_}]:=Module[
  {p1,p2},

  (* check for circularity *)
  If[
    (x1===Automatic)&&(x2===Automatic),
    FigError[Self,"twoautos"]
  ];

  (* resolve independently-resolvable points *)
  If[
    (x1=!=Automatic),
    p1=FigAnchor[LevelName1,Level,x1]
  ];
  If[
    (x2=!=Automatic),
    p2=FigAnchor[LevelName2,Level,x2]
  ];

  (* resolve automatic specifications *)
  (* use "x" from other endpoint, and "y" from Center anchor of present endpoint's level *)
  If[
    (x1===Automatic),
    p1=Absolute[{First[FigResolvePoint[p2]],Last[FigResolvePoint[FigAnchor[LevelName1,Center]]]}]
  ];
  If[
    (x2===Automatic),
    p2=Absolute[{First[FigResolvePoint[p1]],Last[FigResolvePoint[FigAnchor[LevelName2,Center]]]}];
  ];

  (* return result *)
  {p1,p2}
                                                                                   ];


UpgradePairEqualAutomatic[None]:={0,0};
UpgradePairEqualAutomatic[x:(_?NumericQ|Automatic)]:={x,x};
UpgradePairEqualAutomatic[{x:(_?NumericQ|Automatic),y:(_?NumericQ|Automatic)}]:={x,y};


Constructor[Class:Trans,Self_Object][
  Object[LevelName1:ObjectNamePattern[Lev]]|(LevelName1:ObjectNamePattern[Lev]),
  Object[LevelName2:ObjectNamePattern[Lev]]|(LevelName2:ObjectNamePattern[Lev]),
  Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                     Module[
                                       {UsedEndPositions,CanvasPoints,LeftCanvasPoints,RightCanvasPoints,UsedWidth,p1,p2,Curve,UsedIntermediatePoints},

                                       (* validate extra options *)
                                       FigCheckOption[Self,IntermediatePoints,None|{FigCurvePointPattern...},FigOptions];
                                       FigCheckOption[Self,EndPositions,((_?NumericQ)|Automatic)|{((_?NumericQ)|Automatic),((_?NumericQ)|Automatic)},FigOptions];

                                       (* construct full curve specification *)
                                       UsedEndPositions=UpgradePairEqualAutomatic[(EndPositions/.FigOptions)];
                                       {p1,p2}=RealizeTransEndpoints[Self,LevelName1,LevelName2,UsedEndPositions];
                                       UsedIntermediatePoints=ResolveOption[IntermediatePoints,{None->{}},FigOptions];
                                       Curve=Join[{p1},UsedIntermediatePoints,{p2}];

                                       (* invoke basic arrow constructor code *)
                                       BasicArrow[Self][Curve];

                                     ]
                    ];


Constructor[Class:Trans,Self_Object][
  Object[LevelName1:ObjectNamePattern[Lev]]|(LevelName1:ObjectNamePattern[Lev]),
  x1:((_?NumericQ)|Automatic),
  Object[LevelName2:ObjectNamePattern[Lev]]|(LevelName2:ObjectNamePattern[Lev]),
  x2:((_?NumericQ)|Automatic),
  Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                     Module[
                                       {UsedEndPositions,CanvasPoints,LeftCanvasPoints,RightCanvasPoints,UsedWidth,p1,p2,Curve,UsedIntermediatePoints},

                                       (* validate extra options *)
                                       FigCheckOption[Self,IntermediatePoints,None|{FigCurvePointPattern...},FigOptions];
                                       FigCheckOption[Self,EndPositions,((_?NumericQ)|Automatic)|{((_?NumericQ)|Automatic),((_?NumericQ)|Automatic)},FigOptions];

                                       (* construct full curve specification *)
                                       UsedEndPositions={x1,x2};
                                       UsedIntermediatePoints=ResolveOption[IntermediatePoints,{None->{}},FigOptions];
                                       {p1,p2}=RealizeTransEndpoints[Self,LevelName1,LevelName2,UsedEndPositions];
                                       Curve=Join[{p1},UsedIntermediatePoints,{p2}];

                                       (* invoke basic arrow constructor code *)
                                       BasicArrow[Self][Curve];

                                     ]
                    ];


(*Methods*)


MakeAnchor[Class:Trans,Self_Object][Name:(Head|Tail|Center),Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                        FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]
                                                                   ];
MakeAnchor[Class:Trans,Self_Object][Name:Left,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                          FigCurveAnchorFromPoints[Self@GetLeftPoints[],Name,Arg]
                                                     ];
MakeAnchor[Class:Trans,Self_Object][Name:Right,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                           FigCurveAnchorFromPoints[Self@GetRightPoints[],Name,Arg]
                                                      ];


MakeBoundingBox[Class:Trans,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


(*Automatic level/transition spacing controls *)


(*State variables*)


AutoLevelCurrentLevel=Null;
AutoLevelCurrentPosition=0;
AutoLevelSpace=0;
AutoLevelBigSpace=0;


(*Initialization*)


AutoLevelInit[Position_?NumericQ,Space_?NumericQ,BigSpace_?NumericQ]  :=Module[
  {},
  AutoLevelCurrentLevel = Null;
  AutoLevelCurrentPosition = Position;
  AutoLevelSpace = Space;
  AutoLevelBigSpace = BigSpace;
                                                                        ];
DeclareFigFallThroughError[AutoLevelInit];


(*Select level*)


AutoLevel[Object[LevelName1:ObjectNamePattern[Lev]]|(LevelName1:ObjectNamePattern[Lev])]:=Module[
  {},
  AutoLevelCurrentPosition+=If[
    (AutoLevelCurrentLevel === Null),
    0,
    AutoLevelBigSpace-AutoLevelSpace
                            ];
  AutoLevelCurrentLevel =LevelName1;
                                                                                          ];
DeclareFigFallThroughError[AutoLevel];


(*Draw transition*)


AutoTrans[Object[LevelName2:ObjectNamePattern[Lev]]|(LevelName2:ObjectNamePattern[Lev]),Opts___?OptionQ]:=Module[
  {},
  Trans[AutoLevelCurrentLevel ,AutoLevelCurrentPosition,LevelName2,Automatic,Opts];
  AutoLevelCurrentPosition+=AutoLevelSpace; 
                                                                                                          ];
DeclareFigFallThroughError[AutoTrans];


(*Automatic shell substate positioning*)


Options[LevelShellPoints]={MaxPoints->Automatic,Margin->Automatic,Inset->0.75};
LevelShellPoints[
  Object[LevelName1:ObjectNamePattern[Lev]]|(LevelName1:ObjectNamePattern[Lev]),
  Points_Integer,
  Opts___?OptionQ
]:= Module[
  {
    FullOptions=Flatten[{Opts,Options[LevelShellPoints]}],
    UsedMaxPoints,BufferedMaxPoints,
    LevelObject=Object[LevelName1],
    NominalPoints,UserXWidth,UserMargins,
    UsedMargins,UsedInset,NominalCanvasXRange,CanvasY,df,FractionalInsets,CanvasInterval,CanvasXList
  },

  (* option validation *)
  FigCheckOption[LevelShellPoints,Margin,IntervalParametersPattern|Automatic,FullOptions];
  FigCheckOption[LevelShellPoints,MaxPoints,NonNegativeIntegerPattern|Automatic,FullOptions];
  FigCheckOption[LevelShellPoints,Inset,NonNegativePattern,FullOptions];

  (* retrieve level properties *)
  NominalPoints=LevelObject@GetNominalPoints[];
  UserXWidth=LevelObject@GetUserXWidth[];
  UserMargins=LevelObject@GetUserMargins[];


  (* geometry *)
  (* As fraction of original coordinate width w0:
-- left margin covers m1/w0
-- right margin covers m2/w0
-- maximum shell region (for N=BufferedMaxPoints points) covers w/w0
-- active shell region (for N'=Points points) covers w'/w0
Relations:
-- w'/w=(N'-1)/(N-1)
-- w0=m1+w+m2 => w=w0-(m1+m2)
Fractional insets to active shell region:
f1=m1/w0+(w-w')/2/w0 => f1 = m1/w0+df
f2=m2/w0+(w-w')/2/w0 => f2 = m2/w0+df
with 
df=(N-N')/(N-1)*(1-(m1+m2)/w0)
   *)
  UsedMargins=UpgradePairEqual[GetAutoOption[Margin,UserMargins,FullOptions]];
  UsedMaxPoints=GetAutoOption[MaxPoints,Points,FullOptions];
  UsedInset=(Inset/.FullOptions);
  BufferedMaxPoints =UsedMaxPoints+2*UsedInset;  (* allow one step past end in each direction *)
  NominalCanvasXRange=NominalPoints[[;;,1]];  (* obtain canvas x interval {a0,b0} of nominal level *)
  CanvasY=NominalPoints[[1,2]];  (* grab canvas y from first nominal endpoint *)
  df=(1/2)*(BufferedMaxPoints-Points)/(BufferedMaxPoints-1)*(1-Total[UsedMargins]/UserXWidth);
  (*Print[{UsedMargins,UserXWidth,UsedMargins/UserXWidth,BufferedMaxPoints,Points,df}];*)
  FractionalInsets=UsedMargins/UserXWidth+df;
  CanvasInterval=ExtendInterval[NominalCanvasXRange,-FractionalInsets,Scaled];
  (*Print[{NominalCanvasXRange,CanvasInterval}];*)

  (* generate point list *)
  CanvasXList=Rescale[Range[Points],{1,Points},CanvasInterval];
  Absolute[{#,CanvasY}]&/@CanvasXList

    ];
DeclareFigFallThroughError[LevelShellPoints];


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
