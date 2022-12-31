(* ::Package:: *)

(*Header comments*)


(* :Title: FigGraphics *)
(* :Context: SciDraw` *)
(* :Summary: Graphics and image inclusion *)
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


FigGraphics::usage="FIGURE OBJECT: FigGraphics[graphics] includes Mathematica graphics in a figure, drawn with respect to the current panel coordinate system.";
FigInset::usage="FIGURE OBJECT: FigInset[graphics,region] displays Mathematica graphics inset within the current panel or the given region, as it would be displayed by Show.";


(*Other usage*)


(*Begin private context*)


Begin["`Private`"];


(*Dependencies*)





(*FigGraphics*)


(*Object declaration*)


DeclareFigClass[
  FigGraphics,
  {},
  {},
  {}
];
DefineFigClassOptions[
  FigGraphics,
  {
  }
];


(*Constructor*)


BasicGraphics[Self_Object][g_Graphics]:=Module[
  {
    GraphicsPrimatives,TransformedPrimatives
  },

  (* transform graphics to canvas coordinates *)
  (* note: quietly handles null Graphics[], since Mathematica provides nonstandard evaluation First[Graphics[]] \[Rule] {} *)
  (* note: also quietly handles nonlist first argument to Graphics[], since GeometricTransformation accepts an unwrapped single primative for its primatives argument *)
  GraphicsPrimatives=First[g];
  TransformedPrimatives={GeometricTransformation[GraphicsPrimatives,CurrentWindow[]@TFunction[]]};

  If[
    (Debug/.FigOptions),
    Print["Transformed primatives: ",TransformedPrimatives]
  ];
  (* emit graphics *)
  FigVerbatimElement[TransformedPrimatives,FilterRules[FigOptions,{Show,Layer}]];

                                        ];


(*Standard graphics*)


Constructor[Class:FigGraphics,Self_Object][g_Graphics,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                         BasicGraphics[Self][g]
                                                                        ];


(*Allow single graphics to be enclosed in List*)
(*	to gracefully accept Import result for PDF*)


Constructor[Class:FigGraphics,Self_Object][{g_Graphics},Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                           BasicGraphics[Self][g]
                                                                          ];


(*Cast ContourGraphics or DensityGraphics to Graphics*)


Constructor[Class:FigGraphics,Self_Object][g:(_ContourGraphics|_DensityGraphics),Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                                    BasicGraphics[Self][Graphics[g]]
                                                                                                   ];


(*Methods*)


MakeAnchor[Class:FigGraphics,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                            Null
                                                       ];
MakeBoundingBox[Class:FigGraphics,Self_Object][]:=FigMakeBoundingBoxWrapper[Class,Self,Null];


(*FigInset*)


(*Object declaration*)


DeclareFigClass[
  FigInset,
  {"Center","Radius"},
  {},
  {}
];
DefineFigClassOptions[
  FigInset,
  {
    RegionExtension->None,
    RegionDisplacement->None
  }
];


(*Constructor*)


BasicInset[Self_Object][g_,r_]:=Module[
  {
    TransformedPrimatives,CanvasRegion,CanvasCenter,CanvasSize,CanvasRadius
  },

  (* force graphics into inset *)
  CanvasRegion=FigResolveRegion@AdjustRegion[r,FilterRules[FigOptions,{RegionExtension,RegionDisplacement}]];
  CanvasCenter=Mean/@CanvasRegion;
  CanvasSize=-Subtract@@@CanvasRegion;
  TransformedPrimatives={Inset[g,CanvasCenter,Center,CanvasSize]};

  If[
    (Debug/.FigOptions),
    Print["Given region ",r,"Canvas region: ",CanvasRegion]
  ];

  (* save box information *)
  CanvasRadius=CanvasSize/2;
  Self@SetCenter[CanvasCenter];
  Self@SetRadius[CanvasRadius];

  (* emit graphics *)
  FigVerbatimElement[TransformedPrimatives,FilterRules[FigOptions,{Show,Layer}]];

                                ];


(*All forms of directly insettable graphics*)
(*	also allow single graphics to be enclosed in List to gracefully accept Import result for PDF*)
(**)
(*Problem:*)
(*(g:(_Graphics|_ContourGraphics|_DensityGraphics|_Image|_Graphics3D))|{g_Graphics}*)
(*excludes new "graphics" types, such as PointLegend,*)
(*so allow all objects to be passed through, but with special case trap for {_Graphics}.  Ugly since is special case trap and relaxes argument type checking.*)


Constructor[Class:FigInset,Self_Object][(g:(_Graphics|_ContourGraphics|_DensityGraphics|_Image|_Graphics3D))|{g:(_Graphics|_Image)},r:FigRegionPattern:All,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                                                                                                    Module[
                                                                                                                                                                                      {UsedCanvasRegion},

                                                                                                                                                                                      FigCheckOption[Self,RegionExtension,FigDeltaRegionPattern,FigOptions];
                                                                                                                                                                                      FigCheckOption[Self,RegionDisplacement,FigDisplacementPattern,FigOptions];

                                                                                                                                                                                      BasicInset[Self][g,r]
                                                                                                                                                                                    ]
                                                                                                                                                                   ];


(*Methods*)


MakeAnchor[Class:FigInset,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                         FigRectangleAnchor[
                                                                           Self@GetCenter[],Self@GetRadius[],{0,0},0,
                                                                           Name,Arg
                                                                         ]
                                                    ];
MakeBoundingBox[Class:FigInset,Self_Object][]:=FigMakeBoundingBoxWrapper[Class,Self,
                                                                         FigRectangleBoundingBox[
                                                                           Self@GetCenter[],Self@GetRadius[],Self@GetPivot[],Self@GetRotation[]
                                                                         ]
                                               ];


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
