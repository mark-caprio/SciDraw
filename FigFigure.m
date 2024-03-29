(* ::Package:: *)

(*Header comments*)


(* :Title: FigFigure *)
(* :Context: SciDraw` *)
(* :Summary: Figure rendering function *)
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


Needs["Units`"];


(*Common value accessors*)


(*Current window retrieval*)


CurrentWindow[]:=$CurrentWindow;


(*Current background retrieval*)


CurrentBackground[]:=$CurrentBackground;


(*Figure display*)


(*Options*)


Options[Figure]={

  (* plot region control *)
  CanvasSize->{6,4.25},
  CanvasMargin->1,
  CanvasUnits->Inch,

  (* guide frame control *)
  CanvasFrame->False,

  (* rendering properties *)
  Background->None,
  DisplayFunction:>$DisplayFunction,

  (* exporting control *)
  Export->False,
  ExportDirectory->Automatic,
  ExportFileName->"figure-*.eps",
  ExportFormat->"EPS",
  ExportOptions->{},
  ExportStamp->Hold[DateString[{"YearShort","Month","Day","-","Hour24","Minute","Second","-","Millisecond"}]],

  (* style control *)
  Style->None,

  (* debugging options *)
  Debug->False,
  PrintTiming->False  (* profiling for Figure function itself *)

};


(*Error catcher*)


(*CatchError[body] evaluates body and returns True if an Abort was thrown.*)


SetAttributes[CatchError,HoldAll];
CatchError[Body_]:=(CatchAbort[Body,$ErrorFlag]===$ErrorFlag);


(*Ad hoc object scoping construct*)


(*Until ScopeObjects is optimized or object framework is restructured completely...*)


SetAttributes[CleanUpObjects,HoldAll];


CleanUpObjects[Body_]:=
  Internal`WithLocalSettings[
    (* initialization code *)
    ClearObjects[],
    (* body code*)
    Body,
    (* cleanup code *)
    ClearObjects[]
  ];


(*Figure display function*)


Figure::notreentrant="Figure is not reentrant.  It cannot be called within the argument expression given to Figure.";


SetAttributes[Figure,{HoldFirst}];
Figure[Body_,Opts___?OptionQ]:=Module[
  {
    FullOptions=Flatten[{Opts,Options[Figure]}],
    CanvasUnitSize,ErrorStatus,GraphicsOptions,PrimativeList,CanvasFramePrimatives,FigureGraphics,
    StyleOptions,ReturnGraphics,UsedExportFilename,UsedExportDirectory
  },

  (* flag entry into figure, for error handling mechanism *)
  If[$InFigure,
     Message[Figure::notreentrant];
     Return[]
  ];

  (* check for and impose Style specification *)
  ErrorStatus=CatchError[
    FigCheckOption[Figure,Style,StyleSpecifierPattern,FullOptions];
              ];
  If[ErrorStatus,Return[]];

  WithStyle[
    (Style/.FullOptions),
    (* re-realize options options in context of imposed style *)
    FullOptions=Flatten[{Opts,Options[Figure]}];

    (* check options *)
    ErrorStatus=CatchError[
      FigCheckOption[Figure,CanvasSize,{PositivePattern,PositivePattern},FullOptions];
      FigCheckOption[Figure,CanvasMargin,NonNegativeRangeParametersPattern,FullOptions];
      FigCheckOption[Figure,CanvasUnits,(s_Symbol)/;NumericQ[Convert[s,Point]/Point],FullOptions];
      FigCheckOption[Figure,CanvasFrame,LogicalPattern,FullOptions];
      FigCheckOption[Figure,Background,None|ColorDirectivePattern,FullOptions];
      FigCheckOption[Figure,Style,StyleSpecifierPattern,FullOptions];
      FigCheckOption[Figure,Debug,LogicalPattern,FullOptions];
      FigCheckOption[Figure,Export,LogicalPattern,FullOptions];
      FigCheckOption[Figure,ExportDirectory,Automatic|(_String),FullOptions];
      FigCheckOption[Figure,ExportFileName,(_String),FullOptions];
      FigCheckOption[Figure,ExportFormat,(_String),FullOptions];
      FigCheckOption[Figure,ExportOptions,{___?OptionsPattern},FullOptions];
      FigCheckOption[Figure,ExportStamp,_,FullOptions];

                ];
    If[ErrorStatus,Return[]];

    (* generate figure *)
    (* scoping: (1) variables with "global" access, (2) object creation, (3) figure object options, (4) set current coordinate system *)
    (* scoping constructs must trap abort signals for graceful cleanup and exit *)
    Block[
      {
        (* page geometry *)
        $CanvasBaseRange,$CanvasFullRange,$CanvasDimensions,$TickScaleFactor,$CanvasWindow,
        (* background *)
        $CanvasBackground,$CurrentBackground,
        (* styling *)
        (* in-figure flag *)
        $InFigure=True,
        (* graphics accumulator *)
        $GraphicalElementList={}
      },
      PrintTiming[
        (* AD HOC -- Since cleanup phase of ScopeObjects is slow, and no external code is expected to use MathObjects, for now just wipe out all object data after figure. *)
        CleanUpObjects[
          (* set up page coordinate system *)
          CanvasUnitSize=Convert[(CanvasUnits/.FullOptions),Point]/Point;
          $CanvasBaseRange={{0,1},{0,1}}*(CanvasSize/.FullOptions)*CanvasUnitSize;
          $CanvasFullRange=ExtendRegion[$CanvasBaseRange,UpgradeRangeParameters[(CanvasMargin/.FullOptions)]*CanvasUnitSize,Absolute];
          $CanvasDimensions=-Subtract@@@$CanvasFullRange;
          $TickScaleFactor=(Plus@@$CanvasDimensions/2);  (* mean of page dimensions *)
          $CanvasWindow=FigWindow[$CanvasBaseRange];

          (* resolve and record background color *)
          $CanvasBackground=Switch[(Background/.FullOptions),None,White,_,(Background/.FullOptions)];
          $CurrentBackground=$CanvasBackground;

          (* evaluate body *)
          If[
            (Debug/.FullOptions),
            Print["**************** Figure body ****************"]
          ];
          PrintTiming[
            ScopeOptions[ScopeOptionOverrides[
              Block[
                {$CurrentWindow=$CanvasWindow},

                (* set up debugging for figure objects *)
                If[
                  (Debug/.FullOptions),
                  SetOptions[FigObject,Debug->True]
                ];


                (* trap aborts in body *)
                ErrorStatus=PrintTiming[
                  CatchError[Body],
                  Label->">>> Body proper",Print->(PrintTiming/.FullOptions)
                            ];

              ] (* Block $CurrentWindow *)
                         ]],
            Label->">> Body with option scoping",Print->(PrintTiming/.FullOptions)
          ]; (* ScopeOptions, ScopeOptionOverrides *)

          (* collect graphical element list *)

          If[ErrorStatus,
             If[(Debug/.FullOptions),Print["Body evaluated with error."]];
             Return[]
          ];

          (* process graphical element object list *)
          (* diagnostic output *)
          If[
            (Debug/.FullOptions),
            If[(Debug/.FullOptions),Print["**************** Graphical elements ****************"]];
            Print["Graphical elements: ",Length[$GraphicalElementList]];
            Print["Unsorted: ",First/@$GraphicalElementList];
          ];
          (* retrieve primatives *)
          PrimativeList=PrintTiming[
            FigAssemblePrimatives[$GraphicalElementList],
            Label->">> Assembling primitives",
            Print->(PrintTiming/.FullOptions)
                        ];
          If[
            (Debug/.FullOptions),
            Print["Primatives: ",PrimativeList//InputForm]
          ];

          (* prepare guide frame *) 
          CanvasFramePrimatives=If[
            (CanvasFrame/.FullOptions),
            {
              (* active canvas frame *)
              {EdgeForm[{Gray,AbsoluteThickness[2]}],FaceForm[],Rectangle@@Transpose[$CanvasBaseRange]},
              (* full canvas frame *)
              (* pulled inward by 0.5 point to prevent the frame line from disappearing *)
              {EdgeForm[{Gray,Dashed}],FaceForm[],Rectangle@@Transpose[ExtendRegion[$CanvasFullRange,UpgradeRangeParameters[-0.5],Absolute]]}
            },
            {}
                                ];

          (* prepare graphics options *)
          GraphicsOptions={
            PlotRange ->$CanvasFullRange,
            ImageSize->$CanvasDimensions,
            AspectRatio->$CanvasDimensions[[2]]/$CanvasDimensions[[1]],
            Frame-> False,
            Background->(Background/.FullOptions),
            DisplayFunction->(DisplayFunction/.FullOptions)
          };
          (* diagnostic output *)
          If[
            (Debug/.FullOptions),
            Print["Graphics options: ",GraphicsOptions]
          ];

          (* return graphics object *)
          ReturnGraphics=Graphics[{PrimativeList,CanvasFramePrimatives},GraphicsOptions]

        ],
        Label->"> Body with object scoping",Print->(PrintTiming/.FullOptions)
      ] (* ScopeObjects -- need to wait until primatives recovered *)
    ] ;(* Block *)


    (* handle automatic export *)
    If[
      (Export/.FullOptions),
      UsedExportDirectory=(ExportDirectory/.FullOptions);
      UsedExportFilename=StringReplace[(ExportFileName/.FullOptions),"*"->ReleaseHold[(ExportStamp/.FullOptions)]];
      If[
        UsedExportDirectory=!=Automatic,
        UsedExportFilename=FileNameJoin[{UsedExportDirectory,UsedExportFilename}]
      ];
      Print@Export[UsedExportFilename,ReturnGraphics,(ExportFormat/.FullOptions),(ExportOptions/.FullOptions)]
    ];

    (* return graphics *)
    ReturnGraphics

  ] (* WithStyle *)
                               ];


(*Syntax fallthrough*)


DeclareFigFallThroughError[Figure];


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
EndPackage[];
