(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* ::Input::Initialization:: *)
(* :Title: FigGraphics *)
(* :Context: SciDraw` *)
(* :Summary: Graphics and image inclusion *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Copyright: Copyright FIGYEAR, Mark A. Caprio *)
(* :Package Version: FIGVERSION *)
(* :Mathematica Version: MATHVERSION *)
(* :Discussion: FIGDISCUSSION *)
(* :History: See package information file. *)


(* ::Input::Initialization:: *)
BeginPackage["SciDraw`",SciDraw`Private`$ExternalContexts];


(* ::Input::Initialization:: *)
Unprotect[Evaluate[$Context<>"*"]];


(* ::Input::Initialization:: *)
FigGraphics::usage="FIGURE OBJECT: FigGraphics[graphics] includes Mathematica graphics in a figure, drawn with respect to the current panel coordinate system.";
FigInset::usage="FIGURE OBJECT: FigInset[graphics,region] displays Mathematica graphics inset within the current panel or the given region, as it would be displayed by Show.";


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ::Input::Initialization:: *)



(* ::Input::Initialization:: *)
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


(* ::Input::Initialization:: *)
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


(* ::Input::Initialization:: *)
Constructor[Class:FigGraphics,Self_Object][g_Graphics,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
BasicGraphics[Self][g]
];


(* ::Input::Initialization:: *)
Constructor[Class:FigGraphics,Self_Object][{g_Graphics},Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
BasicGraphics[Self][g]
];


(* ::Input::Initialization:: *)
Constructor[Class:FigGraphics,Self_Object][g:(_ContourGraphics|_DensityGraphics),Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
BasicGraphics[Self][Graphics[g]]
];


(* ::Input::Initialization:: *)
MakeAnchor[Class:FigGraphics,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
Null
];
MakeBoundingBox[Class:FigGraphics,Self_Object][]:=FigMakeBoundingBoxWrapper[Class,Self,Null];


(* ::Input::Initialization:: *)
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


(* ::Input::Initialization:: *)
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


(* ::Input::Initialization:: *)
Constructor[Class:FigInset,Self_Object][(g:(_Graphics|_ContourGraphics|_DensityGraphics|_Image|_Graphics3D))|{g_Graphics},r:FigRegionPattern:All,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
Module[
{UsedCanvasRegion},

FigCheckOption[Self,RegionExtension,FigDeltaRegionPattern,FigOptions];
FigCheckOption[Self,RegionDisplacement,FigDisplacementPattern,FigOptions];

BasicInset[Self][g,r]
]
];


(* ::Input::Initialization:: *)
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


(* ::Input::Initialization:: *)
End[];


(* ::Input::Initialization:: *)
Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];