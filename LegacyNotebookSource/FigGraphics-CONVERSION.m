(* ::Package:: *)

(* ::Section::Initialization:: *)
(*Header comments*)


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
FigGraphics::usage="FIGURE OBJECT: FigGraphics[graphics] includes Mathematica graphics in a figure, drawn with respect to the current panel coordinate system.";
FigInset::usage="FIGURE OBJECT: FigInset[graphics,region] displays Mathematica graphics inset within the current panel or the given region, as it would be displayed by Show.";


(* ::Subsection::Initialization:: *)
(*Other usage*)


(* ::Subsection::Initialization:: *)
(*Begin private context*)


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ::Subsection::Initialization:: *)
(*Dependencies*)


(* ::Input::Initialization:: *)



(* ::Section::Initialization:: *)
(*FigGraphics*)


(* ::Subsection::Initialization:: *)
(*Object declaration*)


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


(* ::Subsection::Initialization:: *)
(*Constructor*)


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


(* ::Text::Initialization:: *)
(*Standard graphics*)


(* ::Input::Initialization:: *)
Constructor[Class:FigGraphics,Self_Object][g_Graphics,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
BasicGraphics[Self][g]
];


(* ::Text::Initialization:: *)
(*Allow single graphics to be enclosed in List*)
(*	to gracefully accept Import result for PDF*)


(* ::Input::Initialization:: *)
Constructor[Class:FigGraphics,Self_Object][{g_Graphics},Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
BasicGraphics[Self][g]
];


(* ::Text::Initialization:: *)
(*Cast ContourGraphics or DensityGraphics to Graphics*)


(* ::Input::Initialization:: *)
Constructor[Class:FigGraphics,Self_Object][g:(_ContourGraphics|_DensityGraphics),Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
BasicGraphics[Self][Graphics[g]]
];


(* ::Subsection::Initialization:: *)
(*Methods*)


(* ::Input::Initialization:: *)
MakeAnchor[Class:FigGraphics,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
Null
];
MakeBoundingBox[Class:FigGraphics,Self_Object][]:=FigMakeBoundingBoxWrapper[Class,Self,Null];


(* ::Section::Initialization:: *)
(*FigInset*)


(* ::Subsection::Initialization:: *)
(*Object declaration*)


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


(* ::Subsection::Initialization:: *)
(*Constructor*)


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


(* ::Text::Initialization:: *)
(*All forms of directly insettable graphics*)
(*	also allow single graphics to be enclosed in List to gracefully accept Import result for PDF*)
(**)
(*Problem:*)
(*(g:(_Graphics|_ContourGraphics|_DensityGraphics|_Image|_Graphics3D))|{g_Graphics}*)
(*excludes new "graphics" types, such as PointLegend,*)
(*so allow all objects to be passed through, but with special case trap for {_Graphics}.  Ugly since is special case trap and relaxes argument type checking.*)


(* ::Input::Initialization:: *)
Constructor[Class:FigInset,Self_Object][(g:(_Graphics|_ContourGraphics|_DensityGraphics|_Image|_Graphics3D))|{g_Graphics},r:FigRegionPattern:All,Opts___?OptionQ]:=FigObjectWrapper[Class,Self,{Opts},
Module[
{UsedCanvasRegion},

FigCheckOption[Self,RegionExtension,FigDeltaRegionPattern,FigOptions];
FigCheckOption[Self,RegionDisplacement,FigDisplacementPattern,FigOptions];

BasicInset[Self][g,r]
]
];


(* ::Subsection::Initialization:: *)
(*Methods*)


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
