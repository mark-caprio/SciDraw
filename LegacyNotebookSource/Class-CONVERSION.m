(* ::Package:: *)

(* ::Section::Initialization:: *)
(*Header comments*)


(* ::Input::Initialization:: *)
(* :Title: Class *)
(* :Context: SciDraw` *)
(* :Summary: Class declarations *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Copyright: Copyright FIGYEAR, Mark A. Caprio *)
(* :Package Version: FIGVERSION *)
(* :Mathematica Version: MATHVERSION *)
(* :Discussion: FIGDISCUSSION *)
(* :History: See main package file. *)


(* ::Section::Initialization:: *)
(*Begin package*)


(* ::Subsection::Initialization:: *)
(*Package context definition*)


(* ::Input::Initialization:: *)
BeginPackage["SciDraw`",SciDraw`Private`$ExternalContexts];


(* ::Input::Initialization:: *)
Unprotect[Evaluate[$Context<>"*"]];


(* ::Subsection::Initialization:: *)
(*Begin private context*)


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ::Subsection::Initialization:: *)
(*Dependencies*)


(* ::Input::Initialization:: *)



(* ::Section::Initialization:: *)
(*Class declarations*)


(* ::Subsection::Initialization:: *)
(*Code structuring convention*)


(* ::Text::Initialization:: *)
(*This file contains "up front" class definitions for classes which will be used across files within the SciDraw package. This ensures that the symbols for the methods are established in the correct namespace before their first use.  It also ensures that the option inheritance is defined before the DefineOptions call for any subsequent daughter class definition refers to the options.*)
(**)
(*Convention: Figure object classes which are *not* to be referenced across files within SciDraw can safely be defined within their own module file.  Therefore they *should* be defined in their own module file, not here!*)


(* ::Subsection::Initialization:: *)
(*Set defaults for option resolution*)


(* ::Text::Initialization:: *)
(*Option resolution is a major performance factor*)


(* ::Input::Initialization:: *)
(*SetOptions[DefineOptions,{TrapUninheritableOptions\[Rule]True,TrapUnknownOptions\[Rule]True}];*)


(* ::Subsection::Initialization:: *)
(*FigAnchor*)


(* ::Input::Initialization:: *)
DeclareClass[FigAnchor,{"Point","Angle","Offset"},{},Replace->True];


(* ::Subsection::Initialization:: *)
(*FigElement*)


(* ::Input::Initialization:: *)
DeclareClass[FigElement,{"Primatives","SortKey"},{}];
DeclareClass[FigLineElement,FigElement,{},{}];
DeclareClass[FigPolygonElement,FigElement,{},{}];
DeclareClass[FigPointElement,FigElement,{},{}];
DeclareClass[FigVerbatimElement,FigElement,{},{}];
DeclareClass[FigTextElement,FigElement,{"InitialPoint","Center","Radius","Pivot","Rotation"},{}];
DeclareClass[FigTextFrameElement,FigElement,{},{}];


(* ::Input::Initialization:: *)
Options[FigCompositeElement]={Show->True,Clip->False,Rasterize->False,ImageResolution->300};
DeclareClass[FigCompositeElement,FigElement,{},{}];


(* ::Subsection::Initialization:: *)
(*FigObject*)


(* ::Input::Initialization:: *)
(*DeclareClass[FigObject,{"OptionValues"},{"MakeAnchor","MakeBoundingBox"}];*)
DeclareClass[FigObject,{},{"MakeAnchor","MakeBoundingBox"}];


(* ::Input::Initialization:: *)
DefineOptions[FigObject,
{
(* overall appearance *)
Show->True,Color->Black,Opacity->None,
Directives->{},
Layer->Automatic,

(* outline *)
ShowLine->Default, 
LineColor->Default,LineOpacity->Default,
LineThickness->1,LineDashing->None,LineCapForm->None,LineJoinForm->{"Miter",3.25},
LineDirectives->{},

(* fill *)
ShowFill->Default, FillColor->Default,FillOpacity->Default,
FillDirectives->{},

(* point *)
ShowPoint->Default, PointColor->Default,PointOpacity->Default,
PointSize->3,
PointDirectives->{},

(* text appearance *)
ShowText->Default,TextColor->Default,TextOpacity->Default,
FontFamily->"Times",FontSize->16,FontWeight->Plain,FontSlant->Plain,FontTracking ->Plain,FontVariations->{},
TextStyleOptions->{},  (* possibly including directives, though interaction with options unpredictable *)

(* text background and frame *)
TextBackground->None,
TextFrame->False,TextFrameColor->Default,TextFrameOpacity->Default,
TextFrameThickness->1,TextFrameDashing->None,
TextRoundingRadius->None,
TextMargin->None,TextPadding->False,
TextFrameDirectives->{},

(* text callout *)
TextCallout->False, 
TextCalloutColor->Default,TextCalloutOpacity->Default,
TextCalloutThickness->1,TextCalloutDashing->None,TextCalloutCapForm->None,TextCalloutJoinForm->{"Miter",3.25},
TextCalloutDirectives->{},

(* text positioning *)
TextOffset->Automatic,TextOrientation->Automatic,TextRectify->True,TextBaseBuffer->Automatic,TextBuffer->None,TextNudge->None,

(* style *)
Style->None,

(* accessories *)
Prolog:>None,Epilog:>None,

(* diagnostic *)
Debug->False
(*PrintTiming\[Rule]False*)

}
];


(* ::Subsection::Initialization:: *)
(*FigStyle*)


(* ::Text::Initialization:: *)
(*Figure styles will often be defined at global level (outside figures), and users will typically want to fiddle with settings, so clobbering of instances should be allowed (Replace->True).*)


(* ::Input::Initialization:: *)
(*
DeclareClass[FigStyle,{"StyleList"},{"SymbolOptions"},Replace\[Rule]True];
DefineOptions[FigStyle,
{
BaseStyle\[Rule]None,
Debug\[Rule]False
}
];
*)


(* ::Subsection::Initialization:: *)
(*FigWindow*)


(* ::Text::Initialization:: *)
(*Object declarations are out outermost scope to ensure mutators/accessors are in this scope*)


(* ::Text::Initialization:: *)
(*Data:*)
(*	TFunction -- transformation function from user coordinates to canvas coordinates*)
(*	PreTFunction -- pretransformation function to be applied in transformation returned by TFunction[]*)
(*		meant for origin shift, would have to be revisited for effect on DeltaTFunction otherwise,*)
(*		does not affect extraction of canvas region from region in user coordinates*)
(*	Region -- region in user coordinates*)
(*Methods:*)
(*	Region[] access the region in user coordinates.*)
(*	CanvasRegion[] converts this region to canvas coordinates, using TFunction.*)
(*	TFunction[] gives the transformation function from user coordinates to canvas coordinates.*)
(*	DeltaTFunction[] gives the homogeneous part of the transformation from user coordinates to canvas coordinates.*)
(*	ScaledTFunction[] gives the transformation function from scaled coordinates to canvas coordinates.*)
(*	ScaledDeltaTFunction[] gives the homogeneous part of the transformation function from scaled coordinates to canvas coordinates.*)


(* ::Input::Initialization:: *)
DeclareClass[
FigWindow,
{
"TFunction",
"Region"
},
{
"UserRegion",
"CanvasRegion",
"TFunction",
"InverseTFunction",
"DeltaTFunction",
"ScaledTFunction",
"ScaledDeltaTFunction"
}
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
