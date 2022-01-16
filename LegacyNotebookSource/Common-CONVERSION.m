(* ::Package:: *)

(* ::Section::Initialization:: *)
(*Header comments*)


(* ::Input::Initialization:: *)
(* :Title: Common *)
(* :Context: SciDraw` *)
(* :Summary: Common global constants and flag variables *)
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
(*Begin private context*)


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ::Subsection::Initialization:: *)
(*Dependencies*)


(* ::Input::Initialization:: *)



(* ::Section::Initialization:: *)
(*Global constant initialization*)


(* ::Text::Initialization:: *)
(*These global constants are in the private context.*)
(**)
(*Note: These variables are meant to be accessible in the core infrastructure subpackages, but they are not directly needed by subpackages which define figure object classes.*)


(* ::Subsection::Initialization:: *)
(*Drawing layers*)


(* ::Input::Initialization:: *)
$FigBackgroundLayer=0;
$FigDrawingLayer=1;
$FigBlankingLayer=2;
$FigTextLayer=3;


(* ::Subsection::Initialization:: *)
(*Low-level graphics adjustment*)


(* ::Text::Initialization:: *)
(*Value used for TextBaseBuffer if left as Automatic*)


(* ::Input::Initialization:: *)
$FigTextBaseBuffer=1;


(* ::Subsection::Initialization:: *)
(*Option override rules*)


(* ::Input::Initialization:: *)
$FigOptionRules={};


(* ::Subsection::Initialization:: *)
(*Flag to prevent evaluation of figure objects outside of a figure*)


(* ::Input::Initialization:: *)
$InFigure=False;


(* ::Subsection::Initialization:: *)
(*Figure-wide base style*)


(* ::Input::Initialization:: *)
$FigBaseStyle;


(* ::Subsection::Initialization:: *)
(*Figure element accumulator*)


(* ::Input::Initialization:: *)
$GraphicalElementList;


(* ::Subsection::Initialization:: *)
(*Page dimensions*)


(* ::Input::Initialization:: *)
$CanvasBaseRange;
$CanvasFullRange;
$CanvasDimensions;
$TickScaleFactor;


(* ::Subsection::Initialization:: *)
(*Windows*)


(* ::Text::Initialization:: *)
(*Object references to canvas and current windows.*)


(* ::Input::Initialization:: *)
$CanvasWindow;
$CurrentWindow;


(* ::Subsection::Initialization:: *)
(*Multipanel*)


(* ::Text::Initialization:: *)
(*Object reference to current multipanel environment*)


(* ::Input::Initialization:: *)
$Multipanel=None;


(* ::Subsection::Initialization:: *)
(*Background color*)


(* ::Input::Initialization:: *)
$CanvasBackground;
$CurrentBackground;


(* ::Subsection::Initialization:: *)
(*Profiling and debugging control*)


(* ::Input::Initialization:: *)
$PrintTiming=False;
$DebugOptionOverrides=False;


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
