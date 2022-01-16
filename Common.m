(* ::Package:: *)

(*Header comments*)


(* :Title: Common *)
(* :Context: SciDraw` *)
(* :Summary: Common global constants and flag variables *)
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


(*Begin private context*)


Begin["`Private`"];


(*Dependencies*)





(*Global constant initialization*)


(*These global constants are in the private context.*)
(**)
(*Note: These variables are meant to be accessible in the core infrastructure subpackages, but they are not directly needed by subpackages which define figure object classes.*)


(*Drawing layers*)


$FigBackgroundLayer=0;
$FigDrawingLayer=1;
$FigBlankingLayer=2;
$FigTextLayer=3;


(*Low-level graphics adjustment*)


(*Value used for TextBaseBuffer if left as Automatic*)


$FigTextBaseBuffer=1;


(*Option override rules*)


$FigOptionRules={};


(*Flag to prevent evaluation of figure objects outside of a figure*)


$InFigure=False;


(*Figure-wide base style*)


$FigBaseStyle;


(*Figure element accumulator*)


$GraphicalElementList;


(*Page dimensions*)


$CanvasBaseRange;
$CanvasFullRange;
$CanvasDimensions;
$TickScaleFactor;


(*Windows*)


(*Object references to canvas and current windows.*)


$CanvasWindow;
$CurrentWindow;


(*Multipanel*)


(*Object reference to current multipanel environment*)


$Multipanel=None;


(*Background color*)


$CanvasBackground;
$CurrentBackground;


(*Profiling and debugging control*)


$PrintTiming=False;
$DebugOptionOverrides=False;


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
