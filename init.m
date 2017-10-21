(* ::Package:: *)

(* first establish all shared symbols in proper contexts *)
(* note that external packages listed in $ExternalContexts defined in Information.m will 
be loaded automatically through BeginPackage *)
Get["SciDraw`Information`"];  (* version information and external context list *)
Get["SciDraw`Usage`"];  (* public symbols *)
Get["SciDraw`Class`"];  (* class method names and option inheritance definitions *)
Get["SciDraw`Common`"];  (* private shared variables *)

(* then load core module files which define patterns *)
(* these must be defined before they can be used on the LHS of assignments in 
in other module files *)
Get["SciDraw`FigArgument`"];
Get["SciDraw`FigGeometry`"];

(* then load remaining core module files in arbitrary order *)
Get["SciDraw`FigAnchor`"];
Get["SciDraw`FigElement`"];
Get["SciDraw`FigError`"];
Get["SciDraw`FigFigure`"];
Get["SciDraw`FigObject`"];
(*Get["SciDraw`FigStyle`"];*)
Get["SciDraw`FigText`"];
Get["SciDraw`FigWindow`"];

(* then load figure object module files in (almost) arbitrary order *)
Get["SciDraw`FigData`"]
Get["SciDraw`FigLabel`"];
Get["SciDraw`FigPanel`"];
Get["SciDraw`FigShape`"];
Get["SciDraw`FigArrow`"];
Get["SciDraw`FigScheme`"]; (* needs SciDraw`FigArrow` for inheritance*)
Get["SciDraw`FigGraphics`"]

(* optimization testbeds *)
Get["SciDraw`FigProfile`"]

(* finally output splash panel *)
Get["SciDraw`FigSplash`"];
