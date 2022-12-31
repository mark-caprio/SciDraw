(* ::Package:: *)

(*Header comments*)


(* :Title: Information *)
(* :Context: SciDraw` *)
(* :Summary: Package version information and history comments *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Copyright: Copyright 2015, Mark A. Caprio *)
(* :Package Version: See $FigVersionString below. *)
(* :Mathematica Version: 10.0 *)
(* :Discussion:  

Use of the software is subject to the CPCPL Non\[Dash]Profit Use Licence Agreement (http://cpc.cs.qub.ac.uk/licence/licence.html).

Updates and further information may be obtained through the SciDraw home page at scidraw.nd.edu.  

If you modify the software, please change $FigVersionString below, and thus the message displayed when the package is loaded, to clearly indicate that the package is a modified version. 

 *)
(* :History:  Successor to LevelScheme package, for which early ancestor code was begun July 1999.  Development of SciDraw begun May 2011.
 *)


(*Begin package*)


(*Package context definition*)


BeginPackage["SciDraw`"];


Unprotect[Evaluate[$Context<>"*"]];


(*Begin private context*)


Begin["`Private`"];


(*Dependencies*)





(*Information*)


(*External context list*)


(*These will be loaded in the BeginPackage statement of all SciDraw package files.*)


$ExternalContexts={"Units`","MathObject`","MathObject`Methods`","MathObject`InstanceData`","InheritOptions`","NamedColors`","CustomTicks`","BlockOptions`","Profile`","StyleOptions`"};


(*Version information*)


$FigVersionNumber=0.0;
$FigReleaseNumber=6;
$FigVersionString="0.0.7 (March 28, 2015)";
$FigHomePageURL="http://scidraw.nd.edu";


(*For generic splash for use in preparing documentation...*)


(*$FigVersionString="x.x.x (January 1, 20xx)";*)


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
