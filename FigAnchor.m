(* ::Package:: *)

(*Header comments*)


(* :Title: FigAnchor *)
(* :Context: SciDraw` *)
(* :Summary: Declares the figure anchor object class *)
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





(*FigAnchor*)


(*Constructors*)


(* From coordinate point -- FigAnchor[{x,y}|etc.] with optional arguments FigAnchor[{x,y}|etc.,{xo,yo},theta]*)
(*	returns anchor with point {x,y}, text offset {xo,yo} (default {0,0}), and tangent angle theta (default 0).*)


(* DEPRECATED: Direct access to FigAnchor is deprecated in favor of the wrapper functions Anchor and GetAnchor, in anticipation to revisions to the SciDraw internal representation of anchors. *)


Constructor[Class:FigAnchor,Self_Object][p:FigCoordinatePointPattern,Offset:FigOffsetPattern:Center,theta:(_?NumericQ):0]:=Module[
  {},
  Self@SetPoint[FigResolvePoint[p]];
  Self@SetOffset[FigResolveOffset[Offset]];
  Self@SetAngle[theta];
                                                                                                                           ];


(*Copy from another anchor -- FigAnchor[anchorobject|anchorname] with optional arguments FigAnchor[anchorobject|anchorname,{xo,yo},theta]*)


(*	anchorobject  -- which is how it would be returned from an internal calculation*)
(*	anchorname -- which is how the user would specify it*)
(*	*)
(*	returns anchor with canvas point taken from given anchor, text offset {xo,yo} (default from given anchor), and tangent angle theta (default from given anchor).*)


Constructor[Class:FigAnchor,Self_Object][
  Object[n:ObjectNamePattern[FigAnchor]]|(n:ObjectNamePattern[FigAnchor])
                                        ]:=Module[
                                          {},
                                          SetObjectData[Self,Object[n]];
                                           ];


Constructor[Class:FigAnchor,Self_Object][
  Object[n:ObjectNamePattern[FigAnchor]]|(n:ObjectNamePattern[FigAnchor]),
  Offset:FigOffsetPattern:Center
                                        ]:=Module[
                                          {},
                                          SetObjectData[Self,Object[n]];
                                          Self@SetOffset[FigResolveOffset[Offset]];
                                           ];


Constructor[Class:FigAnchor,Self_Object][
  Object[n:ObjectNamePattern[FigAnchor]]|(n:ObjectNamePattern[FigAnchor]),
  Offset:FigOffsetPattern,theta:(_?NumericQ)
                                        ]:=Module[
                                          {},
                                          SetObjectData[Self,Object[n]];
                                          Self@SetOffset[FigResolveOffset[Offset]];
                                          Self@SetAngle[theta];
                                           ];


(* 
Matching fails when optional arguments omitted (under Mathematica 8):
Constructor[Class:FigAnchor,Self_Object][
Object[n:ObjectNamePattern[FigAnchor]]|(n:ObjectNamePattern[FigAnchor]),
Offset:{_?NumericQ,_?NumericQ}:Automatic,theta:(_?NumericQ):Automatic
]:=Module[
{},
Self@SetPoint[Object[n]@GetPoint[]];
Self@SetOffset[ReplaceSequential[Offset,{Automatic:>Object[n]@GetOffset[]}]];
Self@SetAngle[ReplaceSequential[theta,{Automatic:>Object[n]@GetAngle[]}]];
];
 *)


(*From figure object MakeAnchor method -- FigAnchor[objectname,arg]*)


General::figmakeanchor="Unable to resolve anchor specification `1`.\n(Diagnostic information: `2`)";


 Constructor[Class:FigAnchor,Self_Object][p:(n:ObjectNamePattern[FigObject]),Name_,Arg_:None]:=Module[
   {TemporaryAnchor},

   TemporaryAnchor=Object[n]@MakeAnchor[Name,Arg];
   (* verify success of MakeAnchor call *)
   If[
     Head[TemporaryAnchor]=!=Object,
     FigError[Self,"figmakeanchor",p,TemporaryAnchor]
   ];

   (* copy temporary anchor to self *)
   SetObjectData[Self,TemporaryAnchor];
                                                                                               ];


(* advanced anchor wrapper -- UNDER CONSTRUCTION *)
(*
Constructor[Class:FigAnchor,Self_Object][p:(n:ObjectNamePattern[FigObject]),Name_,Opts___?OptionsPattern]:=Module[
{OptionsList=Flatten[{Opts}],TemporaryAnchor},

TemporaryAnchor=Object[n]@MakeAnchor[Name,OptionsList];
(* verify success of MakeAnchor call *)
If[
Head[TemporaryAnchor]=!=Object,
FigError[Self,"figmakeanchor",p,TemporaryAnchor]
];

(* copy temporary anchor to self *)
SetObjectData[Self,TemporaryAnchor];
];
 *)


(*Set hook so that error in creation (duplicate name or syntax error) triggers Abort.*)
(*Note: Use symbol Figanchor in error message rather than Self, since no Self was created.*)


OnCreationFailure[FigAnchor,Self_Object][___]:=FigError[FigAnchor,"nocreate"];


 (*Anchor interface functions*)


 (*Anchor*)


 (* From coordinate point -- Anchor[{x,y}|etc.] with optional arguments Anchor[{x,y}|etc.,{xo,yo},theta]*)
 (*	returns anchor with point {x,y}, text offset {xo,yo} (default {0,0}), and tangent angle theta (default 0).*)


 Anchor[p:FigCoordinatePointPattern,Offset:FigOffsetPattern:Center,theta:(_?NumericQ):0]:=Module[
   {CanvasPoint,UsedOffset,UsedAngle},

   FigCheckInFigure[Anchor];

   (* pass through *)
   FigAnchor[p,Offset,theta]
                                                                                          ];


 (*Copy from another anchor -- Anchor[anchorobject|anchorname] with optional arguments Anchor[anchorobject|anchorname,{xo,yo},theta]*)


 (*	anchorobject  -- which is how it would be returned from an internal calculation*)
 (*	anchorname -- which is how the user would specify it*)
 (*	*)
 (*	returns anchor with canvas point taken from given anchor, text offset {xo,yo} (default from given anchor), and tangent angle theta (default from given anchor).*)


 Anchor[
   Object[n:ObjectNamePattern[FigAnchor]]|(n:ObjectNamePattern[FigAnchor])
 ]:=Module[
   {},

   FigCheckInFigure[Anchor];

   (* pass through *)
   FigAnchor[Object[n]]
    ];


 Anchor[
   Object[n:ObjectNamePattern[FigAnchor]]|(n:ObjectNamePattern[FigAnchor]),
   Offset:FigOffsetPattern:Center
 ]:=Module[
   {},

   FigCheckInFigure[Anchor];

   (* pass through *)
   FigAnchor[Object[n],Offset]
    ];


 Anchor[
   Object[n:ObjectNamePattern[FigAnchor]]|(n:ObjectNamePattern[FigAnchor]),
   Offset:FigOffsetPattern,theta:(_?NumericQ)
 ]:=Module[
   {},
   FigCheckInFigure[Anchor];

   (* pass through *)
   FigAnchor[Object[n],Offset,theta]
    ];


 DeclareFigFallThroughError[Anchor];


 (*GetAnchor*)


 (*From figure object MakeAnchor method -- GetAnchor[objectname,arg]*)
 (**)
 (*Note that GetAnchor can support Object[n] whereas FigAnchor direct access could not, for confusion with an anchor object representing a point.*)


 GetAnchor::figmakeanchor="Unable to resolve anchor specification for object `1`, anchor name `2`, with argument `3`.\n(Diagnostic information: `4`)";


 GetAnchor[Object[n:ObjectNamePattern[FigObject]]|(n:ObjectNamePattern[FigObject]),Name_,Arg_:None]:=Module[
   {TemporaryAnchor},

   FigCheckInFigure[GetAnchor];

   (* attempt anchor retrieval *)
   TemporaryAnchor=Object[n]@MakeAnchor[Name,Arg];

   (* verify success of MakeAnchor call *)
   (* note that this error trap is presently superfluous -- the actual error trap will occur earlier, in MakeAnchor, if MakeAnchor uses FigMakeAnchorWrapper interface *)
   If[
     Head[TemporaryAnchor]=!=Object,
     FigError[GetAnchor,"figmakeanchor",n,Name,Arg,TemporaryAnchor]
   ];

   (* return temporary anchor *)
   TemporaryAnchor
                                                                                                     ];
 DeclareFigFallThroughError[GetAnchor];


 (*Diagnostic*)


 (*Printed output of anchor parameters*)


 ShowAnchor[p:FigPointPattern]:=Module[
   {Anchor},
   Anchor=FigAnchor[p];
   Print["Anchor: canvas point ",Anchor@GetPoint[],", text offset ",Anchor@GetOffset[],", tilt angle ",Row[{Anchor@GetAngle[]/Degree,Degree}]];
                                ];


 (*Syntax fallthrough*)


 DeclareFigFallThroughError[ShowAnchor];


 (*End package*)


 (*Exit private context*)


 End[];


 (*Exit package context*)


 Protect[Evaluate[$Context<>"*"]];
 Unprotect[Evaluate[$Context<>"$*"]];
 EndPackage[];
