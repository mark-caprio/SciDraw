(* ::Package:: *)

(*Header comments*)


(* :Title: FigArgument *)
(* :Context: SciDraw` *)
(* :Summary: Miscellaneous argument processing definitions *)
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





(*Automatic value utility*)


(*single pass replacement, but allowing for chained replacement (unlike Replace)*)


(*analogous to InheritOptions`GetAutoOption*)
(**)
(*Szabolcz Horvat suggests (March 13, 2015) replacing original implementation using Scan (and variable assignment) with:*)
(**)
(*	ReplaceSequential[x_, Rules : {((_Rule) | (_RuleDelayed)) ...}] :=Fold[Replace, x, Rules]*)


(*
ReplaceSequential[x_, Rules : {((_Rule) | (_RuleDelayed)) ...}] := Module[
{Value},
Value = x;
Scan[
(Value = Replace[Value, #]) &,
Rules
];
Value
];
 *)


ReplaceSequential[x_,Rules:{((_Rule)|(_RuleDelayed))...}]:=Fold[Replace,x,Rules];


 (*ReplaceSequential[a,{a\[Rule]b,b\[Rule]c,a\[Rule]z}]
ReplaceSequential[a,{a\[Rule]b,x_\[RuleDelayed]x^2,x_\[RuleDelayed]x^2,a\[Rule]z}]*)


 (*Timing test of ReplaceSequential*)


 (*Original Scan*)


 (*With Fold*)


 (*PrintTiming[
ReplaceSequential[a,{a\[Rule]b,x_\[RuleDelayed]x^2,x_\[RuleDelayed]x^2,a\[Rule]z}],
Do\[Rule]10000
]*)


 ResolveOption[Option_Symbol,DefaultRules:{((_Rule)|(_RuleDelayed))...},FullOptions_List]:=ReplaceSequential[(Option/.FullOptions),DefaultRules];


 (*General patterns*)


 (*Numeric patterns*)


 NonNegativePattern=((_?NumericQ)?NonNegative);
 PositivePattern=((_?NumericQ)?Positive);
 UnitIntervalPattern=(((_?NumericQ)?NonNegative)?((#<=1)&));


 (*MatchQ[1,UnitIntervalPattern]
MatchQ[3,UnitIntervalPattern]*)


 (*Integer patterns*)


 NonNegativeIntegerPattern=((_Integer)?NonNegative);
 PositiveIntegerPattern=((_Integer)?Positive);


 (*Logical patterns*)


 LogicalPattern=(True|False);


 (*List patterns*)


 NonListPattern=Except[_List];
 FlatListPattern={NonListPattern...};


 (*Style directive options*)


 (*Atomic directives*)


 SizePattern=(Tiny|Small|Medium|Large);


 (*Color*)


 (*Note that Opacity, as in Opacity[a,color], is not allowed here as a color directive, since it is preferred that the Opacity, LineOpacity, etc. options be used instead.*)
 (*However, the present loose check does still allow colors specified with an opacity argument, as in RGBColor[r,g,b,a].*)


 ColorDirectivePattern=(_GrayLevel|_RGBColor|_CMYKColor|_Hue); 


 (*Line thickness*)


 (*default AbsoluteThickness[0.5] (Mathematica 8)*)


 (*
ThicknessArgumentPattern::usage="Pattern matching a valid argument to Thickness[] or AbsoluteThickness[].";
ThicknessDirectivePattern::usage="Pattern matching a valid dashing directive.  This may be either a Thin or Thick directive or else a Thickness[] or AbsoluteThickness[] directive with valid argument.";
  *)


 ThicknessArgumentPattern=(SizePattern|NonNegativePattern);
 ThicknessDirectivePattern=(Thin|Thick)|Thickness[ThicknessArgumentPattern]|AbsoluteThickness[ThicknessArgumentPattern];
 FigThicknessPattern=(ThicknessArgumentPattern|ThicknessDirectivePattern);


 FigResolveThickness[t:FigThicknessPattern]:=Switch[
   t,
   ThicknessArgumentPattern,AbsoluteThickness[t],
   ThicknessDirectivePattern,t
                                             ];


 (*Line dashing*)


 (*default solid line*)
 (**)
 (*Recall: AbsoluteDashing[d] is equivalent to AbsoluteDashing[{d,d}].*)


 (*
DashingArgumentPattern::usage="Pattern matching a valid argument to Dashing[] or AbsoluteDashing[].";
DashingDirectivePattern::usage="Pattern matching a valid dashing directive.  This may be either a size directive or else a Dashing[] or AbsoluteDashing[] directive with valid arguments.";
  *)


 (*DashingArgumentPattern=((SizePattern|NonNegativePattern)|((L:{(SizePattern|NonNegativePattern)...})/;EvenQ[L]));

MatchQ[{3,4},FigDashingPattern] 
yields False
  *)


 DashingArgumentPattern=((SizePattern|NonNegativePattern)|({(SizePattern|NonNegativePattern)..}?(EvenQ[Length[#]]&)));
 DashingDirectivePattern=(Dotted|DotDashed|Dashed)|Dashing[DashingArgumentPattern]|AbsoluteDashing[DashingArgumentPattern];
 FigDashingPattern=(None|DashingArgumentPattern|DashingDirectivePattern);


 FigResolveDashing[d:FigDashingPattern]:=Switch[
   d,
   None,AbsoluteDashing[{}],
   DashingArgumentPattern,AbsoluteDashing[d],
   DashingDirectivePattern,d
                                         ];


 (*Point size*)


 (*default AbsolutePointSize[3] (Mathematica 8)*)


 (*
PointSizeArgumentPattern::usage="Pattern matching a valid argument to PointSize[] or AbsolutePointSize[].";
PointSizeDirectivePattern::usage="Pattern matching a valid dashing directive.  This may be either a Thin or Thick directive or else a PointSize[] or AbsolutePointSize[] directive with valid argument.";
  *)


 PointSizeArgumentPattern=(SizePattern|NonNegativePattern);
 PointSizeDirectivePattern=PointSize[PointSizeArgumentPattern]|AbsolutePointSize[PointSizeArgumentPattern];
 FigPointSizePattern=(PointSizeArgumentPattern|PointSizeDirectivePattern);


 FigResolvePointSize[t:FigPointSizePattern]:=Switch[
   t,
   PointSizeArgumentPattern,AbsolutePointSize[t],
   PointSizeDirectivePattern,t
                                             ];


 (*Font properties*)


 FontFamilyPattern=(Times|(_String));
 (*FontSizePattern=(NonNegativePattern|Scaled[NonNegativePattern]);*)
 FontSizePattern=NonNegativePattern;  (* only accept absolute font sizes, not Scaled[] font sizes *)
 FontWeightPattern=(Plain|Bold|(_String));
 FontSlantPattern=(Plain|Italic|(_String));
 FontTrackingPattern=(Plain|(_String));


 (*Text parameter interpretation*)


 (*Text positioning*)


 FigTextOffsetPattern=Automatic|FigOffsetPattern;
 FigTextOrientationPattern=Automatic|Inverse|Horizontal|Vertical|(_?NumericQ);


 (*End package*)


 (*Exit private context*)


 End[];


 (*Exit package context*)


 Protect[Evaluate[$Context<>"*"]];
 Unprotect[Evaluate[$Context<>"$*"]];
 EndPackage[];
