(* ::Package:: *)

(* ::Section::Initialization:: *)
(*Header comments*)


(* ::Input::Initialization:: *)
(* :Title: FigArgument *)
(* :Context: SciDraw` *)
(* :Summary: Miscellaneous argument processing definitions *)
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
(*Automatic value utility*)


(* ::Text::Initialization:: *)
(*single pass replacement, but allowing for chained replacement (unlike Replace)*)


(* ::Text::Initialization:: *)
(*analogous to InheritOptions`GetAutoOption*)
(**)
(*Szabolcz Horvat suggests (March 13, 2015) replacing original implementation using Scan (and variable assignment) with:*)
(**)
(*	ReplaceSequential[x_, Rules : {((_Rule) | (_RuleDelayed)) ...}] :=Fold[Replace, x, Rules]*)


(* ::Input::Initialization:: *)
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


(* ::Input::Initialization:: *)
ReplaceSequential[x_,Rules:{((_Rule)|(_RuleDelayed))...}]:=Fold[Replace,x,Rules];


(* ::Input::Initialization:: *)
(*ReplaceSequential[a,{a\[Rule]b,b\[Rule]c,a\[Rule]z}]
ReplaceSequential[a,{a\[Rule]b,x_\[RuleDelayed]x^2,x_\[RuleDelayed]x^2,a\[Rule]z}]*)


(* ::Text::Initialization:: *)
(*Timing test of ReplaceSequential*)


(* ::Text::Initialization:: *)
(*Original Scan*)


(* ::Text::Initialization:: *)
(*With Fold*)


(* ::Input::Initialization:: *)
(*PrintTiming[
ReplaceSequential[a,{a\[Rule]b,x_\[RuleDelayed]x^2,x_\[RuleDelayed]x^2,a\[Rule]z}],
Do\[Rule]10000
]*)


(* ::Input::Initialization:: *)
ResolveOption[Option_Symbol,DefaultRules:{((_Rule)|(_RuleDelayed))...},FullOptions_List]:=ReplaceSequential[(Option/.FullOptions),DefaultRules];


(* ::Section::Initialization:: *)
(*General patterns*)


(* ::Subsection::Initialization:: *)
(*Numeric patterns*)


(* ::Input::Initialization:: *)
NonNegativePattern=((_?NumericQ)?NonNegative);
PositivePattern=((_?NumericQ)?Positive);
UnitIntervalPattern=(((_?NumericQ)?NonNegative)?((#<=1)&));


(* ::Input::Initialization:: *)
(*MatchQ[1,UnitIntervalPattern]
MatchQ[3,UnitIntervalPattern]*)


(* ::Subsection::Initialization:: *)
(*Integer patterns*)


(* ::Input::Initialization:: *)
NonNegativeIntegerPattern=((_Integer)?NonNegative);
PositiveIntegerPattern=((_Integer)?Positive);


(* ::Subsection::Initialization:: *)
(*Logical patterns*)


(* ::Input::Initialization:: *)
LogicalPattern=(True|False);


(* ::Subsection::Initialization:: *)
(*List patterns*)


(* ::Input::Initialization:: *)
NonListPattern=Except[_List];
FlatListPattern={NonListPattern...};


(* ::Section::Initialization:: *)
(*Style directive options*)


(* ::Subsection::Initialization:: *)
(*Atomic directives*)


(* ::Input::Initialization:: *)
SizePattern=(Tiny|Small|Medium|Large);


(* ::Subsection::Initialization:: *)
(*Color*)


(* ::Text::Initialization:: *)
(*Note that Opacity, as in Opacity[a,color], is not allowed here as a color directive, since it is preferred that the Opacity, LineOpacity, etc. options be used instead.*)
(*However, the present loose check does still allow colors specified with an opacity argument, as in RGBColor[r,g,b,a].*)


(* ::Input::Initialization:: *)
ColorDirectivePattern=(_GrayLevel|_RGBColor|_CMYKColor|_Hue); 


(* ::Subsection::Initialization:: *)
(*Line thickness*)


(* ::Text::Initialization:: *)
(*default AbsoluteThickness[0.5] (Mathematica 8)*)


(* ::Input::Initialization:: *)
(*
ThicknessArgumentPattern::usage="Pattern matching a valid argument to Thickness[] or AbsoluteThickness[].";
ThicknessDirectivePattern::usage="Pattern matching a valid dashing directive.  This may be either a Thin or Thick directive or else a Thickness[] or AbsoluteThickness[] directive with valid argument.";
*)


(* ::Input::Initialization:: *)
ThicknessArgumentPattern=(SizePattern|NonNegativePattern);
ThicknessDirectivePattern=(Thin|Thick)|Thickness[ThicknessArgumentPattern]|AbsoluteThickness[ThicknessArgumentPattern];
FigThicknessPattern=(ThicknessArgumentPattern|ThicknessDirectivePattern);


(* ::Input::Initialization:: *)
FigResolveThickness[t:FigThicknessPattern]:=Switch[
t,
ThicknessArgumentPattern,AbsoluteThickness[t],
ThicknessDirectivePattern,t
];


(* ::Subsection::Initialization:: *)
(*Line dashing*)


(* ::Text::Initialization:: *)
(*default solid line*)
(**)
(*Recall: AbsoluteDashing[d] is equivalent to AbsoluteDashing[{d,d}].*)


(* ::Input::Initialization:: *)
(*
DashingArgumentPattern::usage="Pattern matching a valid argument to Dashing[] or AbsoluteDashing[].";
DashingDirectivePattern::usage="Pattern matching a valid dashing directive.  This may be either a size directive or else a Dashing[] or AbsoluteDashing[] directive with valid arguments.";
*)


(* ::Input::Initialization:: *)
(*DashingArgumentPattern=((SizePattern|NonNegativePattern)|((L:{(SizePattern|NonNegativePattern)...})/;EvenQ[L]));

MatchQ[{3,4},FigDashingPattern] 
yields False
*)


(* ::Input::Initialization:: *)
DashingArgumentPattern=((SizePattern|NonNegativePattern)|({(SizePattern|NonNegativePattern)..}?(EvenQ[Length[#]]&)));
DashingDirectivePattern=(Dotted|DotDashed|Dashed)|Dashing[DashingArgumentPattern]|AbsoluteDashing[DashingArgumentPattern];
FigDashingPattern=(None|DashingArgumentPattern|DashingDirectivePattern);


(* ::Input::Initialization:: *)
FigResolveDashing[d:FigDashingPattern]:=Switch[
d,
None,AbsoluteDashing[{}],
DashingArgumentPattern,AbsoluteDashing[d],
DashingDirectivePattern,d
];


(* ::Subsection::Initialization:: *)
(*Point size*)


(* ::Text::Initialization:: *)
(*default AbsolutePointSize[3] (Mathematica 8)*)


(* ::Input::Initialization:: *)
(*
PointSizeArgumentPattern::usage="Pattern matching a valid argument to PointSize[] or AbsolutePointSize[].";
PointSizeDirectivePattern::usage="Pattern matching a valid dashing directive.  This may be either a Thin or Thick directive or else a PointSize[] or AbsolutePointSize[] directive with valid argument.";
*)


(* ::Input::Initialization:: *)
PointSizeArgumentPattern=(SizePattern|NonNegativePattern);
PointSizeDirectivePattern=PointSize[PointSizeArgumentPattern]|AbsolutePointSize[PointSizeArgumentPattern];
FigPointSizePattern=(PointSizeArgumentPattern|PointSizeDirectivePattern);


(* ::Input::Initialization:: *)
FigResolvePointSize[t:FigPointSizePattern]:=Switch[
t,
PointSizeArgumentPattern,AbsolutePointSize[t],
PointSizeDirectivePattern,t
];


(* ::Subsection::Initialization:: *)
(*Font properties*)


(* ::Input::Initialization:: *)
FontFamilyPattern=(Times|(_String));
(*FontSizePattern=(NonNegativePattern|Scaled[NonNegativePattern]);*)
FontSizePattern=NonNegativePattern;  (* only accept absolute font sizes, not Scaled[] font sizes *)
FontWeightPattern=(Plain|Bold|(_String));
FontSlantPattern=(Plain|Italic|(_String));
FontTrackingPattern=(Plain|(_String));


(* ::Section::Initialization:: *)
(*Text parameter interpretation*)


(* ::Subsection::Initialization:: *)
(*Text positioning*)


(* ::Input::Initialization:: *)
FigTextOffsetPattern=Automatic|FigOffsetPattern;
FigTextOrientationPattern=Automatic|Inverse|Horizontal|Vertical|(_?NumericQ);


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
