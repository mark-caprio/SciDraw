(* ::Package:: *)

(* ::Section::Initialization:: *)
(*Header comments*)


(* ::Input::Initialization:: *)
(* :Title: FigObject *)
(* :Context: SciDraw`FigObject` *)
(* :Summary: Figure object definition infrastructure *)
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


(* ::Section::Initialization:: *)
(*Registration of symbols for option scoping*)


(* ::Subsection::Initialization:: *)
(*Registry of figure objects*)


(* ::Input::Initialization:: *)
$FigClassRegistry={};


(* ::Input::Initialization:: *)
RegisterFigOptions[s_Symbol]:=($FigClassRegistry=Union[$FigClassRegistry,{s}]);


(* ::Subsection::Initialization:: *)
(*Registration of non-classes*)


(* ::Input::Initialization:: *)
RegisterFigOptions[LinTicks];
RegisterFigOptions[LogTicks];


(* ::Section::Initialization:: *)
(*Generic parent object (FigObject)*)


(* ::Subsection::Initialization:: *)
(*Registration for option scoping*)


(* ::Input::Initialization:: *)
RegisterFigOptions[FigObject];


(* ::Subsection::Initialization:: *)
(*Declaration of attached labels*)


(* ::Input::Initialization:: *)
$FigClassAttachedLabels[FigObject]={};


(* ::Section::Initialization:: *)
(*Machinery for figure object class definitions*)


(* ::Subsection::Initialization:: *)
(*Figure object class declaration*)


(* ::Text::Initialization:: *)
(*Declares class, as descendent of FigObject (or an intermediate parent class).*)
(*Registers class name, so options can be scoped.*)
(*Declares creation failure to throw Abort.*)


(* ::Input::Initialization:: *)
General::nocreate="Invalid or unrecognized arguments given to figure object.  Please check the arguments and try again.";


(* ::Input::Initialization:: *)
DeclareFigClass[
Class_Symbol,
ParentClass:_Symbol:FigObject,
DataMemberNames:{___String},
MethodNames:{___String},
AttachedLabelNames:{((_Symbol)|(_String))...}
]:=Module[
{},

(* declare class as descendent of FigObject (or possibly another parent) *)
DeclareClass[Class,ParentClass,DataMemberNames,MethodNames,Replace->True];

(* register class, so options can be scoped *)
$FigClassRegistry=Union[$FigClassRegistry,{Class}];

(* set hook so that error in creation (duplicate name or syntax error) triggers Abort *)
(* The error message provided by MathObject suffices. *)
OnCreationFailure[Class,Self_Object][___]:=Abort[];

(* record attached label names *)
$FigClassAttachedLabels[Class]=AttachedLabelNames;

];


(* ::Subsection::Initialization:: *)
(*Figure object class option definition*)


(* ::Text::Initialization:: *)
(*Note: Object class parent defined via DeclareFigClass is used here also as option inheritance parent.*)
(*To override the default for an inherited option, include option in DefaultOverrides list.*)
(*To override the default for an attached label option, include option in NewOptionRules list.  (Beware you should explicitly specify the package context, as in SciDraw`TailTextOrientation->Horizontal, since the derived label name might not yet have been constructed and added to the namespace.)*)


(* ::Input::Initialization:: *)
DefineFigClassOptions[
Class_Symbol,
DefaultOverrides:_List:{},
NewOptionRules_List
]:=Module[
{ParentClass,NewAttachedLabels},

ParentClass=ClassAncestry[Class][[-2]];
NewAttachedLabels=Complement[$FigClassAttachedLabels[Class],$FigClassAttachedLabels[ParentClass]];
DefineOptions[
Class,
{ParentClass,DefaultOverrides,All},
{

(* attached labels *)
(* use Complement to allow override via NewOptionRules *)
Complement[
FigDerivedLabelOptions[NewAttachedLabels,True,Default],
NewOptionRules,
SameTest->(SameQ[First[#1],First[#2]]&)
],

(* all other special options *)
NewOptionRules

}
]
];


(* ::Input::Initialization:: *)
Complement


(* ::Subsection::Initialization:: *)
(*Figure object realization*)


(* ::Text::Initialization:: *)
(*used internally by FigObjectWrapper*)


(* ::Text::Initialization:: *)
(*Option precedence: *)
(*	(1) explicit options given as arguments (OptionList), *)
(*	(2) option overrides based on object name ($FigOptionRules)*)
(*	(3) default options defined from a figure style object (specified by Style)*)
(*	(4) preexisting default values (Options[Class])*)
(*Once the active option rules are established, all Inherited values must be resolved.  This step is combined with applying the defaults (3)&(4) in RealizeOptions.*)
(*However, a first pass involving only sources (1,2,4) and inheritance must be carried out to determine the value for the option Style, which could be specified from any of those sources, including inheritance!*)
(**)
(*Self vs. Class: The Self object is to be used for error messages.  The Class is the class for which the options are being resolved, using Options[Class].  (Thus Class might not in general necessarily be the class of Self.  This was the case when defining a FigStyle object, which would then be Self -- though FigStyle is no longer implemented.)*)


(* ::Input::Initialization:: *)
General::figunrecopts="One or more unrecognized options `1` were encountered.";


(* ::Input::Initialization:: *)
FigRealizeOptions[Self_Object,Class_Symbol,OptionList_List]:=Module[
{
UnrecognizedOptions,RealizedOptions,OverrideOptions,BaseStyleOptions,StyleList
},

(* trap illegal options *)
UnrecognizedOptions=Complement[First/@Flatten[OptionList],First/@Options[Class]];
If[
UnrecognizedOptions=!={},
Print["Class",Class];
Print["Class options",First/@Options[Class]];
FigError[Self,"figunrecopts",UnrecognizedOptions]
];


(* extract option overrides *)
(* extract option *override* rules whose LHS pattern matches ObjectName[Self], and assemble a list of the resulting *option* rules *)
(* but do not use these if we are resolving options for another class *)
OverrideOptions=If[
ObjectClass[Self]===Class,
ResolveOptionOverrides[ObjectName[Self]],
{}
];

(* pass 1: realize options sans Style override, to find value of Style option *)
RealizedOptions=RealizeOptions[Class,{OptionList,OverrideOptions}];

(* validate style name *)
(*Print[RealizedOptions];*)
FigCheckOption[Self,Style,StyleSpecifierPattern,RealizedOptions];

(* pass 2: realize options after setting default options from style *)
(* Note: This may affect the realized options by resetting the options for Class directly or by resetting the options for an inheritance parent. *)
RealizedOptions=WithStyle[
(Style/.RealizedOptions),
RealizeOptions[Class,{OptionList,OverrideOptions}]
];

RealizedOptions

];


(* ::Subsection::Initialization:: *)
(*Standard option list*)


(* ::Text::Initialization:: *)
(*{option,pattern} pairs*)
(*In *derived* option sets, for each pattern |Default should also be allowed.  Otherwise Default only applies to Show/Color/Opacity.*)
(*For successful panel edge upgrades, all list patterns should be FlatListPattern or otherwise structured to avoid ambiguity (as for the tick list specifications).*)


(* ::Input::Initialization:: *)
BaseOutlineOptionList={
{ShowLine,LogicalPattern|Default},
{LineColor,None|ColorDirectivePattern|Default}, 
{LineOpacity,None|UnitIntervalPattern|Default},
{LineThickness,FigThicknessPattern},
{LineDashing,FigDashingPattern},
{LineCapForm,None|"Butt"|"Round"|"Square"}, 
{LineJoinForm,None|"Miter"|"Bevel"|"Round"|{"Miter",NonNegativePattern}},
{LineDirectives,FlatListPattern}
};
BaseFillOptionList={
{ShowFill,LogicalPattern|Default},
{FillColor,None|ColorDirectivePattern|Default},
{FillOpacity,None|UnitIntervalPattern|Default},
{FillDirectives,FlatListPattern}
};
BasePointOptionList={
{ShowPoint,LogicalPattern|Default},
{PointColor,None|ColorDirectivePattern|Default}, 
{PointOpacity,None|UnitIntervalPattern|Default},
{PointSize,FigPointSizePattern},
{PointDirectives,FlatListPattern}
};
BaseTextOptionList={

(* text appearance *)
{ShowText,LogicalPattern|Default},
{TextColor,None|ColorDirectivePattern|Default},
{TextOpacity,None|UnitIntervalPattern|Default},
{FontFamily,FontFamilyPattern},
{FontSize,FontSizePattern},
{FontWeight,FontWeightPattern},
{FontSlant,FontSlantPattern},
{FontTracking,FontTrackingPattern},
{FontVariations,FlatListPattern},
{TextStyleOptions,FlatListPattern},

(* text positioning *)
{TextBaseBuffer,Automatic|ScalarParameterPattern},
{TextBuffer,ScalarParameterPattern},
{TextNudge,IntervalParametersPattern},
{TextOffset,FigTextOffsetPattern},
{TextOrientation,FigTextOrientationPattern},

(* text background, frame, and geometry *)
{TextBackground,Automatic|None|ColorDirectivePattern},
{TextFrame,LogicalPattern},
{TextFrameOpacity,None|UnitIntervalPattern|Default},
{TextFrameColor,None|ColorDirectivePattern|Default},
{TextFrameThickness,FigThicknessPattern},
{TextFrameDashing,FigDashingPattern},
(* OMIT: TextFrameCapForm, TextFrameJoinForm -- irrelevant to rectangular frame *)
{TextFrameDirectives,FlatListPattern},
{TextRoundingRadius,IntervalParametersPattern},
{TextMargin,NonNegativeRangeParametersPattern},
{TextPadding,LogicalPattern},
{TextRectify,LogicalPattern},

(* text callout -- duplicate line options *)
{TextCallout,LogicalPattern},
{TextCalloutColor,None|ColorDirectivePattern|Default}, 
{TextCalloutOpacity,None|UnitIntervalPattern|Default},
{TextCalloutThickness,FigThicknessPattern},
{TextCalloutDashing,FigDashingPattern},
{TextCalloutCapForm,None|"Butt"|"Round"|"Square"}, 
{TextCalloutJoinForm,None|"Miter"|"Bevel"|"Round"|{"Miter",NonNegativePattern}},
{TextCalloutDirectives,FlatListPattern}

};


(* ::Subsection::Initialization:: *)
(*Figure object core option validation*)


(* ::Input::Initialization:: *)
FigCheckBaseOptions[Self_Object,FullOptions_List]:=Module[
{},

(* overall appearance *)
FigCheckOption[Self,Show,LogicalPattern,FullOptions];
FigCheckOption[Self,Color,None|ColorDirectivePattern,FullOptions]; 
FigCheckOption[Self,Opacity,None|UnitIntervalPattern,FullOptions];
FigCheckOption[Self,Directives,_List,FullOptions];
FigCheckOption[Self,Layer,Automatic|(_?NumericQ),FullOptions];

(* outline/fill/point/text *)
MapThread[
FigCheckOption[Self,#1,#2,FullOptions]&,
Transpose@Join[BaseOutlineOptionList,BaseFillOptionList,BasePointOptionList,BaseTextOptionList]
];

(* style *)
(* option already resolved *)

(* Prolog/Epilog -- no validation required *)

(* diagnostic *)
FigCheckOption[Self,Debug,LogicalPattern,FullOptions];
(*FigCheckOption[Self,PrintTiming,LogicalPattern,FullOptions];*)

];


(* ::Subsection::Initialization:: *)
(*Standard attached label invocation (helper for FigObjectWrapper)*)


(* ::Input::Initialization:: *)
FigProcessStandardAttachedLabels[Self_Object]:=Module[
{Side},

Do[
FigCheckDerivedLabelOptions[Self,Side,True,FigOptions];
FigSpawnAttachedLabel[Self,Side,FigResolveDerivedLabelOptions[Side,True,FigOptions],FigOptions],
{Side,$FigClassAttachedLabels[ObjectClass[Self]]}
]
];


(* ::Subsection::Initialization:: *)
(*Figure object wrapper*)


(* ::Text::Initialization:: *)
(*Provides uniform figure object interface...*)
(**)
(*Uniform environment for evaluation of body:*)
(*	FigOptions -- realized option set*)
(*	Imposed style (important if body indirectly references options of other symbols, e.g., for FigurePanel) based on Style option applied for evaluation of prolog/body/epilog*)
(**)
(*Body should return: No return value expected.*)
(**)
(*Prolog processing:*)
(*	Checks that object invocation is inside a Figure.*)
(*	Checks that user-assigned object name is valid.*)
(*	Validates option values for core FigObject set of options.*)
(*	Saves option values for object. (Is this really useful?)*)
(*	Evaluates Prolog option.*)
(*	*)
(*Epilog processing:*)
(*	Evaluates Epilog option.*)
(*	Generates attached labels.*)
(*	*)
(*Return:*)
(*	Object reference.*)
(*	*)
(*Also provides some ad hoc profiling machinery.*)


(* ::Text::Initialization:: *)
(**)


(* ::Program::Initialization:: *)
(*(* for profiling of option realization timing -- possible performance bottleneck *)*)
(*FigOptions=PrintTiming[*)
(*FigRealizeOptions[Self,Class,OptionList],*)
(*Label->Row[{">>>Option realization ", Class," ",Self}],*)
(*Print->$PrintTiming*)
(*]*)


(* ::Input::Initialization:: *)
SetAttributes[FigObjectWrapper,HoldAll];
FigObjectWrapper[Class_Symbol,Self_Object,OptionList_List,Body_]:=PrintTiming[Block[
{
FigOptions=FigRealizeOptions[Self,Class,OptionList]
},

(* prolog processing *)
PrintTiming[
{
(* check in figure *)
FigCheckInFigure[Self];

(* validate object name *)
FigCheckObjectName[Self];

(* validate core options *)
FigCheckBaseOptions[Self,FigOptions];

(* save options *)
(*Self@SetOptionValues[FigOptions];*)
},
Label->Row[{"...Object setup ", Class," ",Self}],
Print->$PrintTiming
];

(* evaluate body *)
(* Note: Prolog and Epilog as bare (Prolog/.FigOptions) would require reimplementation so that they are not multiply evaluated during option inheritance processing. Current model is to presume option is wrapped in Hold.  *)
PrintTiming[
WithStyle[
(Style/.FigOptions),
{
If[(Debug/.FigOptions),Print["Entering ",ObjectClass[Self],"[",ObjectName[Self],"]..."]];
ReleaseHold[(Prolog/.FigOptions)];
Body;
ReleaseHold[(Epilog/.FigOptions)];
If[(Debug/.FigOptions),Print["Exiting ",ObjectClass[Self],"[",ObjectName[Self],"]."]]
}
],
Label->Row[{"...Object body ", Class," ",Self}],
Print->$PrintTiming
];

(*If[(Debug/.FigOptions),ShowObjectInformation[Self]];*)


(* validate and spawn any standard attached label *)
PrintTiming[
{
FigProcessStandardAttachedLabels[Self]
},
Label->Row[{"...Object labels ", Class," ",Self}],
Print->$PrintTiming
];

(* return reference to created object -- for possible future use in stream operator overloads *)
Self
],
Label->Row[{"***Object wrapper ", Class," ",Self}],
Print->$PrintTiming
];


(* ::Subsection::Initialization:: *)
(*Figure object anchor retrieval wrapper *)


(* ::Program::Initialization:: *)
(*(* example usage *)*)
(*MakeAnchor[Class:FigLine,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[*)
(*Class,Self,Name,Arg,*)
(*FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]*)
(*];*)


(* ::Input::Initialization:: *)
General::noanchor="An anchor named `2` with argument `3` cannot be generated for this object.  Please check the anchor names and arguments allowed for objects of class `1`.\n\n(Debugging information: `4`)";
General::noanchorn="An anchor named `2` (with no argument) cannot be generated for this object.  Please check the anchor names and arguments allowed for objects of class `1`.\n\n(Debugging information: `4`)";


(* ::Input::Initialization:: *)
FigMakeAnchorWrapper[Class_Symbol,Self_Object,Name_,Arg_,Expr_]:=If[
MatchQ[Expr,_Object],
Expr,
If[
Arg===None,
FigError[MakeAnchor,Self,"noanchorn",Class,Name,Arg,Expr],
FigError[MakeAnchor,Self,"noanchor",Class,Name,Arg,Expr]
]
];


(* ::Subsection::Initialization:: *)
(*Figure object anchor retrieval wrapper -- revised -- UNDER CONSTRUCTION*)


(* ::Text::Initialization:: *)
(*The idea here is that all the option processing for orientation, offset, buffer, and nudge should be done at the anchor level, not later in the text element rendering.*)
(*The wrench in the works is that much of the same machinery (TextNudge, etc.) is used for the tick derived labels, which do not go through anchor resolution.*)
(*Furthermore, the present Buffer mechanism relies on an initial anchor orientation, which will be lost if the anchor generation method already imposes the orientation from the Orientation option.*)
(**)
(*Thus, for instance, an anchor method could support special orientation option values, such as "Tangent" or "Normal".*)


(* ::Text::Initialization:: *)
(*Provides uniform figure object anchor interface...*)
(**)
(*Uniform environment for evaluation of body:*)
(*	FigAnchorOptions -- includes Position, Orientation, Offset, Buffer, and Nudge options*)
(**)
(*Body should return:*)
(*	{GeneratedPoint,GeneratedOrientation,GeneratedOffset,GeneratedBufferDirection} in canonical acceptable form for these values, i.e., matching FigPointPattern, FigTextOrientationPattern, FigTextOffsetPattern, NumericalPairPattern.*)
(**)
(*	Body should be able to resolve Automatic values: Position->Automatic, Orientation->Automatic, Offset->Automatic*)
(*	*)
(*Prolog processing:*)
(*	Validates option values for Buffer and Nudge.*)
(*	*)
(*Epilog processing:*)
(*	Validates that coordinate, orientation, and offset returned by body are in canonical acceptable form.*)
(*	Applies Buffer and Nudge.*)
(*	Generates anchor object.*)
(*	*)
(*Return:*)
(*	Anchor object reference.*)
(*	*)
(*Error convention: Error messages should use FigError[MakeAnchor,...].  Otherwise, an error message with Self would give a misleading error message indicating that the error occurred while generating the object Self, which might or might not be true.*)


(* ::Program::Initialization:: *)
(*(* example usage *)*)
(*MakeAnchor[Class:FigLine,Self_Object][Name_,OptionsList_List]:=FigMakeAnchorWrapper[*)
(*Class,Self,Name,OptionsList,*)
(*FigCurveAnchorFromPoints[Self@GetPoints[],Name]*)
(*];*)


(* ::Text::Initialization:: *)
(*Note buffer direction consideration, from calculation in old FigElement:*)


(* ::Program::Initialization:: *)
(*(* determine buffering *)*)
(*(* "outward" direction is determined from *original* anchor positioning parameters, since possible later overrides, e.g., to Horizontal, would throw off the direction *)*)
(*BufferDirection=If[*)
(*VectorLength[AnchorOffset]==0,{0,0},*)
(*(-1)*RotationTransform[N@AnchorAngle][AnchorOffset/VectorLength[AnchorOffset]]*)
(*];*)


(* ::Input::Initialization:: *)
General::objectanchor="An anchor named `2`, with position `3`, orientation `4`, and offset `5`, could not be constructed for this object.  Please check the anchor names and parameters allowed for objects of class `1`.\n\n(Debugging information: `6`)";


(* ::Input::Initialization:: *)
SetAttributes[FigObjectAnchorWrapper,HoldAll];
FigObjectAnchorWrapper[
Class_Symbol,Self_Object,Name_,OptionsList_List,
Body_]:=Block[
{
FigAnchorOptions=Join[OptionsList,{Position->Automatic,Orientation->Automatic,Offset->Automatic,Buffer->None,Nudge->None}],
GeneratedAnchor,GeneratedPoint,GeneratedAngle,GeneratedOffset,BufferDirection,UsedPoint
},

Print[FigAnchorOptions];
(* validate buffer and nudge *)
FigCheckOption[MakeAnchor,Self,Buffer,ScalarParameterPattern,FigAnchorOptions];
FigCheckOption[MakeAnchor,Self,Nudge,IntervalParametersPattern,FigAnchorOptions];

(* evaluate body *)
GeneratedAnchor=Body;

(* validate body return *)
If[
!MatchQ[GeneratedAnchor,FigPointPattern],
PrintValue@FigError[MakeAnchor,Self,"objectanchor",Class,Name,(Position/.FigAnchorOptions),(Orientation/.FigAnchorOptions),(Offset/.FigAnchorOptions),Body]
];

(* extract components of anchor *)
{GeneratedPoint,GeneratedAngle,GeneratedOffset}={GeneratedAnchor@GetPoint[],GeneratedAnchor@GetAngle[],GeneratedAnchor@GetOffset[]};

(* apply buffer and nudge *)
(* determine buffered and nudged position *)
(* note that classic LevelScheme Nudge\[Rule]dy form is supported here, with UpgradePairVertical, but for derived labels it might be disallowed in the option validation of the calling object (verify?),
as idiosyncratic and incompatible with panel edge upgrade *)
BufferDirection={0,0};  (* TODO implement in generated anchor *)
UsedPoint=GeneratedPoint+UpgradeScalar[(Buffer/.FigAnchorOptions)]*BufferDirection+UpgradePairVertical[(Nudge/.FigAnchorOptions)];

(* return anchor *)
FigAnchor[Canvas[UsedPoint],GeneratedOffset,GeneratedAngle]
];


(* ::Subsection::Initialization:: *)
(*Figure bounding box retrieval wrapper*)


(* ::Input::Initialization:: *)
General::nobb="No bounding region has been implemented for this object.";


(* ::Input::Initialization:: *)
FigMakeBoundingBoxWrapper[Class_Symbol,Self_Object,Expr_]:=If[
MatchQ[Expr,NumericalRegionPattern],
Expr,
FigError[MakeBoundingBox,Class,"nobb"]
];


(* ::Section::Initialization:: *)
(*Derived labels (including attached and tick labels)*)


(* ::Subsection::Initialization:: *)
(*Automated handling of options for derived labels -- e.g., attached labels, tick labels*)


(* ::Text::Initialization:: *)
(*Generate option name*)


(* ::Input::Initialization:: *)
SidifyOptionName[Side:(_Symbol)|(_String)][Option_Symbol]:=Module[
{SideName},

SideName=Switch[
Side,
_Symbol,ToString[Side],
_String,Side
];

ToExpression["SciDraw`"<>SideName<>ToString[Option]]
];


(* ::Text::Initialization:: *)
(*Construct option list for object's option declaration*)
(*	IncludeLabelOptions -- whether or not XLabel and XLabelPosition should actually be included in list of options*)
(*	DefaultFontSize -- possible alternative to usual "Default" for font size, e.g., for special smaller tick font*)


(* ::Input::Initialization:: *)
FigDerivedLabelOptions[Side:(_Symbol)|(_String),IncludeLabelOptions:LogicalPattern,DefaultFontSize_]:=Flatten[{

(* label content *)
If[
IncludeLabelOptions,
{SidifyOptionName[Side]@Label->None,SidifyOptionName[Side]@LabelPosition->Automatic},
{}
],

(* override font size default *)
SidifyOptionName[Side]@FontSize->DefaultFontSize,

(* standard text options *)
Map[
(SidifyOptionName[Side][#]->Default)&,
Complement[First/@BaseTextOptionList,{FontSize}]
]
}];


(* ::Text::Initialization:: *)
(*Or threaded over list for convenience...*)


(* ::Input::Initialization:: *)
FigDerivedLabelOptions[SideList:{((_Symbol)|(_String))...},IncludeLabelOptions:LogicalPattern,DefaultFontSize_]:=Flatten[(FigDerivedLabelOptions[#,IncludeLabelOptions,DefaultFontSize]&)/@SideList];


(* ::Text::Initialization:: *)
(*Option validation*)


(* ::Input::Initialization:: *)
FigCheckDerivedLabelOptions[Self_Object,Side:(_Symbol)|(_String),IncludeLabelOptions:LogicalPattern,FullOptions_List]:=Module[
{},

(* label content *)
 (* anything goes! *)

(* label position *)
(* anything goes, but MakeAnchor method is free to balk later *)


(* standard text options *)
(* all initially acceptable patterns are accepted, plus Default *)
MapThread[
FigCheckOption[Self,SidifyOptionName[Side]@#1,#2|Default,FullOptions]&,
Transpose@BaseTextOptionList
];

];


(* ::Text::Initialization:: *)
(*FigResolveDerivedLabelOptions[side] returns*)
(*	{label,position,optionslist}*)
(*	label -- the value of the option sideLabel*)
(*	position -- the value of XXXXPosition, which will be used as the additional argument to Self@MakeAnchor[side,argument]*)
(*		if XXXXLabelPosition does not exist as an option, the value None is used*)
(*		a value Automatic is taken as an alias for None*)
(*	optionslist -- all the base text labels, upgrade from (sideXXXX->value) to (XXXX->value)*)
(*		If Default, the value for sideXXXX defaults to the value for XXXX.*)
(*		It is okay if this value is still, in turn, Default, since FigTextElement will take care of resolving Default for the base labels.*)
(*The label and position will only be processed if {Label,LabelPosition} are specified in the NonBaseOptions argument.  For instance, they are not defined for tick labels.*)


(* ::Input::Initialization:: *)
FigResolveDerivedLabelOptions[Side:(_Symbol)|(_String),IncludeLabelOptions:LogicalPattern,FullOptions_List]:=Module[
{Content,UsedArgument,OptionsList},

Content=If[
IncludeLabelOptions,
ResolveOption[SidifyOptionName[Side][Label],{},FullOptions],
None
];

UsedArgument=If[
IncludeLabelOptions,
ResolveOption[SidifyOptionName[Side][LabelPosition],{Automatic->None},FullOptions],
None
];

OptionsList=(#->ResolveOption[SidifyOptionName[Side][#],{Default:>(#/.FullOptions)},FullOptions])&/@(First/@BaseTextOptionList);

{Content,UsedArgument,OptionsList}
];


(* ::Subsection::Initialization:: *)
(*Spawning of given attached label*)


(* ::Input::Initialization:: *)
General::figlabelpos="Cannot resolve `1` attached label position.  (Debugging information: Perhaps the option specification `2`->`3` is not of a supported form for this class of object.  The anchor `4` evaluated to `5`.)";


(* ::Input::Initialization:: *)
FigSpawnAttachedLabel[Self_Object,Side:(_Symbol)|(_String),{Content_,LabelPositionArgument_,OptionsList_},FullOptions_List]:=Module[
{Anchor},

(* extract label parameters *)
(*{Content,LabelPositionArgumentList,OptionsList}=FigResolveDerivedLabelOptions[Side,{Label,LabelPosition},FullOptions];*)

(* short circuit *)
If[
(Content===None)||((Show/.OptionsList)===False)||((ShowText/.OptionsList)===False),
Return[]
];

(* generate point *)
Anchor=Self@MakeAnchor[Side,LabelPositionArgument];

(* make text *)
FigTextElement[
Anchor,
Content,
Flatten[{OptionsList,FullOptions}]
];

];


(* ::Section::Initialization:: *)
(*Figure object option control*)


(* ::Subsection::Initialization:: *)
(*Option scoping*)


(* ::Text::Initialization:: *)
(*Option scoping is used by Figure and also is also accessible by the user as a scoping structure within the figure body*)


(* ::Input::Initialization:: *)
SetAttributes[ScopeOptions,HoldAll];
ScopeOptions[Body_]:=BlockOptions[
$FigClassRegistry,
Body
];
DeclareFigFallThroughError[ScopeOptions];


(* ::Subsection::Initialization:: *)
(*Option override*)


(* ::Input::Initialization:: *)
SetOptionOverrides[rl:{((Rule|RuleDelayed)[_,_List])...}]:=($FigOptionRules=Join[$FigOptionRules,rl]);
SetOptionOverrides[r:((Rule|RuleDelayed)[_,_List])]:=SetOptionOverrides[{r}];
DeclareFigFallThroughError[SetOptionOverrides];


(* ::Input::Initialization:: *)
SetAttributes[ScopeOptionOverrides,HoldAll];
ScopeOptionOverrides[Body_]:=Block[
{$FigOptionRules=$FigOptionRules},
Body
];
DeclareFigFallThroughError[ScopeOptionOverrides];


(* ::Input::Initialization:: *)
ResolveOptionOverrides[Name_]:=Module[
{Overrides},
(*Print["resolving ",Name," with rules ",$FigOptionRules];*)
Overrides=OptionsUnion[Cases[$FigOptionRules,((patt_->opts_List)/;MatchQ[Name,patt]):>opts]];
If[
$DebugOptionOverrides,
If[Length[Overrides]>0,
Print["Option overrides for ",Name,": ",Overrides]
]
];
Overrides
]


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
