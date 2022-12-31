(* ::Package:: *)

(* ::Section:: *)
(*Header comments*)


(* :Title: FigPanel *)
(* :Context: SciDraw` *)
(* :Summary: Panel object *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Copyright: Copyright FIGYEAR, Mark A. Caprio *)
(* :Package Version: FIGVERSION *)
(* :Mathematica Version: MATHVERSION *)
(* :Discussion: FIGDISCUSSION *)
(* :History: See package information file. *)


(* ::Section:: *)
(*Begin package*)


(* ::Subsection:: *)
(*Package context definition*)


BeginPackage["SciDraw`",SciDraw`Private`$ExternalContexts];


Unprotect[Evaluate[$Context<>"*"]];


(* ::Subsection:: *)
(*Object usage*)


FigAxis::usage="FIGURE OBJECT: FigAxis[side,coordinate,range] generates an standalone axis.";
FigurePanel::usage="FIGURE OBJECT: FigurePanel[body] generates a figure panel.  A region option may be specified as (1) PanelRegion->All for the full current window (default), (2) PanelRegion->{{x1,x2},{y1,y2}} for a coordinate region, (3) PanelRegion->Scaled[{{xs1,xs2},{ys1,ys2}}] for scaled coordinates relative to the current window, or (4) PanelRegion->Canvas[{{xc1,xc2},{yc1,yc2}}] for canvas coordinates.  Within a Multipanel array, FigurePanel[body,{row,col}] generates a panel with the given with row and column indices.  The argument {row,col} may be a pattern or All, for iteration over multipanel array.";
Multipanel::usage="FIGURE OBJECT: Multpanel[body] generates a multipanel array.";
PanelRegion::usage="Option name for use with FigurePanel and Multipanel.";
LockPanelScale::usage="Option name for use with FigurePanel and Multipanel.";
(*PanelPattern::usage="Option name for use with FigurePanel and Multipanel.";*)


(* ::Subsection:: *)
(*Other usage*)


(* ::Text:: *)
(*Utilities*)


CurrentMultipanel::usage="CurrentMultipanel[] returns the current multipanel object (or None if none).";
SetByPanelRow::usage="SetByPanelRow[var,{value1,...}] sets variable values according to the current multipanel row.";
SetByPanelColumn::usage="SetByPanelColumn[var,{value1,...}] sets variable values according to the current multipanel column.";
SetByPanelIndices::usage="SetByPanelIndices[var,{value1,...}] or SetByPanelIndices[var,{patt1->val1,...}] sets variable values according to the current multipanel (row,column) indices.";
SetByPanelSequence::usage="SetByPanelSequence[var,{value1,...}] sets variable values according to the current multipanel sequence number.";

ByPanelRow::usage="ByPanelRow[{value1,...}] returns values according to the current multipanel row.";
ByPanelColumn::usage="ByPanelColumn[{value1,...}] returns values according to the current multipanel column.";
ByPanelIndices::usage="ByPanelIndices[{value1,...}] or ByPanelIndices[{patt1->val1,...}] returns values according to the current multipanel (row,column) indices.";
ByPanelSequence::usage="ByPanelSequence[{value1,...}] returns values according to the current multipanel sequence number.";


(* ::Text:: *)
(*Options*)


TailExtension::usage="Option name for use with figure objects.  Specifies the distance a figure object curve's tail should be extended from the given endpoint.";
HeadExtension::usage="Option name for use with figure objects.  Specifies the distance a figure object curve's head should be extended from the given endpoint.";
ShowTicks::usage="Option name for use with figure objects.";
TickFontSizeFactor::usage="Option name for use with figure objects.";
TickLengthReference::usage="Option name for use with figure objects.";
(* TickLengthScale::usage="Option name for use with figure objects. -- taken from CustomTicks";*)
TickLabelAllowance::usage="Option name for use with figure objects.";


BackgroundOpacity::usage="Option name for use with figure objects.";
BackgroundDirectives::usage="Option name for use with figure objects.";
XPlotRange::usage="Option name for use with figure objects.";
YPlotRange::usage="Option name for use with figure objects.";
ExtendRange::usage="Option name for use with figure objects.";
XExtendRange::usage="Option name for use with figure objects.";
YExtendRange::usage="Option name for use with figure objects.";

PanelLetter::usage="Option name for use with figure objects.";
PanelLetterPosition::usage="Option name for use with figure objects.";
PanelLetterBase::usage="Option name for use with figure objects.";
PanelLetterOrigin::usage="Option name for use with figure objects.";
PanelLetterDimensions::usage="Option name for use with figure objects.";
PanelLetterCorrection::usage="Option name for use with figure objects.";
PanelLetterDelimiters::usage="Option name for use with figure objects.";
PanelLetterDirection::usage="Option name for use with figure objects.";

(* Frame::usage=... *)
XFrame::usage="Option name for use with figure objects.";
YFrame::usage="Option name for use with figure objects.";

PanelEdgeExterior::usage="Option name for use with figure objects.";

(* FrameLabel::usage=... *)
XFrameLabel::usage="Option name for use with figure objects.";
YFrameLabel::usage="Option name for use with figure objects.";
ShowFrameLabel::usage="Option name for use with figure objects.";
XShowFrameLabel::usage="Option name for use with figure objects.";
YShowFrameLabel::usage="Option name for use with figure objects.";
FrameLabelPosition::usage="Option name for use with figure objects.";
XFrameLabelPosition::usage="Option name for use with figure objects.";
YFrameLabelPosition::usage="Option name for use with figure objects.";

(* Ticks::usage=... *)
XTicks::usage="Option name for use with figure objects.";
YTicks::usage="Option name for use with figure objects.";
ShowTicks::usage="Option name for use with figure objects.";
XShowTicks::usage="Option name for use with figure objects.";
YShowTicks::usage="Option name for use with figure objects.";
ShowTickLabels::usage="Option name for use with figure objects.";
XShowTickLabels::usage="Option name for use with figure objects.";
YShowTickLabels::usage="Option name for use with figure objects.";

ExteriorEdgeMask::usage="Option name for use with figure objects.  (DEPRECATED)";

XPanelSizes::usage="Option name for use with figure objects.";
YPanelSizes::usage="Option name for use with figure objects.";
XPanelGaps::usage="Option name for use with figure objects.";
YPanelGaps::usage="Option name for use with figure objects.";
XPanelGapsExterior::usage="Option name for use with figure objects.";
YPanelGapsExterior::usage="Option name for use with figure objects.";


TickListSpecificationPattern::usage="Pattern which matches possible tick list specifications -- a list which is recognizably a list of ticks or any nonlist expression (which could conceivably be None, Automatic, the name of a tick generation function, or a lambda function).";


(* ::Text:: *)
(*Global variables*)


PanelColumnIndex::usage="Parameter defined within FigPanel.";
PanelRowIndex::usage="Parameter defined within FigPanel.";
PanelIndices::usage="Parameter defined within FigPanel.";
PanelSequenceNumber::usage="Parameter defined within FigPanel.";
PanelColumns::usage="Parameter defined within Multipanel.";
PanelRows::usage="Parameter defined within Multipanel.";


(* ::Subsection:: *)
(*Begin private context*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Dependencies*)





(* ::Section:: *)
(*Utilities*)


(* ::Subsection:: *)
(*Tick matching*)


TickListSpecificationPattern=NonListPattern|(_?TickListQ);


(* ::Subsection:: *)
(*Extraction of option from array*)


(* ::Text:: *)
(*Retrieval of individual edge or panel options*)


ExtractOptionsEntry[CompositeOptions_,{r_Integer,s_Integer}]:=Replace[CompositeOptions,Rule[Option_,Value_]:>Rule[Option,Value[[r,s]]],{1}];


(* ::Section:: *)
(*FigurePanel option set*)


(* ::Subsection:: *)
(*FigurePanel option notes*)


(* ::Text:: *)
(*Whole-panel options*)
(*	FigObject options*)
(*	PanelRegion*)
(*	background options*)
(*	...*)


(* ::Text:: *)
(*Option			# values	Same/Unique	Mirroring		Default	Filler		Axis analog*)
(**)
(*PlotRange			2	*)
(*PlotRangePadding		2x2 special handling				*)
(**)
(*Frame				2x2	S	{XY}->{{YY}{XX}}	True		--		Show			*)
(**)
(*FrameLabel			2x2	U	{XY}->{{YY}{XX}}    	None		None		AxisLabel		CONVENIENCE*)
(*ShowFrameLabel		2x2	S	{XY}->{{Y-}{X-}}		Exterior	--					MASK*)
(*	nominally "redundant" to FrameShowText, but convenient for multipanel exterior masking*)
(*FrameLabelPosition		2x2	S	{XY}->{{YY}{XX}}	{}		--		AxisLabelPosition	CONVENIENCE*)
(*FrameShowText, etc.		2x2	S	{XY}->{{YY}{XX}}	Default	--		AxisShowText		CONVENIENCE*)
(**)
(*[Frame]Ticks			2x2	U	{XY}->{{YY}{XX}}	Automatic	None		Ticks*)
(*TickLabelRange		2x2	U	{XY}->{{YY}{XX}}	Automatic	--		TickLabelRange*)
(*Show[Frame]Ticks		2x2	S	{XY}->{{YY}{XX}}	True		--		(ShowTicks)		MASK*)
(*Show[Frame]TickLabels	2x2	S	{XY}->{{Y-}{X-}}		True		False		ShowTickLabels*)
(*FrameTickShowText, etc.*)
(**)
(*PanelEdgeExterior                 2x2     S	{XY}->{{YY}{XX}}           Automatic	--				MASK *)
(**)
(*-- for Filler indicates mirroring for secondary axis*)
(**)
(*Alternate scenario:*)
(*	Pro -- allows user to immediately specify right and top frame labels, without changing the ShowFrameLabel mask*)
(*	Con -- doesn't allow those labels to come from XFrameLabel and YFrameLabel and is nonparallel to the treatment of ShowTickLabels by boolean masking*)
(*FrameLabel			2x2	U	{XY}->{{Y-}{X-}}    	None		None		AxisLabel		CONVENIENCE*)
(*ShowFrameLabel		2x2	S	{XY}->{{YY}{XX}}	Exterior	--					MASK*)
(**)
(**)
(*Filler is irrelevant for S-style parameters with mirroring.*)
(*A single value None (if allowed by pattern) will propagate automatically.*)


(* ::Subsection:: *)
(*Option descriptor list*)


(* ::Text:: *)
(*Each entry is of the form:*)
(*	{option,default,1,pattern} -- option which applies to whole panel (no derived suboptions)*)
(*	{option,default,2,pattern,xymode} -- option which applies to horizontal/vertical directions (suboptions X and Y)*)
(*	{option,default,4,pattern,xymode,mirror} -- option which applies to each edge (suboptions X, Y, XX, and YY)*)
(**)
(*xymode:*)
(*	PanelOptionXYUnique -- separate values must be given for X and Y (except the single value None may be given, presuming that is a legitimate value)*)
(*	PanelOptionXYSame -- if a single value is given, it is used for both X and Y*)
(*	PanelOptionXYRange -- special treatment for region extension parameters*)
(**)
(*mirror:*)
(*	PanelOptionMirror -- if no value is given for the XX and YY axes, value is taken from X and Y axes*)
(*	value -- if no value is given for the XX and YY axes, this given value is used*)


FigurePanelOptionSet={

  (***** whole-panel options *****)

  (* INHERITED: all FigObject options *)
  (* SPECIAL: object name -- default effectively ObjectName \[Rule] None, but given special handling, not in Options[FigurePanel] *)

  (* region *)
  {PanelRegion,All,1,FigRegionPattern},
  {LockPanelScale,False,1,LogicalPattern},

  (* background *)
  {Background,None,1,None|ColorDirectivePattern},
  {BackgroundOpacity,None,1,None|UnitIntervalPattern},
  {BackgroundDirectives,{},1,_List},

  (* panel letter label *)
  {PanelLetter,Automatic,1,_},
  {PanelLetterPosition,Automatic,1,Automatic|_?NumericQ|_List},
  Sequence@@Replace[
    BaseTextOptionList,
    {Option_,Patt_}:>{SidifyOptionName["PanelLetter"][Option],Default,1,Default|Patt},
    {1}
            ],

  (* panel letter sequence *)
  {PanelLetterBase,"a",1,(s_String)/;(StringLength[s]==1)},
  {PanelLetterOrigin,{1,1},1,{_Integer,_Integer}},
  {PanelLetterDimensions,Automatic,1,{_Integer,_Integer}|Automatic},  (* TODO *)
  {PanelLetterCorrection,0,1,_Integer},
  {PanelLetterDelimiters,{"(",")"},1,{_,_}},
  {PanelLetterDirection,Horizontal,1,Horizontal|Vertical},

  (* exterior edge mask *)
  {ExteriorEdgeMask,Automatic,1,Automatic|LogicalPattern|{{LogicalPattern,LogicalPattern},{LogicalPattern,LogicalPattern}}},

  (* panel content rendering *)
  (* note: options for FigCompositeElement *)
  {Clip,True,1,LogicalPattern},
  {Rasterize,False,1,LogicalPattern},
  {ImageResolution,300,1,_?Positive},

  (* panel geometry *)
  {RegionExtension,None,1,FigDeltaRegionPattern},
  {RegionDisplacement,None,1,FigDisplacementPattern},

  (* multipanel inclusion *)
  (*{PanelPattern,{_,_},1,_},*)

  (***** plot range options *****)

  {PlotRange,{0,1},2,{_?NumericQ,_?NumericQ},PanelOptionXYUnique},
  {ExtendRange,None,2,IntervalParametersPattern,PanelOptionXYRange},

  (***** edge options *****)

  (* frame edge rendering *)
  {Frame,True,4,LogicalPattern,PanelOptionXYSame,PanelOptionMirror}, 

  (* frame edge masking *)
  {PanelEdgeExterior,Automatic,4,Automatic|LogicalPattern,PanelOptionXYSame,PanelOptionMirror},


  (* frame labels *)
  (* note: List forbidden as frame label to improve option validation in Multipanel,
Rule and RuleDelayed forbidden to allow list of rule values to work for FrameLabel *)
  {FrameLabel,None,4,Except[_List|_Rule|_RuleDelayed],PanelOptionXYUnique,PanelOptionMirror},
  {ShowFrameLabel,Exterior,4,Exterior|LogicalPattern,PanelOptionXYSame,False},
  {FrameLabelPosition,Automatic,4,Automatic|(_?NumericQ),PanelOptionXYSame,PanelOptionMirror},
  {TickLabelAllowance,Automatic,4,Automatic|NonNegativePattern,PanelOptionXYSame,PanelOptionMirror},
  (* ... FrameTextXXXX/FrameFontXXXX ... *)
  Sequence@@Replace[
    BaseTextOptionList,
    {Option_,Patt_}:>{SidifyOptionName["Frame"][Option],Default,4,Default|Patt,PanelOptionXYSame,PanelOptionMirror},
    {1}
            ],

  (* frame ticks *)
  {Ticks,Automatic,4,TickListSpecificationPattern,PanelOptionXYUnique,PanelOptionMirror},
  {TickLabelRange,Automatic,4,Automatic|All|NumericalPairPattern,PanelOptionXYUnique,PanelOptionMirror},
  {ShowTicks,True,4,LogicalPattern,PanelOptionXYSame,PanelOptionMirror},
  {ShowTickLabels,Exterior,4,Exterior|LogicalPattern,PanelOptionXYSame,False},
  {TickFontSizeFactor,0.85,4,NonNegativePattern,PanelOptionXYSame,PanelOptionMirror},
  {TickLengthReference,Automatic,4,Automatic|NonNegativePattern,PanelOptionXYSame,PanelOptionMirror},
  {TickLengthScale,1,4,NonNegativePattern,PanelOptionXYSame,PanelOptionMirror},
  (* ... TickTextXXXX/TickFontXXXX ... *)
  (* note: special treatment of font size -- Automatic for reduced tick font size *)
  Sequence@@Replace[
    BaseTextOptionList,
    {
      {Option:FontSize,Patt_}:>{SidifyOptionName["Tick"][Option],Automatic,4,Automatic|Default|Patt,PanelOptionXYSame,PanelOptionMirror},
      {Option_,Patt_}:>{SidifyOptionName["Tick"][Option],Default,4,Default|Patt,PanelOptionXYSame,PanelOptionMirror}
    },
    {1}
            ]

};


(* ::Subsection:: *)
(*Derived: List of default option rules*)


FigurePanelOptionRules=Flatten[
  Replace[
    FigurePanelOptionSet,
    {
      {Option_,Value_,1,Patt_}:>{
        Option->Value
                              },
      {Option_,Value_,2,Patt_,XYMode_}:>{
        SidifyOptionName["X"][Option]->Value,
        SidifyOptionName["Y"][Option]->Value,
        Option->Default
                                      },
      {Option_,Value_,4,Patt_,XYMode_,MirrorOption_}:>{
        SidifyOptionName["X"][Option]->Value,
        SidifyOptionName["Y"][Option]->Value,
        SidifyOptionName["XX"][Option]->Default,
        SidifyOptionName["YY"][Option]->Default,
        Option->Default
                                                    }
    },
    {1}
  ]
                       ];


(* ::Text:: *)
(*For easy inspection...*)


(* ::Input:: *)
(*SciDraw`Private`FigurePanelOptionRules;*)
(*Cases[%,_[ShowTicks,_]|_[XShowTicks,_]]*)


(* ::Subsection:: *)
(*Derived: List of validation patterns*)


FigurePanelOptionTests=Flatten[Replace[
  FigurePanelOptionSet,
  {
    {Option_,Value_,1,Patt_}:>{
      {Option,Patt}
                            },
    {Option_,Value_,2,Patt_,XYMode_}:>{
      {SidifyOptionName["X"][Option],Patt},
      {SidifyOptionName["Y"][Option],Patt},
      {Option,Default|Switch[XYMode,PanelOptionXYUnique,{Patt,Patt},PanelOptionXYRange,Automatic|RangeParametersPattern]}
                                    },
    {Option_,Value_,4,Patt_,XYMode_,MirrorOption_}:>{
      {SidifyOptionName["X"][Option],Patt},
      {SidifyOptionName["Y"][Option],Patt},
      {SidifyOptionName["XX"][Option],Default|Patt},
      {SidifyOptionName["YY"][Option],Default|Patt},
      (* for options where propagating a single value to both X and Y is nonsense, except in the case of None, only allow None as a single value *)
      {Option,Default|Switch[XYMode,PanelOptionXYUnique,None,PanelOptionXYSame,Patt]|{Patt,Patt}|{{Patt,Patt},{Patt,Patt}}|{Patt,Patt,Patt,Patt}
      }
                                                  }
  },
  {1}
                               ],
                               1
                       ];


(* ::Text:: *)
(*For easy inspection...*)


(* ::Input:: *)
(*SciDraw`Private`FigurePanelOptionTests*)


(* ::Subsection:: *)
(*Derived: List of option resolution descriptors*)


(* ::Text:: *)
(*original option specification plus sidified option names, for option types 2 and 4 only*)


FigurePanelOptionResolution=Replace[
  Cases[FigurePanelOptionSet,{_,_,2|4,___}],
  {
    {Option_,Value_,2,Patt_,XYMode_}:>{Option,Value,2,Patt,XYMode,{SidifyOptionName["X"][Option],SidifyOptionName["Y"][Option]}},
    {Option_,Value_,4,Patt_,XYMode_,MirrorOption_}:>{Option,Value,4,Patt,XYMode,MirrorOption,{{SidifyOptionName["Y"][Option],SidifyOptionName["YY"][Option]},{SidifyOptionName["X"][Option],SidifyOptionName["XX"][Option]}}
                                                    }
  },
  {1}
                            ];


(* ::Text:: *)
(*For easy inspection...*)


(* ::Input:: *)
(*SciDraw`Private`FigurePanelOptionResolution;*)
(*Cases[%,{ShowTicks,___}]*)


(* ::Subsection:: *)
(*Derived: List of option array upgrading descriptors*)


(* ::Text:: *)
(*Entries:*)
(*	{option,sense}*)
(**)
(*sense:*)
(*	1 -- horizontal axis option*)
(*	2 -- vertical axis option*)
(*	All -- undirected or true singlet option*)


FigurePanelOptionUpgrading=Flatten[
  Replace[
    FigurePanelOptionSet,
    {
      (*{Option_,Value_,1,Patt_}\[RuleDelayed]{},*)  (* PREVIOUSLY -- to ignore singlet options *)
      {Option_,Value_,1,Patt_}:>{{Option,All,Patt}},
      {Option_,Value_,2,Patt_,XYMode_}:>{
        {Option,All,Default|Patt},
        {SidifyOptionName["X"][Option],1,Patt},
        {SidifyOptionName["Y"][Option],2,Patt}
                                      },
      {Option_,Value_,4,Patt_,XYMode_,MirrorOption_}:>{
        {Option,All,Default|Patt},
        {SidifyOptionName["X"][Option],1,Patt},
        {SidifyOptionName["Y"][Option],2,Patt},
        {SidifyOptionName["XX"][Option],1,Default|Patt},
        {SidifyOptionName["YY"][Option],2,Default|Patt}
                                                    }
    },
    {1}
  ],
  1
                           ];


(* ::Text:: *)
(*For easy inspection...*)


(* ::Input:: *)
(*(*SciDraw`Private`FigurePanelOptionUpgrading*)
 (*Cases[%,{ShowTicks|XShowTicks,___}]*)*)


(* ::Subsection:: *)
(*Derived: List of remaining singlet options*)


(*FigurePanelOptionSinglets=Join[First/@FigurePanelOptionSet,First/@Options[FigObject]];*)
FigurePanelOptionSinglets=Replace[Options[FigObject],((Option_->_)|(Option_:>_)):>{Option,All,_},{1}];


(* ::Text:: *)
(*For easy inspection...*)


(* ::Input:: *)
(*(*SciDraw`Private`FigurePanelOptionSinglets*)*)


(* ::Section:: *)
(*FigurePanel and Multipanel forward object declarations*)


(* ::Text:: *)
(*Interdependence:*)
(*	Panel constructor needs Multipanel methods*)
(*	Multipanel inherits options from Panel*)
(*	Multipanel inherits data members from Panel -- though most currently ignored*)


(* ::Subsection:: *)
(*Object declaration -- FigurePanel*)


DeclareFigClass[
  FigurePanel,
  {"Window","Background","PanelLayer","Ticks"},
  {},
  {Center,Left,Right,Bottom,Top}
];


DefineFigClassOptions[
  FigurePanel,
  Join[

    (* options which will receive standardized treatment (pattern checking, XY upgrades, etc.) *)
    FigurePanelOptionRules,

    (* alternative object naming syntax *)
    (* note: this default value is ignored but is included in Options so that ObjectName is a recognized option for FigurePanel when option lists are validated *)
    {ObjectName->None}
  ]
];


(* ::Subsection:: *)
(*Object declaration -- Multipanel*)


DeclareFigClass[
  Multipanel,
  FigurePanel,
  {"PanelRegionArray","PanelGapsExteriorArray","PanelOptionsArrayList","PanelUnit","CanvasRegion","Dimensions"},
  {},
  {Center,Left,Right,Bottom,Top}
];


DefineFigClassOptions[
  Multipanel,
  {

    (* array geometry *)
    XPanelSizes->1,YPanelSizes->1,
    XPanelGaps->0,YPanelGaps->0,
    XPanelGapsExterior->False,YPanelGapsExterior->False,
    Dimensions->{1,1}

  }
];


(* ::Section:: *)
(*FigAxis*)


(* ::Text:: *)
(*similar to FigBracket, with following differences:*)
(*	arrowheads default ShowTail->False and standard arrowhead dimension*)
(*	convenience label named "AxisLabel"*)
(*	add TailExtension/HeadExtension*)
(*	add ticks*)
(*	range should be specified *numerically* (though positioning coordinate can still be specified in generic fashion)*)


(* ::Text:: *)
(*Note: FigAxis declaration must come before FigurePanel code*)


(* ::Subsection:: *)
(*Object declaration*)


DeclareFigClass[
  FigAxis,
  {"Points","Side","TickLabelAllowance"},
  {},
  {"Axis"}
];
DefineFigClassOptions[
  FigAxis,
  {
    (* curve/arrowhead *)
    FigArrowheadOptions[False,True],

    (* axis special *)
    TailExtension->None,HeadExtension->10,

    (* tick control *)
    Ticks->None,
    TickLabelRange->Automatic,
    ShowTicks -> True,
    ShowTickLabels->True,  (* symbol from CustomTicks *)
    TickFontSizeFactor->0.85,
    TickLengthReference->Automatic,
    TickLengthScale->1,
    TickLabelAllowance->Automatic,
    FigDerivedLabelOptions["Tick",False,Automatic]
(* TODO FigDerivedOutlineOptions["Tick"] *)
  }
];


(* ::Subsection:: *)
(*Constructor*)


Constructor[Class:FigAxis,Self_Object][
  Side:(Left|Right|Bottom|Top),
  Coordinate:FigCoordinatePattern,
  Range:{FigCoordinatePattern,FigCoordinatePattern}|FigRegionPattern,
  Opts___?OptionQ
                                      ]:=FigObjectWrapper[Class,Self,{Opts},
                                                          Module[
                                                            {
                                                              CoordinateIndex,CanvasCoordinate,CanvasInterval,CanvasPoints,CanvasTail,CanvasHead,RangeUseableForTicks,
                                                              UsedTickFontSize,TickOptions,UsedTickLabelRange,UsedTickLengthReference,UsedTickLengthScale,TickList,
                                                              TickPosn,TickText,TickLengths,TickAttributes,
                                                              TickLineArt,TickPoint,TickEndPoints,TickOffset,TickLabelAnchor,
                                                              ComputedTickLabelAllowance,TextElement
                                                            },

                                                            (* validate extra options *)
                                                            FigCheckArrowheadOptions[Self];
                                                            FigCheckOption[Self,TickLabelRange,Automatic|All|NumericalPairPattern];
                                                            FigCheckOption[Self,TailExtension,ScalarParameterPattern,FigOptions];
                                                            FigCheckOption[Self,HeadExtension,ScalarParameterPattern,FigOptions];
                                                            FigCheckOption[Self,ShowTicks,LogicalPattern,FigOptions];
                                                            FigCheckOption[Self,ShowTickLabels,LogicalPattern,FigOptions];
                                                            FigCheckOption[Self,TickFontSizeFactor,NonNegativePattern,FigOptions];
                                                            FigCheckOption[Self,TickLengthReference,Automatic|NonNegativePattern,FigOptions];
                                                            FigCheckOption[Self,TickLengthScale,NonNegativePattern,FigOptions];
                                                            FigCheckOption[Self,TickFontSize,Automatic|FontSizePattern|Default,FigOptions];  (* allow TickFontSize special value Automatic *)
                                                            FigCheckOption[Self,TickLabelAllowance,Automatic|NonNegativePattern,FigOptions];
                                                            FigCheckDerivedLabelOptions[Self,"Tick",False,Automatic,Flatten[{TickFontSize->999,FigOptions}]];  (* ... which would fail standard test *)

                                                            (* determine horizontal/vertical direction *)
                                                            (* CoordinateIndex -- coordinate for *positioning* axis *)
                                                            CoordinateIndex=Switch[
                                                              Side,
                                                              Left|Right,1,
                                                              Bottom|Top,2
                                                                            ];

                                                            (* convert coordinate and interval to canvas values *)
                                                            CanvasCoordinate=FigResolveCoordinate[Coordinate,CoordinateIndex];
                                                            CanvasInterval=Sort@Replace[
                                                              Range,
                                                              {
                                                                {x1:FigCoordinatePattern,x2:FigCoordinatePattern}:>{
                                                                  FigResolveCoordinate[x1,AntiCoordinateIndex[CoordinateIndex]],
                                                                  FigResolveCoordinate[x2,AntiCoordinateIndex[CoordinateIndex]]
                                                                                                                 },
                                                                r:FigRegionPattern:>FigResolveRegion[r][[AntiCoordinateIndex[CoordinateIndex]]]
                                                              }
                                                                                ];

                                                            (* generate curve points *)
                                                            (* curve always goes in "positive" coordinate direction *)
                                                            CanvasPoints=Switch[
                                                              Side,
                                                              Left|Right,{{CanvasCoordinate,CanvasInterval[[1]]},{CanvasCoordinate,CanvasInterval[[2]]}},
                                                              Bottom|Top,{{CanvasInterval[[1]],CanvasCoordinate},{CanvasInterval[[2]],CanvasCoordinate}}
                                                                         ];

                                                            (* do tail and head extension *)
                                                            CanvasTail=InterpolateSegment[
                                                              Take[CanvasPoints,2],
                                                              Tail,Absolute,
                                                              -UpgradeScalar[(TailExtension/.FigOptions)]
                                                                       ];
                                                            CanvasHead=InterpolateSegment[
                                                              Take[CanvasPoints,-2],
                                                              Head,Absolute,
                                                              +UpgradeScalar[(HeadExtension/.FigOptions)]
                                                                       ];
                                                            CanvasPoints=ReplacePart[CanvasPoints,{1->CanvasTail,-1->CanvasHead}];

                                                            (* save curve data *)
                                                            Self@SetPoints[CanvasPoints];
                                                            Self@SetSide[Side];

                                                            (* make curve line *)
                                                            FigLineElement[
                                                              {Line[CanvasPoints]},
                                                              FigOptions
                                                            ];

                                                            (* make arrowhead lines *)
                                                            FigLineElement[
                                                              {Line[
                                                                FigCurveArrowheadPoints[
                                                                  Self@MakeAnchor[Tail,None],
                                                                  Self@MakeAnchor[Head,None],
                                                                  Flatten[{
                                                                    FigOptions
                                                                    }]
                                                                ]
                                                               ]},
                                                              Flatten[{Dashing->None,FigOptions}]
                                                            ];

                                                            (* resolve tick text options *)
                                                            (* first we need to resolve the tick-specific options, now renamed as generic text options *)
                                                            TickOptions=Last[FigResolveDerivedLabelOptions["Tick",False,FigOptions]];
                                                            (* automatic determination of tick font size -- must be done using FigOptions, since in TickOptions TickFontSize is "upgraded" to FontSize *)
                                                            (* cases: 
TickFontSize->Automatic -- calculate FontSize from old FontSize times TickFontSizeFactor 
TickFontSize\[Rule]Default -- use FontSize
TickFontSize\[Rule]value -- set FontSize to TickFontSize
                                                             *)
                                                            TickOptions=Flatten[{
                                                              (* the final decision on tick font size *)
                                                              FontSize->ResolveOption[FontSize,{Automatic:>(TickFontSizeFactor/.FigOptions)*(FontSize/.FigOptions)},TickOptions],
                                                              (* the rest of the resolved text options *)
                                                              TickOptions,
                                                              (* and fall through to all other options from FigOptions *)
                                                              FigOptions
                                                              }];

                                                            (* resolve tick list *)
                                                            (* determine basic tick list -- given or computed from range *)
                                                            FigCheckOption[Self,Ticks,TickListSpecificationPattern,FigOptions];
                                                            RangeUseableForTicks=MatchQ[Range,NumericalPairPattern];
                                                            (* resolving tick label range -- tick label range may be given explicitly, else if left as Automatic the nominal axis Range is used, if this was given explicitly as a range of coordinates, else the flag value All is used, meaning no clipping of tick labels will be imposed *)
                                                            UsedTickLabelRange=ResolveOption[TickLabelRange,{Automatic->If[RangeUseableForTicks,Range,All]},FigOptions];
                                                            TickList=Switch[
                                                              (Ticks/.FigOptions),
                                                              None,{},
                                                              _List,(Ticks/.FigOptions),
                                                              Automatic,If[RangeUseableForTicks,LinTicks@@Range,{}],
                                                              _,If[RangeUseableForTicks,(Ticks/.FigOptions)@@Range,{}]
                                                                     ];
                                                            FigCheckValue[Self,TickList,_?TickListQ,"deduced set of tick marks"];
                                                            (* upgrade tick list to standard form *)
                                                            TickList=AugmentTicks[{0.0075,0},{},TickList];
                                                            (* suppress tick list if no-show *)
                                                            TickList=If[ShowTicks/.FigOptions,TickList,{}];
                                                            (* suppress tick labels if labels no-show *)
                                                            TickList=If[ShowTickLabels/.FigOptions,TickList,StripTickLabels[TickList]];
                                                            (* limit ticks to originally-specified coordinate range *)
                                                            If[RangeUseableForTicks,
                                                               TickList=LimitTickRange[Range,TickList]
                                                            ];
                                                            (* limit tick labels to specified coordinate range *)
                                                            If[UsedTickLabelRange=!=All,
                                                               TickList=LimitTickLabelRange[UsedTickLabelRange,TickList]
                                                            ];
                                                            If[
                                                              (Debug/.FigOptions),
                                                              Print["Axis ticks..."," Given: ",(Ticks/.FigOptions)," Label range: ", UsedTickLabelRange," Final tick list: ",TickList];
                                                            ];
                                                            (* convert ticks to canvas coordinates *)
                                                            (* Mathematica uses scaled coordinates for tick lengths. Here we use an averaged scaled coordinate based upon the average of the full plot's horizontal and vertical sizes, to ensure consistent tick lengths between horizontal and vertical axes and between panels. *)
                                                            UsedTickLengthReference=ResolveOption[TickLengthReference,{
                                                              Automatic:>Mean[-Subtract@@@(CurrentWindow[]@CanvasRegion[])]
                                                                                                  },FigOptions];
                                                            UsedTickLengthScale=(TickLengthScale/.FigOptions);
                                                            TickList=TransformTicks[
                                                              (FigResolveCoordinate[#,AntiCoordinateIndex[CoordinateIndex]]&)
 (*With[
{UsedAntiCoordinateIndex=AntiCoordinateIndex[CoordinateIndex]},
(FigResolveCoordinate[#,UsedAntiCoordinateIndex]&)
]*),
  (UsedTickLengthReference*UsedTickLengthScale*#&),
  TickList
 ];
                                                              (* iterate over ticks *)
                                                              (* accumulating line directives, emitting labels, and retaining label sizes *)
                                                              TickLineArt={};
                                                              ComputedTickLabelAllowance=0;
                                                              Do[

                                                                (* extract tick parameters *)
                                                                {TickPosn,TickText,TickLengths,TickAttributes}=TickData;

                                                                (* compute tick geometry *)
                                                                Switch[
                                                                  Side,
                                                                  Left,
                                                                  TickPoint={CanvasCoordinate,TickPosn};
                                                                  TickEndPoints={TickPoint+{TickLengths[[1]],0},TickPoint-{TickLengths[[2]],0}},
                                                                  Right,
                                                                  TickPoint={CanvasCoordinate,TickPosn};
                                                                  TickEndPoints={TickPoint-{TickLengths[[1]],0},TickPoint+{TickLengths[[2]],0}},
                                                                  Bottom,
                                                                  TickPoint={TickPosn,CanvasCoordinate};
                                                                  TickEndPoints={TickPoint+{0,TickLengths[[1]]},TickPoint-{0,TickLengths[[2]]}},
                                                                  Top,
                                                                  TickPoint={TickPosn,CanvasCoordinate};
                                                                  TickEndPoints={TickPoint-{0,TickLengths[[1]]},TickPoint+{0,TickLengths[[2]]}}
                                                                ];

                                                                (* accumulate tick lines (with any style directive overrides) *)
                                                                (* suppressed if line length is null *)
                                                                If[
                                                                  Chop[Subtract@@TickEndPoints]!={0,0},
                                                                  AppendTo[TickLineArt,Join[TickAttributes,{Line[TickEndPoints]}]]
                                                                ];

                                                                (* process text *)
                                                                (* suppressed if label text is None or null string *)
                                                                TickText=Replace[TickText,{""->None}];
                                                                If[
                                                                  (TickText=!=None),

                                                                  (* emit text *)
                                                                  TickOffset=-NamedPointOffset[Side];
                                                                  TickLabelAnchor=FigAnchor[Absolute[TickPoint],TickOffset,0];
                                                                  TextElement=FigTextElement[TickLabelAnchor,TickText,TickOptions];

                                                                  (* retain dimension information *)
                                                                  (* allow for height on horizontal axis or width on vertical axis *)
                                                                  (* CAVEAT: This assumes tick label is not rotated using TextOrientation option. *)
                                                                  ComputedTickLabelAllowance=Max[ComputedTickLabelAllowance,2*(TextElement@GetRadius[])[[CoordinateIndex]]]
                                                                ],
                                                                {TickData,TickList}
                                                              ];
                                                              ComputedTickLabelAllowance=GetAutoOption[TickLabelAllowance,ComputedTickLabelAllowance,FigOptions];

                                                              (* save allowance for tick labels *)
                                                              Self@SetTickLabelAllowance[ComputedTickLabelAllowance];

                                                              (* emit tick lines all together *)
                                                              FigLineElement[TickLineArt,Flatten[{Dashing->None,FigOptions}]];

                                                                     ]
                                                          ];


                                                          (* ::Subsection:: *)
                                                          (*Methods*)


                                                          (* ::Text:: *)
                                                          (*convenience label "Axis" allows for width of tick labels*)


                                                          MakeAnchor[Class:FigAxis,Self_Object][Name:"Axis",Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                                                                        DisplacePoint[
                                                                                                                                          FigCurveAnchorFromPoints[
                                                                                                                                            Self@GetPoints[],
                                                                                                                                            Switch[Self@GetSide[],Left|Top,Left,Bottom|Right,Right],
                                                                                                                                            Arg
                                                                                                                                          ],
                                                                                                                                          Absolute[NamedPointOffset[Self@GetSide[]]*(Self@GetTickLabelAllowance[])]
                                                                                                                                        ]
                                                                                                                   ]; 


                                                          (* ::Text:: *)
                                                          (*define standard curve anchors (needed, e.g., for arrowheads)*)


                                                          MakeAnchor[Class:FigAxis,Self_Object][Name:Except["Axis"],Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                                                                                FigCurveAnchorFromPoints[Self@GetPoints[],Name,Arg]
                                                                                                                           ];


                                                          (* ::Text:: *)
                                                          (*define standard bounding box*)


                                                          MakeBoundingBox[Class:FigAxis,Self_Object][]:=FigCurveBoundingBox[Self@GetPoints[]];


                                                          (* ::Section:: *)
                                                          (*FigurePanel*)


                                                          (* ::Subsection:: *)
                                                          (*Special constructor interface *)


                                                          (* ::Text:: *)
                                                          (*Initial approach: Uniformity would suggest defining figure panels through an object FigPanel.  The natural syntax would be FigPanel[body,...].*)
                                                          (**)
                                                          (*Problem: Unfortunately, since FigPanel is not the head symbol here, there is no mechanism for "holding" the evaluation of body until inside the FigPanel constructor.*)
                                                          (**)
                                                          (*Constraints: On the other hand, it is indispensable that FigPanel be derived from FigObject in the InheritOptions sense (so that it inherits *options* from FigObject) and it also be a derived class of FigObject in the MathObject sense (critically, so that it respects FigStyle styles, and for convenience so that the usual anchor methods can be used on it).*)
                                                          (**)
                                                          (*Solution: The compromise is to define a nonstandard "wrapper" function FigurePanel[body,...] for invocation by the user, in lieu of the standard constructor.  This wrapper will have attribute HoldFirst.  The wrapper then calls the standard constructor wrapper and thus in turn the standard constructor, which will be responsible for the usual object-ish tasks (like saving the panel's parameters as member data) but not for evaluating the body, which will happen last.*)
                                                          (**)
                                                          (*Nomenclature: To provide a reminder the nonstandard syntax, we will use a name which does not conform to the usual FigXxxx pattern, instead retaining the LevelScheme name FigurePanel.  By its resemblance to Figure[], this is distanced from being an "object" and appears more like a "scoping structure" like Figure[].*)
                                                          (**)
                                                          (*Object instance name argument: We still want to be able to define a name for the panel.  This will be passed as an optional parameter in the nonstandard wrapper's argument sequence.*)
                                                          (*	FigurePanel[*)
                                                          (*		body,*)
                                                          (*		options...*)
                                                          (*		];*)
                                                          (*		*)
                                                          (*The required argument $PrivateConstructorMarker is extra insurance against the object constructor ever being directly executed by the user.*)
                                                          (*The explicit reference to ConstructorWrapper is needed since the standard FigurePanel[name][...] syntax can conflict with the present nonstandard wrapper.*)


                                                          (* ::Text:: *)
                                                          (*General attributes*)


                                                          SetAttributes[FigurePanel,{HoldFirst}];


                                                          (* ::Text:: *)
                                                          (*Standalone (or inset) panel*)


                                                          FigurePanel[
                                                            Body_List,
                                                            Opts___?OptionQ
                                                          ]:=Module[
                                                            {Name,Self},

                                                            (* extract object name option *)
                                                            Name=Replace[ObjectName,Flatten[{Opts,ObjectName->None}]];
                                                            Self=Object[Name];

                                                            (* do ordinary construction *)
                                                            ConstructorWrapper[FigurePanel,Self][
                                                              HoldComplete[Body],None,
                                                              FilterRules[{Opts},{Except[Name]}], 
                                                              $PrivateConstructorMarker
                                                                                                ]

                                                             ];


                                                          (* ::Text:: *)
                                                          (*Panel in array*)


                                                          FigurePanel::nomulti="The second argument for FigurePanel was specified as `1`, which has the form of a pair of indices (or All), but no multipanel array is currently defined.";
                                                          FigurePanel::badindices="The given {row,column} index pair `1` is out of range, or, if it is meant as a pattern, it does not match any panel indices in the current multipanel array's range.  The current multipanel array has dimensions `2`.";


                                                          FigurePanel[
                                                            Body_List,
                                                            GivenPanelIndexPattern:Except[_?OptionQ],
                                                            Opts___?OptionQ
                                                          ]:=Module[
                                                            {Name,Self,CurrentPanelRegion,CurrentPanelOptions,MadeMatch,PanelIndexPattern},

                                                            (* check multipanel sanity *)
                                                            (* note: must use class rather than object in FigError since object metadata has not yet been registered *)
                                                            If[
                                                              CurrentMultipanel[]===None,
                                                              FigError[FigurePanel,"nomulti",GivenPanelIndexPattern]
                                                            ];

                                                            (* process panel index pattern *)
                                                            PanelIndexPattern=ReplaceSequential[GivenPanelIndexPattern,{All->{_,_}}];

                                                            (* do ordinary construction *)
                                                            MadeMatch=False;
                                                            Do[
                                                              If[
                                                                MatchQ[{r,s},PanelIndexPattern],

                                                                (* record match *)
                                                                MadeMatch=True;

                                                                (* retrieve multipanel parameters for this individual panel  *)
                                                                (* note: option retrieval must be done here, before options are used in FigObjectWrapper, rather than in constructor *)
                                                                CurrentPanelOptions=ExtractOptionsEntry[(CurrentMultipanel[]@GetPanelOptionsArrayList[]),{r,s}];

                                                                (* provide access to global variables PanelRowIndex, PanelColumnIndex, and PanelIndices *)
                                                                Block[
                                                                  {
                                                                    PanelRowIndex=r,PanelColumnIndex=s,
                                                                    PanelIndices={r,s},
                                                                    PanelSequenceNumber =({r,s}-{1,1}).{PanelColumns,1}+1
                                                                  },

                                                                  (* extract object name option -- here for possible access to panel variables*)
                                                                  Name=Replace[ObjectName,Flatten[{Opts,ObjectName->None}]];
                                                                  Self=Object[Name];

                                                                  (* call constructor *)
                                                                  ConstructorWrapper[FigurePanel,Self][
                                                                    HoldComplete[Body],PanelIndices,
                                                                    FilterRules[{Opts},{Except[Name]}], 
                                                                    CurrentPanelOptions,
                                                                    $PrivateConstructorMarker
                                                                                                      ]
                                                                ]

                                                              ],
                                                              {r,PanelRows},{s,PanelColumns}
                                                            ];

                                                            If[
                                                              !MadeMatch,
                                                              FigError[FigurePanel,"badindices",GivenPanelIndexPattern,(CurrentMultipanel[]@GetDimensions[])]
                                                            ];

                                                             ];


                                                          (* ::Subsection:: *)
                                                          (*Edge option resolution*)


                                                          (* ::Text:: *)
                                                          (*Resolution of PlotRange-like option*)


                                                          ResolvePanelOption[
                                                            Self_Object,
                                                            {Option_,Value_,2,Patt_,XYMode:PanelOptionXYUnique,XYOption:{XOption_,YOption_}},
                                                            FullOptions_List
                                                          ]:=(
                                                            Option->ResolveOption[Option,{Default:>(XYOption/.FullOptions)},FullOptions]
  );


                                                          (* ::Text:: *)
                                                          (*Resolution of ExtendRange-like option*)


                                                          ResolvePanelOption[
                                                            Self_Object,
                                                            {Option_,Value_,2,Patt_,XYMode:PanelOptionXYRange,XYOption:{XOption_,YOption_}},
                                                            FullOptions_List
                                                          ]:=(
                                                            Option->ResolveOption[Option,
                                                                                  {
                                                                                    (* process special single-symbol values *)
                                                                                    None:>0,
                                                                                    Automatic:>0.02,
                                                                                    (* Default: take XY values *)
                                                                                    Default:>(XYOption/.FullOptions),
                                                                                    (* then upgrade None or single-number values for the X and Y values to pairs *)
                                                                                    {x_,y_}:>{UpgradePairEqual[x],UpgradePairEqual[y]},
                                                                                    (* or upgrade a single-number (any {x,y} pair values will have already been handled above) given directly for ExtendRange *)
                                                                                    d_:>UpgradeRangeParameters[d]
                                                                                  },
                                                                                  FullOptions
                                                                    ]
  );


                                                          (* ::Text:: *)
                                                          (*Resolution of edge option*)


                                                          ResolvePanelOption[
                                                            Self_Object,
                                                            {Option_,Value_,4,Patt_,XYMode_,MirrorOption_,XXYYOption:{{YOption_,YYOption_},{XOption_,XXOption_}}},
                                                            FullOptions_List
                                                          ]:=(
                                                            Option->Module[
                                                              {UsedXOption,UsedXXOption,UsedYOption,UsedYYOption},

                                                              (* primary edges: get given option values *)
                                                              UsedXOption=ResolveOption[XOption,{},FullOptions];
                                                              UsedYOption=ResolveOption[YOption,{},FullOptions];

                                                              (* secondary edges: get given option value, then apply mirroring on value Default *)
                                                              UsedXXOption=ResolveOption[XXOption,{Default:>Switch[MirrorOption,PanelOptionMirror,UsedXOption,_,MirrorOption]},FullOptions];
                                                              UsedYYOption=ResolveOption[YYOption,{Default:>Switch[MirrorOption,PanelOptionMirror,UsedYOption,_,MirrorOption]},FullOptions];

                                                              (* master 2x3 list: get given value for singlet option, then process fall-throughs *)
                                                              Replace[
                                                                (Option/.FullOptions),
                                                                {
                                                                  (* Default as XY fallthrough *)
                                                                  Default->{{UsedYOption,UsedYYOption},{UsedXOption,UsedXXOption}},
                                                                  (* upgrade singlet value to all edges, if it survived the option validation, using either mirroring u\[Rule]{{u,u},{u,u}} or filler u\[Rule]{{u,X},{u,X}}  *)
                                                                  (u:Patt):>{
                                                                    {u,Switch[MirrorOption,PanelOptionMirror,u,_,MirrorOption]},
                                                                    {u,Switch[MirrorOption,PanelOptionMirror,u,_,MirrorOption]}
                                                                          },
                                                                  (* upgrade XY pair to all edges, using either mirroring {B,L}\[Rule]{{L,L},{B,B}} or filler {B,L}\[Rule]{{L,X},{B,X}} *)
                                                                  {x:Patt,y:Patt}:>{
                                                                    {y,Switch[MirrorOption,PanelOptionMirror,y,_,MirrorOption]},
                                                                    {x,Switch[MirrorOption,PanelOptionMirror,x,_,MirrorOption]}
                                                                                 },
                                                                  (* gracefully accept pre-6 legacy edge parameter specification, as {B,L,T,R}\[Rule]{{L,R},{B,T}} *)
                                                                  {b:Patt,l:Patt,t:Patt,r:Patt}:>{{l,r},{b,t}}
                                                                }
                                                              ]
                                                                    ]
  );


                                                          (* ::Subsection:: *)
                                                          (*Constructor*)


                                                          FigurePanel::prange="The panel plot range `1` has zero width or height.  (This plot range was obtained after applying the options PlotRange, XPlotRange, YPlotRange, ExtendRange, XExtendRange, and YExtendRange.)";


                                                          Constructor[Class:FigurePanel,Self_Object][HeldBody_,PanelIndices:(None|{r_Integer,s_Integer}),Opts___?OptionQ,$PrivateConstructorMarker]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                                                                                                                                      Module[
                                                                                                                                                                                                                        {
                                                                                                                                                                                                                          ResolvedCompositeOptions,
                                                                                                                                                                                                                          UsedCanvasRegion,ExtendedPlotRange,UsedPlotRange,BaseExteriorEdgeMask,DerivedExteriorEdgeMask,UsedExteriorEdgeMask,
                                                                                                                                                                                                                          EffectiveBackground,Window,
                                                                                                                                                                                                                          CommonAxisOptions,CompositeAxisOptions,
                                                                                                                                                                                                                          UsedPanelLetter,UsedPanelLetterDimensions,UsedPanelLetterPosition,PanelLayer,
                                                                                                                                                                                                                          StartLetterCode,DerivedPanelLetterCode,DerivedPanelLetter,
                                                                                                                                                                                                                          WindowGraphicalElementList,Rows,Columns,GivenPanelRegion,
                                                                                                                                                                                                                          PanelGapsExteriorArray,NominalCanvasRegion,NominalWindow
                                                                                                                                                                                                                        },

                                                                                                                                                                                                                        (* automated option handling *)

                                                                                                                                                                                                                        (* validate options *)
                                                                                                                                                                                                                        (FigCheckOption[Self,#1,#2,FigOptions]&)@@@FigurePanelOptionTests;

                                                                                                                                                                                                                        (* realize composite options as {x,y} or {{l,r},{t,b}} values *)
                                                                                                                                                                                                                        ResolvedCompositeOptions=(ResolvePanelOption[Self,#,FigOptions]&)/@FigurePanelOptionResolution;
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          (Debug/.FigOptions),
                                                                                                                                                                                                                          Print["  ","Panel composite options: ",ResolvedCompositeOptions]
                                                                                                                                                                                                                        ];

                                                                                                                                                                                                                        (* option postprocessing *)

                                                                                                                                                                                                                        (* derive region *)
                                                                                                                                                                                                                        GivenPanelRegion=If[
                                                                                                                                                                                                                          PanelIndices===None,
                                                                                                                                                                                                                          (PanelRegion/.FigOptions),
                                                                                                                                                                                                                          (CurrentMultipanel[]@GetPanelRegionArray[])[[r,s]]
                                                                                                                                                                                                                                         ];

                                                                                                                                                                                                                        UsedCanvasRegion=FigResolveRegion@AdjustRegion[GivenPanelRegion,FilterRules[FigOptions,{RegionExtension,RegionDisplacement}]];
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          (Debug/.FigOptions),
                                                                                                                                                                                                                          Print["Region diagnostics:"," indices ",PanelIndices," given region ",GivenPanelRegion," panel extension ",(RegionExtension/.FigOptions)," panel displacement ",(RegionDisplacement/.FigOptions)," adjusted region ",FullForm[UsedCanvasRegion]]
                                                                                                                                                                                                                        ];

                                                                                                                                                                                                                        (* derive plot range *)
                                                                                                                                                                                                                        ExtendedPlotRange=ExtendRegion[(PlotRange/.ResolvedCompositeOptions),(ExtendRange/.ResolvedCompositeOptions),Scaled];
                                                                                                                                                                                                                        (* if scale is locked against canvas region extension, then true coordinate range (i.e., extended from nominal coordinates according to ExtendRange option) must then also be extended by same fractional amounts as canvas region -- but this is hard to calculate, since region extension may be expressed in either absolute or relative form; also, we might want to lock origin against a canvas region displacement *)
                                                                                                                                                                                                                        (* so, instead, rely upon existing window transformation machinery -- create a window based on the unmodified canvas region, then see what user coordinates would cover modified canvas region *)
                                                                                                                                                                                                                        (*XYLockingMask={1,1}; (* TODO make controllable *)
UsedPlotRange=ExtendRegion[UsedPlotRange,XYLockingMask*(RegionExtension/.FigOptions),Scaled];*)
                                                                                                                                                                                                                        UsedPlotRange=If[
                                                                                                                                                                                                                          (LockPanelScale/.FigOptions),
                                                                                                                                                                                                                          NominalCanvasRegion=FigResolveRegion[GivenPanelRegion];
                                                                                                                                                                                                                          NominalWindow=FigWindow[NominalCanvasRegion,ExtendedPlotRange];
                                                                                                                                                                                                                          TransformRegion[NominalWindow@InverseTFunction[],UsedCanvasRegion],
                                                                                                                                                                                                                          ExtendedPlotRange
                                                                                                                                                                                                                                      ];

                                                                                                                                                                                                                        (* trap zero-sized range as error condition *)
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Chop[Min[Abs[Subtract@@@UsedPlotRange]]]==0,
                                                                                                                                                                                                                          FigError[FigurePanel,"prange",UsedPlotRange]
                                                                                                                                                                                                                        ];

                                                                                                                                                                                                                        (* define window *)
                                                                                                                                                                                                                        Window=FigWindow[UsedCanvasRegion,UsedPlotRange];
                                                                                                                                                                                                                        Self@SetWindow[Window];

                                                                                                                                                                                                                        (* retrieve multipanel array dimensions *)
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          PanelIndices=!=None,
                                                                                                                                                                                                                          {Rows,Columns}=CurrentMultipanel[]@GetDimensions[]
                                                                                                                                                                                                                        ];

                                                                                                                                                                                                                        (* define effective background for objects displayed in this panel *)
                                                                                                                                                                                                                        EffectiveBackground=If[
                                                                                                                                                                                                                          (Show/.FigOptions)&&((Background/.FigOptions)=!=None),
                                                                                                                                                                                                                          (Background/.FigOptions),
                                                                                                                                                                                                                          CurrentBackground[]
                                                                                                                                                                                                                                            ];
                                                                                                                                                                                                                        Self@SetBackground[EffectiveBackground];

                                                                                                                                                                                                                        (* choose layer for panel *)
                                                                                                                                                                                                                        (* CAVEAT: if predefined labels are ever declared for the panel as a whole, they will not respect this layer. *)
                                                                                                                                                                                                                        PanelLayer=ResolveOption[Layer,{Automatic:>$FigBackgroundLayer},FigOptions];
                                                                                                                                                                                                                        Self@SetPanelLayer[Layer];

                                                                                                                                                                                                                        (* set up option handling for frame edges *)

                                                                                                                                                                                                                        (* first handle a few options with values which are shared by all edges *)
                                                                                                                                                                                                                        CommonAxisOptions={
                                                                                                                                                                                                                          (* override of FigAxis special options to be appropriate to frame *)
                                                                                                                                                                                                                          ShowTicks -> True, (*already implemented as mask on Ticks *)
                                                                                                                                                                                                                          ShowHead->False,ShowTail->False,HeadExtension->None,TailExtension->None,
                                                                                                                                                                                                                          (* override of FigAxis special options by FigurePanel options *)
                                                                                                                                                                                                                          FilterRules[FigOptions,Options[FigAxis]]
                                                                                                                                                                                                                        };

(* identify edges which should count as "exterior" edges of panel *)
(*
(1) Base values (BaseExteriorEdgeMask): For standalone panel, all edges are exterior.  For multipanel array, takes into account position in array and XYPanelGapsExterior options.  

(2) Derived value (DerivedExteriorEdgeMask): Respect XYPanelEdgeExterior options, where value Automatic for an edge means to use base value.

(3) Final value (UsedExteriorEdgeMask): Respect legacy ExteriorEdgeMask option.  Automatic means to use above derived value.  Single logical value oapplies to all edges.  Or full 2x2 list of logical values is used verbatim.
 *)
PanelGapsExteriorArray=(CurrentMultipanel[]@GetPanelGapsExteriorArray[]);
                                                                                  BaseExteriorEdgeMask=If[
                                                                                    PanelIndices===None,
                                                                                    (* standalone panel *)
                                                                                    {{True,True},{True,True}},
                                                                                    (* panel in array *)
                                                                                    {
                                                                                      {
                                                                                        (s==1)||((s>1)&&PanelGapsExteriorArray[[1]][[s-1]]),
                                                                                        (s==Columns)||((s<Columns)&&PanelGapsExteriorArray[[1]][[s]])
                                                                                      },{
                                                                                        (r==Rows)||((r<Rows)&&PanelGapsExteriorArray[[2]][[r]]),
                                                                                        (r==1)||((r>1)&&PanelGapsExteriorArray[[2]][[r-1]])
                                                                                      }
                                                                                    }
                                                                                                       ];
                                                                                  (*Print[BaseExteriorEdgeMask];*)
                                                                                  DerivedExteriorEdgeMask=ResolveAutomaticEdgeOption[
                                                                                    (PanelEdgeExterior/.ResolvedCompositeOptions),
                                                                                    BaseExteriorEdgeMask
                                                                                                          ];
                                                                                  (*Print[DerivedExteriorEdgeMask];*)
                                                                                  UsedExteriorEdgeMask=ResolveOption[ExteriorEdgeMask,{(x:LogicalPattern):>{{x,x},{x,x}},Automatic->DerivedExteriorEdgeMask},FigOptions];
                                                                                  (*Print[UsedExteriorEdgeMask];*)

                                                                                  (* deduce option values for the four edges and bundle them as arrays
	option\[Rule]{{left,right},{bottom,top}}
                                                                                   *)
                                                                                  CompositeAxisOptions=Flatten[{

                                                                                    (* Show -- edge display control *)
                                                                                    Show->If[
                                                                                      (Show/.FigOptions),
                                                                                      (Frame/.ResolvedCompositeOptions),
                                                                                      Table[False,{2},{2}]
                                                                                          ],

                                                                                    (* AxisLabel -- validate and process frame labels *)
                                                                                    AxisLabel->MaskEdgeOption[
                                                                                      (FrameLabel/.ResolvedCompositeOptions),
                                                                                      (ShowFrameLabel/.ResolvedCompositeOptions),
                                                                                      UsedExteriorEdgeMask,None
                                                                                               ],

                                                                                    (* AxisLabelPosition -- validate and process frame label positions *)
                                                                                    (* each position *must* be Automatic or a number, not a list, for meaningful upgrading *)
                                                                                    AxisLabelPosition->(FrameLabelPosition/.ResolvedCompositeOptions),
                                                                                    TickLabelAllowance->(TickLabelAllowance/.ResolvedCompositeOptions),

                                                                                    (* AxisTextXXXX/AxisFontXXXX -- validate and process base text options for axis labels *)
                                                                                    Map[
                                                                                      (SidifyOptionName["Axis"][#1]->(SidifyOptionName["Frame"][#1]/.ResolvedCompositeOptions))&,
                                                                                                                                                                               (First/@BaseTextOptionList)
                                                                                    ],

                                                                                    (* Ticks -- validate and process frame ticks *)
                                                                                    Ticks->MaskEdgeOption[
                                                                                      Ticks/.ResolvedCompositeOptions,
                                                                                      ShowTicks/.ResolvedCompositeOptions,
                                                                                      UsedExteriorEdgeMask,None
                                                                                           ],

                                                                                    (* TickLabelRange -- resolve Automatic values as given plot range before extension *)
                                                                                    TickLabelRange->ResolveAutomaticEdgeOption[
                                                                                      TickLabelRange/.ResolvedCompositeOptions,
                                                                                      (* array trick to double up entries as {{yrange,yrange},{xrange,xrange}} *)
                                                                                      Reverse@Transpose[{(PlotRange/.ResolvedCompositeOptions),(PlotRange/.ResolvedCompositeOptions)}]
                                                                                                    ],

                                                                                    (* ShowTickLabels -- validate and process frame tick label masking *)
                                                                                    ShowTickLabels->MaskEdgeOption[
                                                                                      {{True,True},{True,True}},
                                                                                      ShowTickLabels/.ResolvedCompositeOptions,
                                                                                      UsedExteriorEdgeMask,
                                                                                      False
                                                                                                    ],
                                                                                    TickFontSizeFactor->(TickFontSizeFactor/.ResolvedCompositeOptions),
                                                                                    TickLengthReference->(TickLengthReference/.ResolvedCompositeOptions),
                                                                                    TickLengthScale->(TickLengthScale/.ResolvedCompositeOptions),

                                                                                    (* TickTextXXXX/TickFontXXXX -- validate and process base text options for tick labels *)
                                                                                    Map[
                                                                                      (SidifyOptionName["Tick"][#1]->(SidifyOptionName["Tick"][#1]/.ResolvedCompositeOptions))&,
                                                                                                                                                                              (First/@BaseTextOptionList)
                                                                                    ]

                                                                                    }
                                                                                                       ];

                                                                                  (* graphical element generation *)

                                                                                  (* panel background *)
                                                                                  FigCompositeElement[
                                                                                    CollectGraphicalElements[
                                                                                      FigPolygonElement[
                                                                                        {Rectangle@@Transpose[Window@CanvasRegion[]]},
                                                                                        Flatten[{
                                                                                          Layer->$FigBackgroundLayer,
                                                                                          ShowLine->False,
                                                                                          ShowFill->True,
                                                                                          FillColor->(Background/.FigOptions),
                                                                                          FillOpacity->(BackgroundOpacity/.FigOptions),
                                                                                          FillDirectives->(BackgroundDirectives/.FigOptions),
                                                                                          FigOptions
                                                                                          }]
                                                                                      ],
                                                                                      Window,EffectiveBackground
                                                                                    ],
                                                                                    Window,PanelLayer,
                                                                                    Clip->False,Debug->True
                                                                                  ];

                                                                                  (* evaluate body *)
                                                                                  (* option scoping, origin scoping, and all that wonderfull flattening/clipping/rasterization is imposed *)
                                                                                  (* note: could be elegant to use FigureGroup, but then must set $CurrentWindow and $CurrentBackground separately *)
                                                                                  FigCompositeElement[
                                                                                    CollectGraphicalElements[
                                                                                      ScopeOptions[ScopeOptionOverrides[
                                                                                        ReleaseHold[HeldBody]
                                                                                                   ]],
                                                                                      Window,EffectiveBackground
                                                                                    ],
                                                                                    Window,PanelLayer,
                                                                                    FilterRules[FigOptions,Options[FigCompositeElement]]
                                                                                  ];

                                                                                  (* panel letter *)
                                                                                  (* note that this cannot easily be done as an automatic predefined label, since it would then show up in the parent panel's layer *)
                                                                                  (* also, we override the name PanelLetter(Label) and position PanelLetter(Label)Position *)
                                                                                  (* and want to allow for Automatic label content *)
                                                                                  StartLetterCode=First[ToCharacterCode[PanelLetterBase/.FigOptions]];
                                                                                  UsedPanelLetterDimensions=ResolveOption[PanelLetterDimensions,{Automatic:>({Rows,Columns}-(PanelLetterOrigin/.FigOptions)+{1,1})},FigOptions];
                                                                                  DerivedPanelLetter=If[
                                                                                    PanelIndices===None,
                                                                                    None,
                                                                                    DerivedPanelLetterCode=StartLetterCode+(PanelLetterCorrection/.FigOptions)+({r,s}-(PanelLetterOrigin/.FigOptions)).Switch[
                                                                                      (PanelLetterDirection/.FigOptions),
                                                                                      Horizontal,{UsedPanelLetterDimensions[[2]],1},
                                                                                      Vertical,{1,UsedPanelLetterDimensions[[1]]}
                                                                                                                                                                                                      ];
                                                                                    Row[{First[PanelLetterDelimiters/.FigOptions],FromCharacterCode[DerivedPanelLetterCode],Last[PanelLetterDelimiters/.FigOptions]}]
                                                                                                     ];

                                                                                  UsedPanelLetter=ResolveOption[PanelLetter,{Automatic->DerivedPanelLetter},FigOptions];
                                                                                  UsedPanelLetterPosition=ResolveOption[PanelLetterPosition,{Automatic->{},(x_?NumericQ):>{x}},FigOptions];
                                                                                  FigCompositeElement[
                                                                                    CollectGraphicalElements[
                                                                                      FigSpawnAttachedLabel[
                                                                                        Self,"PanelLetter",
                                                                                        {UsedPanelLetter,UsedPanelLetterPosition,Last@FigResolveDerivedLabelOptions["PanelLetter",False,FigOptions]},
                                                                                        FigOptions
                                                                                      ],
                                                                                      Window,EffectiveBackground
                                                                                    ],
                                                                                    Window,PanelLayer,
                                                                                    Clip->False
                                                                                  ];

                                                                                  (* panel frame *)
                                                                                  FigCompositeElement[
                                                                                    CollectGraphicalElements[
                                                                                      {
                                                                                        (* spawn frame edges *)
                                                                                        FigAxis[
                                                                                          Left,UsedPlotRange[[1,1]],UsedPlotRange[[2]],
                                                                                          ExtractOptionsEntry[CompositeAxisOptions,{1,1}],
                                                                                          CommonAxisOptions
                                                                                        ];
                                                                                        FigAxis[
                                                                                          Right,UsedPlotRange[[1,2]],UsedPlotRange[[2]],
                                                                                          ExtractOptionsEntry[CompositeAxisOptions,{1,2}],
                                                                                          CommonAxisOptions
                                                                                        ];
                                                                                        FigAxis[
                                                                                          Bottom,UsedPlotRange[[2,1]],UsedPlotRange[[1]],
                                                                                          ExtractOptionsEntry[CompositeAxisOptions,{2,1}],
                                                                                          CommonAxisOptions
                                                                                        ];
                                                                                        FigAxis[
                                                                                          Top,UsedPlotRange[[2,2]],UsedPlotRange[[1]],
                                                                                          ExtractOptionsEntry[CompositeAxisOptions,{2,2}],
                                                                                          CommonAxisOptions
                                                                                        ];

                                                                                      },
                                                                                      Window,CurrentBackground[]
                                                                                    ],
                                                                                    Window,PanelLayer,
                                                                                    Clip->False
                                                                                  ];

]
                                                                                                                                                                                                                      ];


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*Syntax fallthrough*)


                                                                                                                                                                                                                      DeclareFigFallThroughError[FigurePanel];


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*Methods*)


                                                                                                                                                                                                                      MakeAnchor[Class:FigurePanel,Self_Object][Name:"PanelLetter",Arg:{Corner:FigOffsetPattern:TopLeft,Indent:NonNegativeIntervalParametersPattern:15}]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                                                                                                                                                                                                                                                                                                                               Module[
                                                                                                                                                                                                                                                                                                                                                                                                 {CanvasCorner,CanvasInset,CanvasPoint,UsedOffset},
                                                                                                                                                                                                                                                                                                                                                                                                 UsedOffset=FigResolveOffset[Corner];
                                                                                                                                                                                                                                                                                                                                                                                                 CanvasCorner=((Self@GetWindow[])@ScaledTFunction[])@(RescalingTransform[{{-1,+1},{-1,+1}},{{0,+1},{0,+1}}]@UsedOffset);
                                                                                                                                                                                                                                                                                                                                                                                                 CanvasInset=-UsedOffset*UpgradePairEqual[Indent];
                                                                                                                                                                                                                                                                                                                                                                                                 CanvasPoint=CanvasCorner+CanvasInset;
                                                                                                                                                                                                                                                                                                                                                                                                 FigAnchor[Absolute[CanvasPoint]]
                                                                                                                                                                                                                                                                                                                                                                                               ]
                                                                                                                                                                                                                                                                                                                                                                          ];


                                                                                                                                                                                                                      MakeAnchor[Class:FigurePanel,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                                                                                                                                                                                                                                  Module[
                                                                                                                                                                                                                                                                                                    {Region},
                                                                                                                                                                                                                                                                                                    Region=(Self@GetWindow[])@CanvasRegion[];
                                                                                                                                                                                                                                                                                                    FigRectangleAnchor[
                                                                                                                                                                                                                                                                                                      Mean/@Region,-(Subtract@@@Region)/2,{0,0},0,
                                                                                                                                                                                                                                                                                                      Name,Arg
                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                             ];


                                                                                                                                                                                                                      (* ::Text:: *)
                                                                                                                                                                                                                      (*Bounding box is canvas region of frame proper (not labels)*)


                                                                                                                                                                                                                      MakeBoundingBox[Class:FigurePanel,Self_Object][]:=(Self@GetWindow[])@CanvasRegion[];


                                                                                                                                                                                                                      (* ::Section:: *)
                                                                                                                                                                                                                      (*Multipanel*)


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*CurrentMultipanel*)


                                                                                                                                                                                                                      $CurrentMultipanel=None;


                                                                                                                                                                                                                      CurrentMultipanel[]:=$CurrentMultipanel;


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*Constructor interface *)


                                                                                                                                                                                                                      SetAttributes[Multipanel,{HoldFirst}];
                                                                                                                                                                                                                      Multipanel[
                                                                                                                                                                                                                        Body_,
                                                                                                                                                                                                                            Opts___?OptionQ
                                                                                                                                                                                                                      ]:=Module[
                                                                                                                                                                                                                        {Name,Self},

                                                                                                                                                                                                                        (* extract object name option *)
                                                                                                                                                                                                                        Name=Replace[ObjectName,Flatten[{Opts,ObjectName->None}]];

                                                                                                                                                                                                                        (* do ordinary construction *)
                                                                                                                                                                                                                        ConstructorWrapper[Multipanel,Object[Name]][
                                                                                                                                                                                                                          HoldComplete[Body],
                                                                                                                                                                                                                          FilterRules[{Opts},{Except[Name]}], 
                                                                                                                                                                                                                          $PrivateConstructorMarker
                                                                                                                                                                                                                                                                   ]

                                                                                                                                                                                                                         ];


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*Option array assembly*)


                                                                                                                                                                                                                      (* ::Text:: *)
                                                                                                                                                                                                                      (*Note:*)
                                                                                                                                                                                                                      (*	Axis	Sense	Dimension*)
                                                                                                                                                                                                                      (*	X	1	2 (columns)*)
                                                                                                                                                                                                                      (*	Y	2	1 (rows)*)
                                                                                                                                                                                                                      (*All -- undirected or true singlet option*)


                                                                                                                                                                                                                      VectorOfQ[x_,p_]:=VectorQ[x,MatchQ[#,p]&];
                                                                                                                                                                                                                      MatrixOfQ[x_,p_]:=MatrixQ[x,MatchQ[#,p]&];
                                                                                                                                                                                                                      VectorPattern[p_]:=(_?(VectorOfQ[#,p]&));
                                                                                                                                                                                                                      MatrixPattern[p_]:=(_?(MatrixOfQ[#,p]&));
                                                                                                                                                                                                                      RulePattern[p_]:=Rule[_,p]|(_RuleDelayed); (* RHS of RuleDelayed not forced to match p, since may only evaluate to p*)
                                                                                                                                                                                                                      RuleListPattern[p_]={RulePattern[p]..};    (* rule list cannot be null *)
                                                                                                                                                                                                                      MultipanelArrayPattern[p_]:=(RuleListPattern[p]|p|VectorPattern[p]|MatrixPattern[p]);


                                                                                                                                                                                                                      (* ::Text:: *)
                                                                                                                                                                                                                      (*Directed or singlet option -- given single value*)


                                                                                                                                                                                                                      MakePanelOptionArray[
                                                                                                                                                                                                                        Self_Object,
                                                                                                                                                                                                                        {Option_,Sense:(1|2|All),Patt_},
                                                                                                                                                                                                                        ArrayDimensions:{Rows_Integer,Columns_Integer},
                                                                                                                                                                                                                        Data_
                                                                                                                                                                                                                      ]/;MatchQ[Data,Patt]:=Option->Table[Data,{Rows},{Columns}];


                                                                                                                                                                                                                      (* ::Text:: *)
                                                                                                                                                                                                                      (*Directed option -- given vector*)


                                                                                                                                                                                                                      Multipanel::optvector="Expecting a list of `3` values for option `1`, but given `2`.";


                                                                                                                                                                                                                      MakePanelOptionArray[
                                                                                                                                                                                                                        Self_Object,
                                                                                                                                                                                                                        {Option_,Sense:(1|2),Patt_},
                                                                                                                                                                                                                        ArrayDimensions:{Rows_Integer,Columns_Integer},
                                                                                                                                                                                                                        Data_
                                                                                                                                                                                                                      ]/;MatchQ[Data,VectorPattern[Patt]]:=Module[
                                                                                                                                                                                                                        {},
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Length[Data]!=ArrayDimensions[[AntiCoordinateIndex[Sense]]],
                                                                                                                                                                                                                          FigError[Self,"optvector",Option,Length[Data],ArrayDimensions[[AntiCoordinateIndex[Sense]]]]
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        Option->Switch[Sense,1,Identity,2,Transpose]@Table[Data,{ArrayDimensions[[Sense]]}]
                                                                                                                                                                                                                                                           ];


                                                                                                                                                                                                                      (* ::Text:: *)
                                                                                                                                                                                                                      (*Directed or singlet option -- given array*)
                                                                                                                                                                                                                      (*	Note: However, array of true singlet options falls afoul of argument checking. (?)*)


                                                                                                                                                                                                                      Multipanel::optarray="Expecting an array of dimensions `3` for option `1`, but given an array of dimensions `2`.";


                                                                                                                                                                                                                      MakePanelOptionArray[
                                                                                                                                                                                                                        Self_Object,
                                                                                                                                                                                                                        {Option_,Sense:(1|2|All),Patt_},
                                                                                                                                                                                                                        ArrayDimensions:{Rows_Integer,Columns_Integer},
                                                                                                                                                                                                                        Data_
                                                                                                                                                                                                                      ]/;MatchQ[Data,MatrixPattern[Patt]]:=Module[
                                                                                                                                                                                                                        {},
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Dimensions[Data,2]!=ArrayDimensions,  (* need Dimension[...,2] in case entries are themselves lists *)
                                                                                                                                                                                                                          FigError[Self,"optarray",Option,Dimensions[Data],ArrayDimensions]
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        Option->Data
                                                                                                                                                                                                                                                           ];


                                                                                                                                                                                                                      (* ::Text:: *)
                                                                                                                                                                                                                      (*Directed or singlet option -- given rule list*)


                                                                                                                                                                                                                      Multipanel::optbadrule="Rule list \"`2`\" for option `1` yields an array `3` containing one or more invalid or undefined values.";
                                                                                                                                                                                                                      MakePanelOptionArray[
                                                                                                                                                                                                                        Self_Object,
                                                                                                                                                                                                                        {Option_,Sense:(1|2|All),Patt_},
                                                                                                                                                                                                                        ArrayDimensions:{Rows_Integer,Columns_Integer},
                                                                                                                                                                                                                        Data_
                                                                                                                                                                                                                      ]/;MatchQ[Data,RuleListPattern[Patt]]:=Module[
                                                                                                                                                                                                                        {ValueArray,FallThroughRuleList},

                                                                                                                                                                                                                        FallThroughRuleList=Append[Data,_->Undefined];
                                                                                                                                                                                                                        ValueArray=Table[
                                                                                                                                                                                                                          Replace[{i,j},FallThroughRuleList],
                                                                                                                                                                                                                          {i,Rows},{j,Columns}
                                                                                                                                                                                                                                   ];

                                                                                                                                                                                                                        (* warn of rule list not covering all panels -- but allow since not all panels might be drawn *)
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          (!FreeQ[ValueArray,Undefined,{2}]),
                                                                                                                                                                                                                          FigMessage[Self,"optundef",Option,Data,Position[ValueArray,Undefined,{2}]];
                                                                                                                                                                                                                        ];

                                                                                                                                                                                                                        (* do parameter validation, but allowing for Undefined parameters *)
                                                                                                                                                                                                                        (* TODO -- instead fall through to default value of option if fits single-panel pattern *)
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          (!MatchQ[ValueArray,MatrixPattern[Patt|Undefined]]),
                                                                                                                                                                                                                          FigError[Self,"optbad",Option,Data,ValueArray];
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        Option->ValueArray
                                                                                                                                                                                                                                                             ];


                                                                                                                                                                                                                      (* ::Text:: *)
                                                                                                                                                                                                                      (*Directed or singlet option -- fall through errors*)


                                                                                                                                                                                                                      Multipanel::optbad="Given unrecognized value or set of values \"`2`\" for option `1`.";  (* Could dump `3`... *)
                                                                                                                                                                                                                      Multipanel::optundef="The given set of option rules \"`2`\" for option `1` does not provide option values for one or more panels.  The unaccounted for panels have indices `3`.  This is okay if those panels are not actually ever drawn as part of the multipanel array, but attempting to draw any of these panels will result in an error (indicating the option has value \"Undefined\").";


                                                                                                                                                                                                                      MakePanelOptionArray[
                                                                                                                                                                                                                        Self_Object,
                                                                                                                                                                                                                        {Option_,Sense:(1|2),Patt_},
                                                                                                                                                                                                                        ArrayDimensions:{Rows_Integer,Columns_Integer},
                                                                                                                                                                                                                        Data_
                                                                                                                                                                                                                      ]/;MatchQ[Data,Except[Patt|VectorPattern[Patt]|MatrixPattern[Patt]|RuleListPattern[Patt]]]:=FigError[Self,"optbad",Option,Data];


                                                                                                                                                                                                                      MakePanelOptionArray[
                                                                                                                                                                                                                        Self_Object,
                                                                                                                                                                                                                        {Option_,Sense:All,Patt_},
                                                                                                                                                                                                                        ArrayDimensions:{Rows_Integer,Columns_Integer},
                                                                                                                                                                                                                        Data_
                                                                                                                                                                                                                      ]/;MatchQ[Data,Except[Patt|MatrixPattern[Patt]|RuleListPattern[Patt]]]:=FigError[Self,"optbad",Option,Data];


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*Constructor*)


                                                                                                                                                                                                                      UpgradeVector[u_?NumericQ,n_Integer]:=Table[u,{n}];
                                                                                                                                                                                                                      UpgradeVector[v:{_?NumericQ..},n_Integer]:=v;
                                                                                                                                                                                                                      UpgradeVector[u:LogicalPattern,n_Integer]:=Table[u,{n}];  (* for XYPanelGapsExterior *)
                                                                                                                                                                                                                      UpgradeVector[v:{LogicalPattern..},n_Integer]:=v;


                                                                                                                                                                                                                      Multipanel::numsizes="Expecting `3` `1`, but given `2`.";


                                                                                                                                                                                                                      Constructor[Class:Multipanel,Self_Object][HeldBody_,Opts___?OptionQ,$PrivateConstructorMarker]:=FigObjectWrapper[Class,Self,{Opts},
                                                                                                                                                                                                                                                                                                                                       Module[
                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                           CanvasRegion,
                                                                                                                                                                                                                                                                                                                                           RegionArray,OptionsArrayList,
                                                                                                                                                                                                                                                                                                                                           PanelSizes,PanelGaps,PanelGapsExteriorArray,PanelSizeAccumulator,PanelGapAccumulator,PanelIntervals,TotalInterval,ReversalFunction,CoordinateRegion,Window,
                                                                                                                                                                                                                                                                                                                                           ArrayDimensions,Rows,Columns
                                                                                                                                                                                                                                                                                                                                         },

                                                                                                                                                                                                                                                                                                                                         (* first order of business is to extract dimensions *)
                                                                                                                                                                                                                                                                                                                                         FigCheckOption[Dimensions,{_Integer?Positive,_Integer?Positive},FigOptions];
                                                                                                                                                                                                                                                                                                                                         {Rows,Columns}=ArrayDimensions=(Dimensions/.FigOptions);

                                                                                                                                                                                                                                                                                                                                         (* validate options *)
                                                                                                                                                                                                                                                                                                                                         FigCheckOption[XPanelSizes,_?NumericQ|{_?NumericQ..},FigOptions];
                                                                                                                                                                                                                                                                                                                                         FigCheckOption[YPanelSizes,_?NumericQ|{_?NumericQ..},FigOptions];
                                                                                                                                                                                                                                                                                                                                         FigCheckOption[XPanelGaps,_?NumericQ|{_?NumericQ..},FigOptions];
                                                                                                                                                                                                                                                                                                                                         FigCheckOption[YPanelGaps,_?NumericQ|{_?NumericQ..},FigOptions];
                                                                                                                                                                                                                                                                                                                                         FigCheckOption[XPanelGapsExterior,LogicalPattern,FigOptions];
                                                                                                                                                                                                                                                                                                                                         FigCheckOption[YPanelGapsExterior,LogicalPattern,FigOptions];
                                                                                                                                                                                                                                                                                                                                         If[
                                                                                                                                                                                                                                                                                                                                           MatchQ[(XPanelSizes/.FigOptions),_List]&&(Length[(XPanelSizes/.FigOptions)]!=Columns),
                                                                                                                                                                                                                                                                                                                                           FigError[Self,"numsizes","X panel sizes",Length[(XPanelSizes/.FigOptions)],Columns]
                                                                                                                                                                                                                                                                                                                                         ];
                                                                                                                                                                                                                                                                                                                                         If[
                                                                                                                                                                                                                                                                                                                                           MatchQ[(YPanelSizes/.FigOptions),_List]&&(Length[(YPanelSizes/.FigOptions)]!=Rows),
                                                                                                                                                                                                                                                                                                                                           FigError[Self,"numsizes","Y panel sizes",Length[(YPanelSizes/.FigOptions)],Rows]
                                                                                                                                                                                                                                                                                                                                         ];
                                                                                                                                                                                                                                                                                                                                         If[
                                                                                                                                                                                                                                                                                                                                           MatchQ[(XPanelGaps/.FigOptions),_List]&&(Length[(XPanelGaps/.FigOptions)]!=(Columns-1)),
                                                                                                                                                                                                                                                                                                                                           FigError[Self,"numsizes","X panel gaps",Length[(XPanelGaps/.FigOptions)],(Columns-1)]
                                                                                                                                                                                                                                                                                                                                         ];
                                                                                                                                                                                                                                                                                                                                         If[
                                                                                                                                                                                                                                                                                                                                           MatchQ[(YPanelGaps/.FigOptions),_List]&&(Length[(YPanelGaps/.FigOptions)]!=(Rows-1)),
                                                                                                                                                                                                                                                                                                                                           FigError[Self,"numsizes","Y panel gaps",Length[(YPanelGaps/.FigOptions)],(Rows-1)]
                                                                                                                                                                                                                                                                                                                                         ];
                                                                                                                                                                                                                                                                                                                                         If[
                                                                                                                                                                                                                                                                                                                                           MatchQ[(XPanelGapsExterior/.FigOptions),_List]&&(Length[(XPanelGapsExterior/.FigOptions)]!=(Columns-1)),
                                                                                                                                                                                                                                                                                                                                           FigError[Self,"numsizes","X panel gap interior/exterior flag",Length[(XPanelGapsExterior/.FigOptions)],(Columns-1)]
                                                                                                                                                                                                                                                                                                                                         ];
                                                                                                                                                                                                                                                                                                                                         If[
                                                                                                                                                                                                                                                                                                                                           MatchQ[(YPanelGapsExterior/.FigOptions),_List]&&(Length[(YPanelGapsExterior/.FigOptions)]!=(Rows-1)),
                                                                                                                                                                                                                                                                                                                                           FigError[Self,"numsizes","Y panel gaps interior/exterior flag",Length[(YPanelGapsExterior/.FigOptions)],(Rows-1)]
                                                                                                                                                                                                                                                                                                                                         ];

                                                                                                                                                                                                                                                                                                                                         (* store global information *)
                                                                                                                                                                                                                                                                                                                                         If[(Debug/.FigOptions),Print["  Setting dimensions..."]];
                                                                                                                                                                                                                                                                                                                                         Self@SetDimensions[ArrayDimensions];
                                                                                                                                                                                                                                                                                                                                         CanvasRegion=FigResolveRegion[(PanelRegion/.FigOptions)];
                                                                                                                                                                                                                                                                                                                                         Self@SetCanvasRegion[CanvasRegion];

                                                                                                                                                                                                                                                                                                                                         (* set up panel options *)
                                                                                                                                                                                                                                                                                                                                         If[(Debug/.FigOptions),Print["  Setting up panel options..."]];
                                                                                                                                                                                                                                                                                                                                         OptionsArrayList=
                                                                                                                                                                                                                                                                                                                                         Join[
                                                                                                                                                                                                                                                                                                                                           MakePanelOptionArray[Self,#,ArrayDimensions,(First[#]/.FigOptions)]&/@FigurePanelOptionUpgrading,
                                                                                                                                                                                                                                                                                                                                           MakePanelOptionArray[Self,#,ArrayDimensions,(First[#]/.FigOptions)]&/@FigurePanelOptionSinglets
                                                                                                                                                                                                                                                                                                                                         ];
                                                                                                                                                                                                                                                                                                                                         Self@SetPanelOptionsArrayList[OptionsArrayList];
                                                                                                                                                                                                                                                                                                                                         PanelGapsExteriorArray=Table[
                                                                                                                                                                                                                                                                                                                                           UpgradeVector[
                                                                                                                                                                                                                                                                                                                                             {XPanelGapsExterior,YPanelGapsExterior}[[CoordinateIndex]]/.FigOptions,
                                                                                                                                                                                                                                                                                                                                             ArrayDimensions[[AntiCoordinateIndex[CoordinateIndex]]]-1
                                                                                                                                                                                                                                                                                                                                           ],
                                                                                                                                                                                                                                                                                                                                           {CoordinateIndex,1,2}
                                                                                                                                                                                                                                                                                                                                                                ];
                                                                                                                                                                                                                                                                                                                                         Self@SetPanelGapsExteriorArray[PanelGapsExteriorArray];

                                                                                                                                                                                                                                                                                                                                         (* set up geometry  *)
                                                                                                                                                                                                                                                                                                                                         If[(Debug/.FigOptions),Print["  Setting up geometry..."]];
                                                                                                                                                                                                                                                                                                                                         (* collect relative sizes *)
                                                                                                                                                                                                                                                                                                                                         Do[
                                                                                                                                                                                                                                                                                                                                           PanelSizes[CoordinateIndex]=UpgradeVector[
                                                                                                                                                                                                                                                                                                                                             {XPanelSizes,YPanelSizes}[[CoordinateIndex]]/.FigOptions,
                                                                                                                                                                                                                                                                                                                                             ArrayDimensions[[AntiCoordinateIndex[CoordinateIndex]]]
                                                                                                                                                                                                                                                                                                                                                                       ];
                                                                                                                                                                                                                                                                                                                                           PanelGaps[CoordinateIndex]=UpgradeVector[
                                                                                                                                                                                                                                                                                                                                             {XPanelGaps,YPanelGaps}[[CoordinateIndex]]/.FigOptions,
                                                                                                                                                                                                                                                                                                                                             ArrayDimensions[[AntiCoordinateIndex[CoordinateIndex]]]-1
                                                                                                                                                                                                                                                                                                                                                                      ];
                                                                                                                                                                                                                                                                                                                                           (* convert these to coordinate intervals *)
                                                                                                                                                                                                                                                                                                                                           ReversalFunction=Switch[CoordinateIndex,1,Identity,2,Reverse];
                                                                                                                                                                                                                                                                                                                                           PanelSizeAccumulator=Prepend[Accumulate[ReversalFunction@PanelSizes[CoordinateIndex]],0];
                                                                                                                                                                                                                                                                                                                                           PanelGapAccumulator=Prepend[Accumulate[ReversalFunction@PanelGaps[CoordinateIndex]],0];
                                                                                                                                                                                                                                                                                                                                           PanelIntervals[CoordinateIndex]=ReversalFunction@Table[
                                                                                                                                                                                                                                                                                                                                             {PanelSizeAccumulator[[i]]+PanelGapAccumulator[[i]],PanelSizeAccumulator[[i+1]]+PanelGapAccumulator[[i]]},
                                                                                                                                                                                                                                                                                                                                             {i,1,ArrayDimensions[[AntiCoordinateIndex[CoordinateIndex]]]}
                                                                                                                                                                                                                                                                                                                                                                                            ];
                                                                                                                                                                                                                                                                                                                                           TotalInterval[CoordinateIndex]={0,Last[PanelSizeAccumulator]+Last[PanelGapAccumulator]},
                                                                                                                                                                                                                                                                                                                                           {CoordinateIndex,1,2}
                                                                                                                                                                                                                                                                                                                                         ];
                                                                                                                                                                                                                                                                                                                                         (* combine intervals into region information *)
                                                                                                                                                                                                                                                                                                                                         RegionArray=Table[
                                                                                                                                                                                                                                                                                                                                           {PanelIntervals[1][[s]],PanelIntervals[2][[r]]},
                                                                                                                                                                                                                                                                                                                                           {r,Rows},{s,Columns}
                                                                                                                                                                                                                                                                                                                                                     ];
                                                                                                                                                                                                                                                                                                                                         Self@SetPanelRegionArray[RegionArray];
                                                                                                                                                                                                                                                                                                                                         CoordinateRegion=Table[TotalInterval[CoordinateIndex],{CoordinateIndex,1,2}];

                                                                                                                                                                                                                                                                                                                                         (* define window *)
                                                                                                                                                                                                                                                                                                                                         (* note: permits panel region, shift, and adjustments to be in natural "panel units" coordinates *)
                                                                                                                                                                                                                                                                                                                                         Window=FigWindow[CanvasRegion,CoordinateRegion];
                                                                                                                                                                                                                                                                                                                                         Self@SetWindow[Window];

                                                                                                                                                                                                                                                                                                                                         (* evaluate body *)
                                                                                                                                                                                                                                                                                                                                         If[(Debug/.FigOptions),Print["  Evaluating multipanel body..."]];
                                                                                                                                                                                                                                                                                                                                         (* providing access to global variables PanelRows and PanelColumns *)
                                                                                                                                                                                                                                                                                                                                         Block[
                                                                                                                                                                                                                                                                                                                                           {
                                                                                                                                                                                                                                                                                                                                             $CurrentWindow=Window,
                                                                                                                                                                                                                                                                                                                                             $CurrentMultipanel=Self,
                                                                                                                                                                                                                                                                                                                                             PanelRows=Rows,PanelColumns=Columns
                                                                                                                                                                                                                                                                                                                                           },
                                                                                                                                                                                                                                                                                                                                           ReleaseHold[HeldBody]
                                                                                                                                                                                                                                                                                                                                         ];

                                                                                                                                                                                                                                                                                                                                       ]
                                                                                                                                                                                                                                                                                                                      ];


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*Syntax fallthrough*)


                                                                                                                                                                                                                      DeclareFigFallThroughError[Multipanel];


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*Methods*)


                                                                                                                                                                                                                      MakeAnchor[Class:Multipanel,Self_Object][Name_,Arg_]:=FigMakeAnchorWrapper[Class,Self,Name,Arg,
                                                                                                                                                                                                                                                                                                 Module[
                                                                                                                                                                                                                                                                                                   {Region},
                                                                                                                                                                                                                                                                                                   Region=Self@GetCanvasRegion[];
                                                                                                                                                                                                                                                                                                   FigRectangleAnchor[
                                                                                                                                                                                                                                                                                                     Mean/@Region,-(Subtract@@@Region)/2,{0,0},0,
                                                                                                                                                                                                                                                                                                     Name,Arg
                                                                                                                                                                                                                                                                                                   ]
                                                                                                                                                                                                                                                                                                 ]
                                                                                                                                                                                                                                                                            ];


                                                                                                                                                                                                                      (* ::Text:: *)
                                                                                                                                                                                                                      (*Bounding box is canvas region of multipanel array proper (not labels)*)


                                                                                                                                                                                                                      MakeBoundingBox[Class:Multipanel,Self_Object][]:=(Self@GetCanvasRegion[]);


                                                                                                                                                                                                                      (* ::Section:: *)
                                                                                                                                                                                                                      (*Value setting utilities*)


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*SetByRow, SetByColumn, etc.*)


                                                                                                                                                                                                                      SetAttributes[SetByPanelRow,HoldFirst];
                                                                                                                                                                                                                      SetByPanelRow::values="Value list for `1` was given as `2`, which is not long enough to provide a value for row `3`.";
                                                                                                                                                                                                                      SetByPanelRow[Variable:_Symbol,Values_List]:=Module[
                                                                                                                                                                                                                        {},
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Length[Values]<PanelRowIndex,
                                                                                                                                                                                                                          FigError[SetByPanelRow,"values",ToString[HoldForm[Variable]],Values,PanelRowIndex]
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        (Variable=Values[[PanelRowIndex]])
                                                                                                                                                                                                                                                                   ];


                                                                                                                                                                                                                      SetAttributes[SetByPanelColumn,HoldFirst];
                                                                                                                                                                                                                      SetByPanelColumn::values="Value list for `1` was given as `2`, which is not long enough to provide a value for column `3`.";
                                                                                                                                                                                                                      SetByPanelColumn[Variable:_Symbol,Values_List]:=Module[
                                                                                                                                                                                                                        {},
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Length[Values]<PanelColumnIndex,
                                                                                                                                                                                                                          FigError[SetByPanelColumn,"values",ToString[HoldForm[Variable]],Values,PanelColumnIndex]
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        (Variable=Values[[PanelColumnIndex]])
                                                                                                                                                                                                                                                                      ];


                                                                                                                                                                                                                      PairLessEqual[{x_,y_},{a_,b_}]:=TrueQ[Min[{a,b}-{x,y}]>=0];


                                                                                                                                                                                                                      SetAttributes[SetByPanelIndices,HoldFirst];
                                                                                                                                                                                                                      SetByPanelIndices::values="Value list for `1` was given as `2`, which has dimensions `4` and is not long or deep enough to provide a value for (row,column) indices `3`.";
                                                                                                                                                                                                                      SetByPanelIndices[Variable:_Symbol,Values_/;MatrixQ[Values,True&]]:=Module[
                                                                                                                                                                                                                        {},
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          !PairLessEqual[PanelIndices,Dimensions[Values,2]],
                                                                                                                                                                                                                          FigError[SetByPanelIndices,"values",ToString[HoldForm[Variable]],Values,PanelIndices,Dimensions[Values,2]]
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        (Variable=Extract[Values,PanelIndices])
                                                                                                                                                                                                                                                                                          ];
                                                                                                                                                                                                                      SetByPanelIndices::norule="Value list for `1` was given as `2`, which does not provide a value for (row,column) indices `3`.";
                                                                                                                                                                                                                      SetByPanelIndices[Variable:_Symbol,Rules:RuleListPattern[_]]:=Module[
                                                                                                                                                                                                                        {Value,FallThroughRuleList},
                                                                                                                                                                                                                        FallThroughRuleList=Append[Rules,_->Undefined];
                                                                                                                                                                                                                        Value=Replace[PanelIndices,Rules];
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Value===Undefined,
                                                                                                                                                                                                                          FigError[SetByPanelIndices,"norule",ToString[HoldForm[Variable]],Rules,PanelIndices];
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        Variable=Value
                                                                                                                                                                                                                                                                                    ];


                                                                                                                                                                                                                      SetAttributes[SetByPanelSequence,HoldFirst];
                                                                                                                                                                                                                      SetByPanelSequence::values="Value list for `1` was given as `2`, which is not long enough to provide a value for column `3`.";
                                                                                                                                                                                                                      SetByPanelSequence[Variable:_Symbol,Values_List]:=Module[
                                                                                                                                                                                                                        {},
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Length[Values]<PanelSequenceNumber,
                                                                                                                                                                                                                          FigError[SetByPanelSequence,"values",ToString[HoldForm[Variable]],Values,PanelSequenceNumber]
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        (Variable=Values[[PanelSequenceNumber]])
                                                                                                                                                                                                                                                                        ];


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*ByRow, ByColumn, etc.*)


                                                                                                                                                                                                                      ByPanelRow::values="Value list was given as `1`, which is not long enough to provide a value for row `2`.";
                                                                                                                                                                                                                      ByPanelRow[Values_List]:=Module[
                                                                                                                                                                                                                        {},
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Length[Values]<PanelRowIndex,
                                                                                                                                                                                                                          FigError[ByPanelRow,"values",Values,PanelRowIndex]
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        (Values[[PanelRowIndex]])
                                                                                                                                                                                                                                               ];


                                                                                                                                                                                                                      ByPanelColumn::values="Value list was given as `1`, which is not long enough to provide a value for column `2`.";
                                                                                                                                                                                                                      ByPanelColumn[Values_List]:=Module[
                                                                                                                                                                                                                        {},
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Length[Values]<PanelColumnIndex,
                                                                                                                                                                                                                          FigError[ByPanelColumn,"values",Values,PanelColumnIndex]
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        (Values[[PanelColumnIndex]])
                                                                                                                                                                                                                                                  ];


                                                                                                                                                                                                                      ByPanelIndices::values="Value list was given as `1`, which has dimensions `4` and is not long or deep enough to provide a value for (row,column) indices `2`.";
                                                                                                                                                                                                                      ByPanelIndices[Values_/;MatrixQ[Values,True&]]:=Module[
                                                                                                                                                                                                                        {},
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          !PairLessEqual[PanelIndices,Dimensions[Values,2]],
                                                                                                                                                                                                                          FigError[ByPanelIndices,"values",Values,PanelIndices,Dimensions[Values,2]]
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        (Extract[Values,PanelIndices])
                                                                                                                                                                                                                                                                      ];
                                                                                                                                                                                                                      ByPanelIndices::norule="Value list was given as `1`, which does not provide a value for (row,column) indices `2`.";
                                                                                                                                                                                                                      ByPanelIndices[Rules:RuleListPattern[_]]:=Module[
                                                                                                                                                                                                                        {Value,FallThroughRuleList},
                                                                                                                                                                                                                        FallThroughRuleList=Append[Rules,_->Undefined];
                                                                                                                                                                                                                        Value=Replace[PanelIndices,Rules];
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Value===Undefined,
                                                                                                                                                                                                                          FigError[ByPanelIndices,"norule",Rules,PanelIndices];
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        Value
                                                                                                                                                                                                                                                                ];


                                                                                                                                                                                                                      ByPanelSequence::values="Value list was given as `1`, which is not long enough to provide a value for column `2`.";
                                                                                                                                                                                                                      ByPanelSequence[Values_List]:=Module[
                                                                                                                                                                                                                        {},
                                                                                                                                                                                                                        If[
                                                                                                                                                                                                                          Length[Values]<PanelSequenceNumber,
                                                                                                                                                                                                                          FigError[ByPanelSequence,"values",Values,PanelSequenceNumber]
                                                                                                                                                                                                                        ];
                                                                                                                                                                                                                        (Values[[PanelSequenceNumber]])
                                                                                                                                                                                                                                                    ];


                                                                                                                                                                                                                      (* ::Section:: *)
                                                                                                                                                                                                                      (*End package*)


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*Exit private context*)


                                                                                                                                                                                                                      End[];


                                                                                                                                                                                                                      (* ::Subsection:: *)
                                                                                                                                                                                                                      (*Exit package context*)


                                                                                                                                                                                                                      Protect[Evaluate[$Context<>"*"]];
                                                                                                                                                                                                                      Unprotect[Evaluate[$Context<>"$*"]];
                                                                                                                                                                                                                      EndPackage[];
