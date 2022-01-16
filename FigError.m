(* ::Package:: *)

(*Header comments*)


(* :Title: FigError *)
(* :Context: SciDraw` *)
(* :Summary: Error passing and messaging utilities *)
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





(*Error handling*)


(*Message emitter*)


(*Note: Separate message function saves evaluation problems.  For instance, in an ordinary Message call,*)
(*	Message[Evaluate[Evaluate[ObjectClass[Self]]::figobjname],...]*)
(*would be required since both MessageName and Message are HoldFirst.  However, by passing the expressions through FigMessage, this reduces to*)
(*	FigMessage[Self,figobjname,...].*)
(*	*)
(*Note: Use of MessageName[f,m] is since f::m or f::Evaluate[m] does not accomplish proper replacement of parameter m.  This is presumably since the symbol::tag notation converts tag to the string "tag" before parameter replacement can take place.  On the other hand, with an argument of type m_Symbol, this form leads to full qualification of symbol (such as FigLine::SciDraw`Private`figobjname), which is also undesirable.  Therefore, require strings for message names.*)


FigMessage[f_Symbol,m_String,Args___]:=Message[MessageName[f,m],Args];
FigMessage[s_Object,m_String,Args___]:=FigMessage[ObjectClass[s],m,Args];


(*Object identifier message*)


(*Grammatical note: Use of article "a" rather than "an" presumes all object names will begin with consonants.*)


General::figobj="While `1` a `2` object named \"`3`\"...";
General::figobjanon="While `1` a `2` object (no name specified)...";


(*Note on evaluation:*)
(*	Context[Evaluate[ObjectName[Self]]] is since Context is HoldFirst.*)
(*	Message[Evaluate[Evaluate[ObjectClass[Self]]::figobjname] is since both MessageName and Message are HoldFirst.*)


FigWarnObject[Self_Object,Action_String]:=If[
  MatchQ[ObjectName[Self],_Symbol]&&Context[Evaluate[ObjectName[Self]]]==$ObjectInstanceContext,
  FigMessage[SciDraw,"figobjanon",Action,ObjectClass[Self]],
  FigMessage[SciDraw,"figobj",Action,ObjectClass[Self],ObjectName[Self]]
                                          ];


(*Error thrower*)


FigError[f_Symbol,m_String,Args___]:=Module[
  {},
  FigMessage[f,m,Args];
  Abort[]
                                     ];
FigError[s_Object,m_String,Args___]:=Module[
  {},
  FigWarnObject[s,"constructing"];
  FigMessage[s,m,Args];
  Abort[]
                                     ];
FigError[MakeAnchor,s_Object,m_String,Args___]:=Module[
  {},
  FigWarnObject[s,"constructing an anchor for"];
  FigMessage[s,m,Args];
  Abort[]
                                                ];
FigError[MakeBoundingBox,s_Object,m_String,Args___]:=Module[
  {},
  FigWarnObject[s,"constructing the bounding region for"];
  FigMessage[s,m,Args];
  Abort[]
                                                     ];


(*Object name validation*)


(*Formerly: Object names are recommended to be of the form*)
(*	string              EX: "shape"*)
(*	symbol            EX: A, MyObject*)
(*	symbol[args]   EX: Shape[35], Person["John", "Doe"]*)
(*where symbol is not in the System` context.  This final condition eliminates such insidious cases as*)
(*	1/2 = Rational[1,2]*)
(*	{1,2} = List[1,2].*)
(**)
(*Reconsideration: Actually, what was wrong with these forms?  Names such as 1/2 are awkward since Mathematica has multiple internal representations.  Head List is problematic if Flatten is applied to a list of object names.  These forms were problematic in SciDraw early development, when object names were used directly as coordinates, and thus an object name which could be mistaken for a coordinate was ambiguous.  However, object names are now only used in contexts in which only an object name would be accepted.*)
(**)
(*Presently: FigCheckObjectName is a no-op.*)


(*
NewSymbolPattern=((x_Symbol)/;(Context[x]=!="System`"));
SaneObjectNamePattern=NewSymbolPattern|NewSymbolPattern[___]|(_String);

General::figobjbadname="The object name `1`  is not of the recommended form and might therefore lead to syntax ambiguities.  For object names, it is recommended that you use a string (e.g., \"name\"), a new symbol (e.g., Name) not already defined as a Mathematica reserved symbol, or an expression headed by such a symbol (e.g., Name[1,2]).";

FigCheckObjectName[Self:Object[A_]/;!MatchQ[A,SaneObjectNamePattern]]:=
FigMessage[Self,"figobjbadname",ObjectName[Self]];
 *)


FigCheckObjectName[Self:Object[A_]]:=Null;


(*Error for function use outside figure*)


General::notinfigobj="Attempted to create figure object outside of a figure.";General::notinfigfcn="This function may only be used inside a Figure environment.";


FigCheckInFigure[Self_Object]/;(!SciDraw`Private`$InFigure):=FigError[Self,"notinfigobj"];
FigCheckInFigure[Self_Symbol]/;(!SciDraw`Private`$InFigure):=FigError[Self,"notinfigfcn"];


(*Error for function with unmatched argument sequence*)


General::figargs="Missing or unexpected arguments in `1`.";
General::figargsnull="One of the arguments in `1` is Null.  Check for extra commas among the arguments.";


SetAttributes[FigFallThroughError,HoldRest];
FigFallThroughError[Self_Symbol,Expr_]:=Module[
  {},
  If[
    Count[Hold[Expr],Null,{2}]>0,
    FigError[Self,"figargsnull",HoldForm[Expr]],
    FigError[Self,"figargs",HoldForm[Expr]]
  ]
                                        ];


DeclareFigFallThroughError[f_Symbol]:=(Expr:HoldPattern[f[___]]:=FigFallThroughError[f,Expr]);


(*Message suppression*)


(*SuppressMessage is a scoping structure which temporarily suppresses a specified message while evaluating an expression, without affecting the global On/Off status of the message.*)


SetAttributes[SuppressMessage,HoldAll];
SuppressMessage[MessageID_,Body_]:=Module[
  {WasOn,EvaluatedBody,Aborted},
  AbortProtect[

    (* record prior message status *)
    WasOn=Switch[
      Head[MessageID],
      String|MessageName,True,
      $Off,False
          ];

    (* evaluate body with message disabled *)
    Off[MessageID];
    Aborted=False;
    CheckAbort[
      EvaluatedBody=Body,
      Aborted=True
    ];

    (* restore message status *)
    If[WasOn,On[MessageID]];
  ];

  (* return value *)
  (* passes through abort, and also explicitly returns $Aborted in case Abort[] is suppressed *)
  If[Aborted,Abort[];$Aborted,EvaluatedBody]
                                   ];


(*Option and parameter validation*)


(*Option check against pattern*)


General::figbadopt="Option `1` has invalid value `2`.";


FigCheckOption[Caller:((_Object)|(_Symbol)),OptionName_Symbol,TestPattern_,OptionList_List]:=Module[
  {},
  (*
If[
OptionName\[Equal]Width,
Print["value ",(OptionName/.OptionList)," pattern ",FullForm[TestPattern]," match ",MatchQ[(OptionName/.OptionList),TestPattern]];
  ];
   *)
  If[
    !MatchQ[(OptionName/.OptionList),TestPattern],
    FigError[Caller,"figbadopt",OptionName,(OptionName/.OptionList)]
  ]
                                                                                             ];
FigCheckOption[SpecialMode:MakeAnchor,Caller:((_Object)|(_Symbol)),OptionName_Symbol,TestPattern_,OptionList_List]:=Module[
  {},
  If[
    !MatchQ[(OptionName/.OptionList),TestPattern],
    FigError[SpecialMode,Caller,"figbadopt",OptionName,(OptionName/.OptionList)]
  ]
                                                                                                                    ];


(*Value check against pattern*)


General::figbadvalue="The `1` has an invalid value `2`.";


FigCheckValue[Caller:((_Object)|(_Symbol)),Value_,TestPattern_,Description_String]:=Module[
  {},
  If[
    !MatchQ[Value,TestPattern],
    FigError[Caller,"figbadvalue",Description,Value]
  ]
                                                                                    ];


General::figbadvaluelist="One of the `1` has an invalid value `2`.";


FigCheckValueList[Caller:((_Object)|(_Symbol)),ValueList_List,TestPattern_,Description_String]:=Module[
  {Value},
  Do[
    If[
      !MatchQ[Value,TestPattern],
      FigError[Caller,"figbadvaluelist",Description,Value]
    ],
    {Value,ValueList}
  ]
                                                                                                ];


(*End package*)


(*Exit private context*)


End[];


(*Exit package context*)


Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
