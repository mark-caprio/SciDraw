(* ::Package:: *)

(* ::Section:: *)
(*Header comments*)


(* :Title: FigText *)
(* :Context: SciDraw` *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Summary: Miscellaneous text formatting utilities. *)
(* :Copyright: Copyright 2011, Mark A. Caprio *)
(* :Package Version: 0.0 *)
(* :Mathematica Version: 7.0 *)
(* :History:
Package MCText started December 2003, based upon functions written for LevelScheme.
Distributed with the LevelScheme package.
October 2010. Context changed to LevelScheme`.
May 2011.  Renamed from MCText to FigText.  Context changed to SciDraw`.
 *)


(* ::Section:: *)
(*Begin package*)


(* ::Subsection:: *)
(*Package context definition*)


BeginPackage["SciDraw`",SciDraw`Private`$ExternalContexts];


Unprotect[Evaluate[$Context<>"*"]];


(* ::Subsection:: *)
(*Begin private context*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Dependencies*)


(* ::Section:: *)
(*Mathematical text formatting*)


(* ::Subsection:: *)
(*LATEX-like text formatting*)


textup[x_]:=StyleForm[x,FontSlant->"Plain"];
textsl[x_]:=StyleForm[x,FontSlant->"Oblique"];
textit[x_]:=StyleForm[x,FontSlant->"Italic"];
textmd[x_]:=StyleForm[x,FontWeight->"Plain"];
textbf[x_]:=StyleForm[x,FontWeight->"Bold"];
textrm[x_]:=StyleForm[x,FontFamily->"Times"];
texttt[x_]:=StyleForm[x,FontFamily->"Courier"];
textsf[x_]:=StyleForm[x,FontFamily->"Helvetica"];


hspace[Lems_]:=AdjustmentBox["",BoxMargins->{{0,Lems},{0,0}}];


textsize[s_,x_]:=StyleForm[x,FontSize->s];
textcolor[c_,x_]:=StyleForm[x,FontColor->c];
texttracking[t_,x_]:=StyleForm[x,FontTracking->t];
textfamily[f_,x_]:=StyleForm[x,FontFamily->f];
texthidden[x_]:=StyleForm[x,ShowContents->False];


textsubscript[x_]:=SubscriptBox["",x];
textsuperscript[y_]:=SuperscriptBox["",y];
textsubsuperscript[x_,y_]:=SubsuperscriptBox["",x,y];


(* ::Text:: *)
(*Italic correction*)


Options[textit]={hspace->0};
textit[x_,Opts___]:=Module[
  {FullOpts=Flatten[{Opts,Options[textit]}]},
  AdjustmentBox[
    StyleForm[x,FontSlant->"Italic"],
    BoxMargins->{{0,hspace/.FullOpts},{0,0}}
  ]
                    ];


(* ::Subsection:: *)
(*General text formatting*)


StackText[Alignment_,Spacing_,Lines_List,Opts___?OptionQ]:=GridBox[{#}&/@Lines,ColumnAlignments->Alignment,RowSpacings->Spacing,Opts];


SuperPrimeBox[x_,n:(_Integer?NonNegative):1]:=SuperscriptBox[x,StringJoin[Table["\[Prime]",{n}]]];
(*SuperPrime[x_]:=Superscript[x,"\[Prime]"];*)
SuperPrime[x_,n:(_Integer?NonNegative):1]:=Superscript[x,StringJoin[Table["\[Prime]",{n}]]];


(* ::Subsection:: *)
(*Multiplet*)


Options[MultipletLabel]={EntrySeparator->",",Delimiter->{"(",")"}};
MultipletLabel[Values_List,Opts:OptionsPattern[]]:=Module[
  {DeducedEntrySeparator,DeducedDelimiter},

  DeducedDelimiter=Switch[
    OptionValue[Delimiter],
    None,{"",""},
    _,OptionValue[Delimiter]
                   ];
  DeducedEntrySeparator=Switch[
    OptionValue[EntrySeparator],
    None,"",
    _,OptionValue[EntrySeparator]
                        ];

  Row[Join[{DeducedDelimiter[[1]]},Riffle[Values,DeducedEntrySeparator],{DeducedDelimiter[[2]]}]]
                                                   ];


(* ::Input:: *)
(*(*MultipletLabel[{1,2,3}]*)
 (*MultipletLabel[{1,2,3},Delimiter\[Rule]{"[","]"}]*)
 (*MultipletLabel[{1,2,3},EntrySeparator\[Rule]None]*)
 (*MultipletLabel[{1,2,3},EntrySeparator\[Rule]None,Delimiter\[Rule]None]*)*)


(* ::Subsection:: *)
(*Sign character*)


SignString[x_?NumericQ]:=Switch[
  Sign[x],
  +1,"+",
  0,"",
  -1,"-"
                         ];
GradeSignString[g_?NumericQ]:=Switch[
  g,
  0,"+",
  1,"-"
                              ];


(* ::Subsection:: *)
(*Formatting of root-containing quantities entirely under radical*)


SetAttributes[Sqrtize,Listable];
Sqrtize[x_?NumericQ]:=Which[
  (*RationalQ[x],x,*)
  RationalQ[x^2],Sign[x]*SqrtBox[x^2],
  True,x
                      ];
SetAttributes[Radicalize,Listable];
Radicalize[x_?NumericQ,n_Integer]:=Which[
  (*RationalQ[x],x,*)
  RationalQ[x^n],Sign[x]*RadicalBox[Abs[x^n],n],
  True,x
                                   ];


RationalQ[x_]:=(IntegerQ[x]||(Head[x]===Rational));


(* ::Input:: *)
(*(*RationalQ[1]*)
 (*RationalQ[1/5]*)
 (*RationalQ[-1/5]*)
 (*RationalQ[Sqrt[5]]*)*)


(* ::Input:: *)
(*(*Sqrtize[1]//DisplayForm*)
 (*Sqrtize[1/5]//DisplayForm*)
 (*Sqrtize[-Sqrt[5]/2]//DisplayForm*)
 (*Sqrtize[Pi]//DisplayForm*)*)


(* ::Input:: *)
(*(*Radicalize[1,3]//DisplayForm*)
 (*Radicalize[-3^(1/3)/2,3]//DisplayForm*)*)


(* ::Subsection:: *)
(*String alignment and value extraction*)


Options[AlignmentBox]={
  AlignmentMarker->"&",
  Align->True,
  ColumnAlignments->{Right,Left},ColumnSpacings->0
};


BreakString[Separator_,Str_]:=Module[
  {PosnList},
  PosnList=Join[{{Null,0}},StringPosition[Str,Separator],{{StringLength[Str]+1,Null}}];

  Table[StringTake[Str,{PosnList[[i]][[2]]+1,PosnList[[i+1]][[1]]-1}],
        {i,1,Length[PosnList]-1}
  ]
                              ];

AlignmentBox[Str_,Opts___?OptionQ]:=Module[
  {
    FullOpts=Flatten[{Opts,Options[AlignmentBox]}]
  },

  CheckOption[Align,True|False,FullOpts];
  CheckOption[AlignmentMarker,_String,FullOpts];
  StyleBox[
    If[
      Align/.FullOpts,
      GridBox[
        {BreakString[(AlignmentMarker/.FullOpts),Str]},
        Sequence@@FilterRules[FullOpts,Options@GridBox]
      ],
      StringReplace[Str,(AlignmentMarker/.FullOpts)->""]
    ],
    Sequence@@FilterRules[FullOpts,Options@StyleBox]
  ]
                                    ];


(* ::Input:: *)
(*(*AlignmentBox["a&b",ColumnWidths\[Rule]{5,5},Align\[Rule]False]//DisplayForm*)
 (*AlignmentBox["a&b",ColumnWidths\[Rule]{5,5}]//DisplayForm*)
 (*AlignmentBox["aaaa&bbbb",ColumnWidths\[Rule]{5,5}]//DisplayForm*)*)


(* ::Output:: *)
(*\!\( *)
(*TagBox["ab",*)
(*DisplayForm]\)*)


(* ::Output:: *)
(*\!\( *)
(*TagBox[GridBox[{*)
(*{"a", "b"}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {Right, {Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},*)
(*GridBoxItemSize->{"Columns" -> {{5}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],*)
(*DisplayForm]\)*)


(* ::Output:: *)
(*\!\( *)
(*TagBox[GridBox[{*)
(*{"aaaa", "bbbb"}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {Right, {Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},*)
(*GridBoxItemSize->{"Columns" -> {{5}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],*)
(*DisplayForm]\)*)


LaTeXTableEntryValue[Value_?NumericQ]:=Value;
LaTeXTableEntryValue[Str_String]:=Module[
  {
    FirstNumericPosn,
    ErrorBarsPosn,
    Value
  },

  FirstNumericPosn=StringPosition[Str,{"+","-","0","1","2","3","4","5","6","7","8","9","."},1][[1,1]];
  ErrorBarsPosn=If[
    Length[StringPosition[Str,"(",1]]>=1,
    StringPosition[Str,{"(","\[PlusMinus]"},1][[1,1]],
    StringLength[Str]+1
                ];
  Value=ToExpression[
    StringReplace[
      StringTake[Str,{FirstNumericPosn,ErrorBarsPosn-1}],
      {"&"->""}
    ]
        ];
  If[!NumericQ[Value],
     Message[LaTeXTableEntryValue::notnumeric,Str,Value]
  ];
  Value
                                  ];


(* ::Subsection:: *)
(*Matrix formatting*)


Options[SubarrayEllipsis]={Padding->"\[CenterEllipsis]"};
SubarrayEllipsis[m_?MatrixQ,{Rows:(_Integer|Infinity),Columns:(_Integer|Infinity)},OptionsPattern[]]:=Module[
  {
    RowTrim,ColumnTrim,RowMax,ColumnMax,MatrixTrimmed
  },
  RowTrim=(Dimensions[m][[1]]>Rows);
  RowMax=Min[Dimensions[m][[1]],Rows];
  ColumnTrim=(Dimensions[m][[2]]>Columns);
  ColumnMax=Min[Dimensions[m][[2]],Columns];

  ArrayPad[
    m[[1;;RowMax,1;;ColumnMax]],
    {{0,If[RowTrim,1,0]},{0,If[ColumnTrim,1,0]}},
    OptionValue[Padding]
  ]
                                                                                                      ];
SubarrayEllipsis[m_List,{Rows:(_Integer|Infinity)},OptionsPattern[]]:=Module[
  {
    RowTrim,RowMax,MatrixTrimmed
  },
  RowTrim=(Dimensions[m][[1]]>Rows);
  RowMax=Min[Dimensions[m][[1]],Rows];

  ArrayPad[
    m[[1;;RowMax]],
    {{0,If[RowTrim,1,0]}},
    OptionValue[Padding]
  ]
                                                                      ];


(* ::Input:: *)
(*(*SubarrayEllipsis[Table[i,{i,10},{j,10}],{5,5}]//MatrixForm*)
 (*SubarrayEllipsis[Table[i,{i,10},{j,10}],{5,12}]//MatrixForm*)
 (*SubarrayEllipsis[Table[i,{i,5},{j,10}],{5,5}]//MatrixForm*)
 (*SubarrayEllipsis[Table[i,{i,10},{j,10}],{5,5},Padding\[Rule]"~"]//TableForm*)*)


(* ::Input:: *)
(*(*MatrixForm[SubarrayEllipsis[Table[i,{i,10},{j,10}],{5}],TableDepth\[Rule]1]*)*)


(* ::Subsection:: *)
(*Pagination*)


PageBreak[]:=CellPrint[Cell["",PageBreakBelow->True]];


(* ::Subsection:: *)
(*Unit labels*)


UnitsLabel[FactorSequence___]:=Module[
  {EntryList},

  (* create exponents *)
  EntryList=Replace[{FactorSequence},{u_,p_}:>Superscript[u,p],{1}];

  (* intersperse thin spaces *)
  EntryList=Riffle[EntryList,"\[ThinSpace]"(*ThinSpace*)];

  (* but undo spaces around solidus *)
  EntryList=Replace[EntryList,{pre___,"\[ThinSpace]"(*ThinSpace*),"/","\[ThinSpace]"(*ThinSpace*),post___}:>{pre,"/",post},{0}];

  (* create row *)
  Row[EntryList]
                               ];


(* ::Input:: *)
(*(*UnitsLabel[{textit["e"],2},{"fm",4}]*)
 (*textrm[%]//DisplayForm  (* see what it will look like in a label *)*)
 (*UnitsLabel["kg",{"m",2},"/",{"s",2}]*)
 (*textrm[%]//DisplayForm (* see what it will look like in a label *)*)*)


(* ::Section:: *)
(*Formatting for spectroscopy and nuclear physics*)


(* ::Subsection:: *)
(*Spectroscopic \[ScriptL] notation*)


SpectroscopicLetter[0]="s";
SpectroscopicLetter[1]="p";
SpectroscopicLetter[2]="d";
SpectroscopicLetter[3]="f";
SpectroscopicLetter[4]="g";
SpectroscopicLetter[5]="h";
SpectroscopicLetter[6]="i";
SpectroscopicLetter[7]="j";
SpectroscopicLetter[8]="k";


(* ::Subsection:: *)
(*nlj shell label*)


Options[ShellLabel]={Style->Italic};
ShellLabel[{n_,l_,j_},OptionsPattern[]]:=Row[{n,Subscript[Style[SpectroscopicLetter[l],OptionValue[Style]],SolidusFractionize[j]]}];
ShellLabel[{l_,j_},OptionsPattern[]]:=Row[{Subscript[Style[SpectroscopicLetter[l],OptionValue[Style]],SolidusFractionize[j]]}];


(* ::Input:: *)
(*(*ShellLabel[{0,0,1/2}]*)
 (*ShellLabel[{0,1/2}]*)*)


(* ::Subsection:: *)
(*Isotope label as NucleusBox -- DEPRECATED*)


Options[NucleusBox]={NuclearA->"",NuclearZ->"",NuclearN->""};
NucleusBox[Element_,Opts___?OptionQ]:=Module[
  {FullOpts=Flatten[{Opts,Options[NucleusBox]}]},
  Row[{
    SubsuperscriptBox["",NuclearZ/.FullOpts,NuclearA/.FullOpts],SubsuperscriptBox[Element,NuclearN/.FullOpts,""]  (* to match subsuperscript on left for alignment *)
    }]
                                      ];


(* ::Input:: *)
(*(*NucleusBox["Au",NuclearA\[Rule]"A",NuclearZ->"Z",NuclearN->"N"]//DisplayForm*)*)


(* ::Output:: *)
(*\!\( *)
(*TagBox[*)
(*TemplateBox[{SubsuperscriptBox["", "Z", "A"], SubsuperscriptBox["Au", "N", ""]},*)
(*"Row",*)
(*DisplayFunction->(RowBox[{#, " ", #2}]& ),*)
(*InterpretationFunction->(RowBox[{"Row", "[", RowBox[{"{", RowBox[{#, ",", #2}], "}"}], "]"}]& )],*)
(*DisplayForm]\)*)


(* ::Subsection:: *)
(*Isotope label*)


(* ::Text:: *)
(*Meant to roughly follow usage of L AT EX isotope package (Heiko Bauke)*)


(* LIMITATION: A and Z are left aligned *)


Isotope[A_:None,Z_:None,N_:None,Sup_:None,Element_String]:=Which[
  (Z===None)&&(N===None),
  (* use subsuperscript on both sides if use on either, to match alignment *)
  Row[{
    Superscript["",Replace[A,None->""]],Superscript[Element,Replace[Sup,None->""]]  
    }],
  True,
  Row[{
    Subsuperscript["",Replace[Z,None->""],Replace[A,None->""]],Subsuperscript[Element,Replace[N,None->""],Replace[Sup,None->""]]  
    }]
                                                           ];


Isotope[PriorArgs___,(Z_Integer)?NonNegative]:=Isotope[PriorArgs,ElementAbbreviation[Z]];


(* ::Input:: *)
(*(*Isotope["C"]*)
(*Isotope[12,"C"]*)
(*Isotope[12,6,"C"]*)
(*Isotope[12,6,6,"C"]*)
(*Isotope[12,None,6,"C"]*)
(*Isotope[12,None,None,"*","C"]*)\[AliasDelimiter]*)


(* ::Input:: *)
(*(*Isotope[12,6]*)*)


(* ::Input:: *)
(*(*Isotope["X","X","X","X","X"]*)
 (*Isotope["X",{1,1}]*)
 (*Isotope["+",{1,1}]*)
 (*Isotope["+",{1,1}]//DisplayForm*)*)


(* ::Subsection:: *)
(*Isotope label -- {Z,N} form*)


Isotope[Sup_:None,{Z_Integer,N_Integer}]:=Isotope[Z+N,None,None,Sup,ElementAbbreviation[Z]];


(* ::Input:: *)
(*(*Isotope[{3,3}]*)
 (*Isotope["*",{3,3}]*)*)


(* ::Subsection:: *)
(*Isotope data*)


ElementAbbreviations=
{"H","He","Li","Be","B","C","N","O","F","Ne","Na","Mg","Al","Si","P",
 "S","Cl","Ar","K","Ca","Sc","Ti","V","Cr","Mn","Fe","Co","Ni","Cu",
 "Zn","Ga","Ge","As","Se","Br","Kr","Rb","Sr","Y","Zr","Nb","Mo","Tc",
 "Ru","Rh","Pd","Ag","Cd","In","Sn","Sb","Te","I","Xe","Cs","Ba","La",
 "Ce","Pr","Nd","Pm","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb",
 "Lu","Hf","Ta","W","Re","Os","Ir","Pt","Au","Hg","Tl","Pb","Bi","Po",
 "At","Rn","Fr","Ra","Ac","Th","Pa","U","Np","Pu","Am","Cm","Bk","Cf",
 "Es","Fm","Md","No","Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt",
 "Uun","Uuu","Uub"}; (* data circa Mathematica 5.0 *)
ElementAbbreviationsLength=Length[ElementAbbreviations];


(* from ChemicalElements package 1.4 Elements list, converted to strings *)
ElementNames={"Hydrogen","Helium","Lithium","Beryllium","Boron","Carbon","Nitrogen","Oxygen","Fluorine","Neon","Sodium","Magnesium","Aluminium","Silicon","Phosphorus","Sulfur","Chlorine","Argon","Potassium","Calcium","Scandium","Titanium","Vanadium","Chromium","Manganese","Iron","Cobalt","Nickel","Copper","Zinc","Gallium","Germanium","Arsenic","Selenium","Bromine","Krypton","Rubidium","Strontium","Yttrium","Zirconium","Niobium","Molybdenum","Technetium","Ruthenium","Rhodium","Palladium","Silver","Cadmium","Indium","Tin","Antimony","Tellurium","Iodine","Xenon","Caesium","Barium","Lanthanum","Cerium","Praseodymium","Neodymium","Promethium","Samarium","Europium","Gadolinium","Terbium","Dysprosium","Holmium","Erbium","Thulium","Ytterbium","Lutetium","Hafnium","Tantalum","Tungsten","Rhenium","Osmium","Iridium","Platinum","Gold","Mercury","Thallium","Lead","Bismuth","Polonium","Astatine","Radon","Francium","Radium","Actinium","Thorium","Protactinium","Uranium","Neptunium","Plutonium","Americium","Curium","Berkelium","Californium","Einsteinium","Fermium","Mendelevium","Nobelium","Lawrencium","Rutherfordium","Dubnium","Seaborgium","Bohrium","Hassium","Meitnerium","Ununnilium","Unununium","Ununbium"};
ElementNamesLength=Length[ElementNames];


StableIsotopes["Hydrogen"]={1,2};
StableIsotopes["Helium"]={3,4};
StableIsotopes["Lithium"]={6,7};
StableIsotopes["Beryllium"]={9};
StableIsotopes["Boron"]={10,11};
StableIsotopes["Carbon"]={12,13};
StableIsotopes["Nitrogen"]={14,15};
StableIsotopes["Oxygen"]={16,17,18};
StableIsotopes["Fluorine"]={19};
StableIsotopes["Neon"]={20,21,22};
StableIsotopes["Sodium"]={23};
StableIsotopes["Magnesium"]={24,25,26};
StableIsotopes["Aluminium"]={27};
StableIsotopes["Silicon"]={28,29,30};
StableIsotopes["Phosphorus"]={31};
StableIsotopes["Sulfur"]={32,33,34,36};
StableIsotopes["Chlorine"]={35,37};
StableIsotopes["Argon"]={36,38,40};
StableIsotopes["Potassium"]={39,41};
StableIsotopes["Calcium"]={40,42,43,44,46,48};
StableIsotopes["Scandium"]={45};
StableIsotopes["Titanium"]={46,47,48,49,50};
StableIsotopes["Vanadium"]={51};
StableIsotopes["Chromium"]={50,52,53,54};
StableIsotopes["Manganese"]={55};
StableIsotopes["Iron"]={54,56,57,58};
StableIsotopes["Cobalt"]={59};
StableIsotopes["Nickel"]={58,60,61,62,64};
StableIsotopes["Copper"]={63,65};
StableIsotopes["Zinc"]={66,67,68,70};
StableIsotopes["Gallium"]={69,71};
StableIsotopes["Germanium"]={70,72,73,74,76};
StableIsotopes["Arsenic"]={75};
StableIsotopes["Selenium"]={74,76,77,78,80,82};
StableIsotopes["Bromine"]={79,81};
StableIsotopes["Krypton"]={78,80,82,83,84,86};
StableIsotopes["Rubidium"]={85};
StableIsotopes["Strontium"]={84,86,87,88};
StableIsotopes["Yttrium"]={89};
StableIsotopes["Zirconium"]={90,91,92,94};
StableIsotopes["Niobium"]={93};
StableIsotopes["Molybdenum"]={92,94,95,96,97,98,100};
StableIsotopes["Technetium"]={};
StableIsotopes["Ruthenium"]={96,98,99,100,101,102,104};
StableIsotopes["Rhodium"]={103};
StableIsotopes["Palladium"]={102,104,105,106,108,110};
StableIsotopes["Silver"]={107,109};
StableIsotopes["Cadmium"]={106,108,110,111,112,113,114,116};
StableIsotopes["Indium"]={113};
StableIsotopes["Tin"]={112,114,115,116,117,118,119,120,122,124};
StableIsotopes["Antimony"]={121,123};
StableIsotopes["Tellurium"]={120,122,124,125,126,128,130};
StableIsotopes["Iodine"]={127};
StableIsotopes["Xenon"]={129,130,131,132,134,136};
StableIsotopes["Caesium"]={133};
StableIsotopes["Barium"]={130,132,134,135,136,137,138};
StableIsotopes["Lanthanum"]={138};
StableIsotopes["Cerium"]={136,138,140,142};
StableIsotopes["Praseodymium"]={141};
StableIsotopes["Neodymium"]={142,143,145,146,148,150};
StableIsotopes["Promethium"]={};
StableIsotopes["Samarium"]={144,150,152,154};
StableIsotopes["Europium"]={151,153};
StableIsotopes["Gadolinium"]={154,155,156,157,158,160};
StableIsotopes["Terbium"]={159};
StableIsotopes["Dysprosium"]={156,158,160,161,162,163,164};
StableIsotopes["Holmium"]={165};
StableIsotopes["Erbium"]={162,164,166,167,168,170};
StableIsotopes["Thulium"]={169};
StableIsotopes["Ytterbium"]={168,170,171,172,173,174,176};
StableIsotopes["Lutetium"]={175};
StableIsotopes["Hafnium"]={176,177,178,179,180};
StableIsotopes["Tantalum"]={181};
StableIsotopes["Tungsten"]={180,182,183,184,186};
StableIsotopes["Rhenium"]={185};
StableIsotopes["Osmium"]={184,186,187,188,189,190,192};
StableIsotopes["Iridium"]={191,193};
StableIsotopes["Platinum"]={194,195,196,198};
StableIsotopes["Gold"]={197};
StableIsotopes["Mercury"]={196,198,199,200,201,202,204};
StableIsotopes["Thallium"]={203,205};
StableIsotopes["Lead"]={204,206,207,208};
StableIsotopes["Bismuth"]={209};
StableIsotopes["Polonium"]={};
StableIsotopes["Astatine"]={};
StableIsotopes["Radon"]={};
StableIsotopes["Francium"]={};
StableIsotopes["Radium"]={};
StableIsotopes["Actinium"]={};
StableIsotopes["Thorium"]={};
StableIsotopes["Protactinium"]={};
StableIsotopes["Uranium"]={};
StableIsotopes["Neptunium"]={};
StableIsotopes["Plutonium"]={};
StableIsotopes["Americium"]={};
StableIsotopes["Curium"]={};
StableIsotopes["Berkelium"]={};
StableIsotopes["Californium"]={};
StableIsotopes["Einsteinium"]={};
StableIsotopes["Fermium"]={};
StableIsotopes["Mendelevium"]={};
StableIsotopes["Nobelium"]={};
StableIsotopes["Lawrencium"]={};
StableIsotopes["Rutherfordium"]={};
StableIsotopes["Dubnium"]={};
StableIsotopes["Seaborgium"]={};
StableIsotopes["Bohrium"]={};
StableIsotopes["Hassium"]={};
StableIsotopes["Meitnerium"]={};
StableIsotopes["Ununnilium"]={};
StableIsotopes["Unununium"]={};
StableIsotopes["Ununbium"]={};



(* ::Subsection:: *)
(*Isotope data retrieval*)


(* ::Text:: *)
(*see also http://library.wolfram.com/infocenter/MathSource/6794/ *)


(* ::Input:: *)
(*(*ElementData["Properties"]*)*)


(* ::Input:: *)
(*(* Note: ElementData[Element,"Abbreviation"] works only online. Therefore using static offline data. *)*)


ElementAbbreviation[Z:0]="n";
ElementAbbreviation[(Z_Integer)?Positive]/;(Z<=ElementAbbreviationsLength):=ElementAbbreviations[[Z]];
ElementAbbreviation[(Z_Integer)?Positive]/;(Z>ElementAbbreviationsLength):=ToString[Z];


ElementName[Z:0]="Neutron";
ElementName[(Z_Integer)?Positive]/;(Z<=ElementNamesLength):=ElementNames[[Z]];
ElementName[(Z_Integer)?Positive]/;(Z>ElementNamesLength):=ToString[Z];


(*IsotopeIsStable[{Z:0,(A_Integer)?Positive}]:=False;
IsotopeIsStable[{(Z_Integer)?Positive,(A_Integer)?Positive}]/;(Z\[LessEqual]ElementNamesLength):=MemberQ[StableIsotopes[ElementName[Z]],A];
IsotopeIsStable[{(Z_Integer)?Positive,(A_Integer)?Positive}]/;(Z>ElementNamesLength):=False;*)


StableIsotopes[Z:0]:={};
StableIsotopes[(Z_Integer)?Positive]/;(Z<=ElementNamesLength):=StableIsotopes[ElementName[Z]];
StableIsotopes[(Z_Integer)?Positive]/;(Z>ElementNamesLength):={};


IsotopeIsStable[{Z:0,(N_Integer)?Positive}]:=False;
IsotopeIsStable[{(Z_Integer)?Positive,(N_Integer)?NonNegative}]/;(Z<=ElementNamesLength):=MemberQ[StableIsotopes[ElementName[Z]],Z+N];
IsotopeIsStable[{(Z_Integer)?Positive,(N_Integer)?NonNegative}]/;(Z>ElementNamesLength):=False;


Isotope[Args___,(Z_Integer)?NonNegative]:=Isotope[Args,ElementAbbreviation[Z]];


(* ::Input:: *)
(*(*Isotope[12,6]*)*)


(* ::Input:: *)
(*(*ElementName[12]*)*)


(* ::Input:: *)
(*(*StableIsotopes[6]*)*)


(* ::Input:: *)
(*(*IsotopeIsStable[{6,6}]*)*)


(* ::Subsection:: *)
(*Level spin-parity label as LabelJ[i]P -- DEPRECATED*)


Options[LabelJiP]={Rational->SolidusFractionize};
LabelJiP[J_,i_,P:(+1|-1|None):+1,OptionsPattern[]]:=SubsuperscriptBox[
  J/.{x_Rational:>OptionValue[Rational][x]},
  i,
  Switch[P,+1,"+",-1,"-",None,""]
                                                    ];
Options[LabelJP]={Rational->SolidusFractionize};
LabelJP[J_,P:(+1|-1|None):+1,OptionsPattern[]]:=SuperscriptBox[
  J/.{x_Rational:>OptionValue[Rational][x]},
  Switch[P,+1,"+",-1,"-",None,""]
                                                ];


(* ::Input:: *)
(*(*{LabelJiP[2,1,-1],LabelJiP[2,1,None],LabelJiP[1/2,1],LabelJP[1/2]}//DisplayForm*)*)


(* ::Input:: *)
(*(*{LabelJiP[1/2,1,Rational\[Rule]DiagonalFractionize],LabelJiP[1/2,1,Rational\[Rule]Fractionize]}//DisplayForm*)*)


(* ::Subsection:: *)
(*Level spin-parity label*)


(* ::Text:: *)
(*Caveat: Including a null string subscript still allows space for the subscript, which throws off the baseline of the label.  Therefore only use a subscript if necessary.*)


(* ::Text:: *)
(*DEFICIENCY: "Jp" (i.e., without index) format is cumbersome to specify automatically through arguments, without options*)


Options[LevelLabel]={Rational->SolidusFractionize,Parity->None};
LevelLabel[{J_,i_,P_},Opts:OptionsPattern[]]:=Module[
  {JText,iValue,PValue,PText},

  (* construct entry text *)
  JText=J/.{x_Rational:>OptionValue[Rational][x]};
  iValue=ReplaceSequential[i,{Automatic->None}];
  PValue=ReplaceSequential[P,{Automatic->OptionValue[Parity]}];
  PText=Switch[PValue,+1,"+",-1,"-",_,PValue];

  (* generate scriptbox as appropriate *)
  Which[
    (iValue===None)&&(PValue===None),
    JText,
    (iValue===None),
    Superscript[JText,PText],
    (PValue===None),
    Subscript[JText,iValue],
    True,
    Subsuperscript[JText,iValue,PText]
  ]
                                              ];
LevelLabel[{J_,i_},Opts:OptionsPattern[]]:=LevelLabel[{J,i,Automatic},Opts];
LevelLabel[{J_},Opts:OptionsPattern[]]:=LevelLabel[{J,Automatic,Automatic},Opts];


(* ::Text:: *)
(*Version with unwrapped arguments*)


LevelLabel[J_,i:Except[_?OptionQ],P:Except[_?OptionQ],Opts:OptionsPattern[]]:=LevelLabel[{J,i,P},Opts];
LevelLabel[J_,i:Except[_?OptionQ],Opts:OptionsPattern[]]:=LevelLabel[{J,i},Opts];
LevelLabel[J_,Opts:OptionsPattern[]]:=LevelLabel[{J},Opts];


(* ::Input:: *)
(*(*{LevelLabel[2,1,-1],LevelLabel[2,1,None],LevelLabel[1/2,1],LevelLabel[1/2]}//DisplayForm*)*)


(* ::Input:: *)
(*(*{LevelLabel[1/2,1,+1,Rational\[Rule]DiagonalFractionize],LevelLabel[1/2,1,+1,Rational\[Rule]Fractionize]}//DisplayForm*)*)


(* ::Input:: *)
(*(*{LevelLabel[2],LevelLabel[2,Parity\[Rule]+1]}*)*)


(* ::Input:: *)
(*(*LevelLabel["J","i","\[Pi]"]//DisplayForm*)*)


(* ::Subsection:: *)
(*Energy label*)


(* ::Text:: *)
(*E(level)*)


Options[EnergyLabel]={Symbol->textit["E"]};


EnergyLabel[Level1_,Opts:OptionsPattern[]]:=Module[
  {},
  Row[{OptionValue[Symbol],"(",Switch[Level1,_List,LevelLabel[Level1],_,Level1],")"}]
                                            ];


(* ::Subsection:: *)
(*Electromagnetic transition labels*)


(* ::Text:: *)
(*Reduced transition probability -- B(\[Sigma]\[Lambda];i->f)*)


(* ::Text:: *)
(*Provision for ImageMargins is to prevent clipping of text in EPS output (ImageMargins->1), but this can also add extensive padding and throw off spacing (even with, say, ImageMargins->0.0001).*)
(**)
(*Reverse:*)
(*	False -- Use "experimentalist's order" for arguments (initial,final).*)
(*	True -- Use "theorist's order" for arguments, i.e., same order as in RME, (final,initial).*)
(*	*)


Options[RTPLabel]={MultipolaritySymbols->{"E","M"},MultipolarityStyle->Italic,Reverse->False,ImageMargins->0};


(* ::Text:: *)
(*	handle default cases, where function provides "E" or "M" symbol*)


RTPLabel[Multipolarity:{sigma:(+1|-1),lambda_Integer},Level1_,Level2_,Opts:OptionsPattern[]]:=Module[
  {Mode,StyleDirective},
  Mode=Switch[
    sigma*(-1)^lambda,
    +1,OptionValue[MultipolaritySymbols][[1]],  (* "E" *)
    -1,OptionValue[MultipolaritySymbols][[2]]  (* "M" *)
       ];
  StyleDirective=OptionValue[MultipolarityStyle];
  RTPLabel[Row[{Style[Mode,StyleDirective],lambda}],Level1,Level2,Opts]
                                                                                              ];
RTPLabel[lambda_Integer,Level1_,Level2_,Opts:OptionsPattern[]]:=RTPLabel[{+1,lambda},Level1,Level2,Opts];


RTPLabel[Multipolarity:{sigma:(+1|-1),lambda_Integer},Opts:OptionsPattern[]]:=Module[
  {Mode,StyleDirective},
  Mode=Switch[
    sigma*(-1)^lambda,
    +1,OptionValue[MultipolaritySymbols][[1]],  (* "E" *)
    -1,OptionValue[MultipolaritySymbols][[2]]  (* "M" *)
       ];
  StyleDirective=OptionValue[MultipolarityStyle];
  RTPLabel[Row[{Style[Mode,StyleDirective],lambda}],Opts]
                                                                              ];
RTPLabel[lambda_Integer,Opts:OptionsPattern[]]:=RTPLabel[{+1,lambda},Opts];


(* ::Text:: *)
(*	generic case -- multipolarity symbol given explicitly*)


RTPLabel[Multipolarity:Except[(_Integer)|(_List)],Level1_,Level2_,OptionsPattern[]]:=Module[
  {Label1,Label2},
  {Label1,Label2}={Switch[Level1,_List,LevelLabel[Level1],_,Level1],Switch[Level2,_List,LevelLabel[Level2],_,Level2]};
  If[
    OptionValue[Reverse],
    {Label1,Label2}=Reverse[{Label1,Label2}]
  ];
  Row[{textit["B"],"(",Multipolarity,";",Label1,"\[Rule]",Label2,")"},ImageMargins->OptionValue[ImageMargins]]
                                                                                     ];
RTPLabel[Multipolarity:Except[(_Integer)|(_List)],OptionsPattern[]]:=Row[{
  textit["B"],"(",Multipolarity,
  ")"
  },ImageMargins->OptionValue[ImageMargins]];


(* ::Input:: *)
(*(*RTPLabel[2]//DisplayForm*)
 (*RTPLabel[2,{2,1,+1},{0,1,+1}]//DisplayForm*)
 (*RTPLabel[{-1,1},{1,1,-1},{0,1,+1}]//DisplayForm*)
 (*RTPLabel["\[Sigma]\[Lambda]",{"J","i"},{"J","f"}]//DisplayForm*)
 (*RTPLabel["\[Sigma]\[Lambda]",{"J","f"},{"J","i"},Reverse\[Rule]True]//DisplayForm*)*)


(* ::Text:: *)
(*Reduced matrix element -- <f||\[Sigma]\[Lambda]||>*)


(* ::Text:: *)
(*Note: The \Vert symbol [DoubleVerticalBar] is too small/low-slung in Mathematica.  Therefore, use as default two bars (with no space between them).*)


Options[RMELabel]={MultipolaritySymbols->{"Q","M"},MultipolarityStyle->Bold,Dividers->"||",ImageMargins->1};


(* ::Text:: *)
(*	handle default cases, where function provides "Q" or "M" symbol*)
(*	alternative MultipolaritySymbols->{textbf["Q"],textbf["M"]}*)


RMELabel[Multipolarity:{sigma:(+1|-1),lambda_Integer},Level2_,Level1_,Opts:OptionsPattern[]]:=Module[
  {Mode,StyleDirective},
  Mode=Switch[
    sigma*(-1)^lambda,
    +1,OptionValue[MultipolaritySymbols][[1]],  (* "Q" *)
    -1,OptionValue[MultipolaritySymbols][[2]]  (* "M" *)
       ];
  StyleDirective=OptionValue[MultipolarityStyle];
  RMELabel[Subscript[Style[Mode,StyleDirective],lambda],Level2,Level1,Opts]
                                                                                              ];
RMELabel[lambda_Integer,Level2_,Level1_,Opts:OptionsPattern[]]:=RMELabel[{+1,lambda},Level2,Level1,Opts];


RMELabel[Multipolarity:{sigma:(+1|-1),lambda_Integer},Opts:OptionsPattern[]]:=Module[
  {Mode},
  Mode=Switch[
    sigma*(-1)^lambda,
    +1,OptionValue[MultipolaritySymbols][[1]],  (* "E" *)
    -1,OptionValue[MultipolaritySymbols][[2]]  (* "M" *)
       ];
  RMELabel[Subscript[Mode,lambda],Opts]
                                                                              ];
RMELabel[lambda_Integer,Opts:OptionsPattern[]]:=RMELabel[{+1,lambda},Opts];


(* ::Text:: *)
(*	generic case -- multipolarity symbol given explicitly*)


RMELabel[Multipolarity:Except[(_Integer)|(_List)],Level2_,Level1_,OptionsPattern[]]:=Row[{
  "\[LeftAngleBracket]",Switch[Level2,_List,LevelLabel[Level2],_,Level2],OptionValue[Dividers],Multipolarity,OptionValue[Dividers],Switch[Level1,_List,LevelLabel[Level1],_,Level1],"\[RightAngleBracket]"
  }]
RMELabel[Multipolarity:Except[(_Integer)|(_List)],OptionsPattern[]]:=Row[{
  "\[LeftAngleBracket]",OptionValue[Dividers],Multipolarity,OptionValue[Dividers],"\[RightAngleBracket]"
  },ImageMargins->OptionValue[ImageMargins]]


(* ::Input:: *)
(*(*RMELabel[2]//DisplayForm*)
 (*RMELabel[2,{0,1,+1},{2,1,+1}]//DisplayForm*)
 (*RMELabel[{-1,1},{0,1,+1},{1,1,-1}]//DisplayForm*)
 (*RMELabel[textit["T"],{"J","f"},{"J","i"}]//DisplayForm*)
 (*textfamily["Times",RMELabel[2]]//DisplayForm*)*)


Options[MomentLabel]={ImageMargins->1};


MomentLabel[lambda:1,Level1_,Opts:OptionsPattern[]]:=Row[{"\[Mu]","(",Switch[Level1,_List,LevelLabel[Level1],_,Level1],")"},ImageMargins->OptionValue[ImageMargins]];
MomentLabel[lambda:2,Level1_,Opts:OptionsPattern[]]:=Row[{textit["Q"],"(",Switch[Level1,_List,LevelLabel[Level1],_,Level1],")"},ImageMargins->OptionValue[ImageMargins]];


(* ::Input:: *)
(*(*MomentLabel[1,{2,1,+1}]//DisplayForm*)
 (*MomentLabel[2,{2,1,+1}]//DisplayForm*)*)


(* ::Input:: *)
(*(*MomentLabel[1,textit["J"]]*)
 (*MomentLabel[2,textit["J"]]*)*)


(* ::Section:: *)
(*Fraction formatting*)


(* ::Text:: *)
(*Notes on formatting in fraction constructs prior to Mathematica 6.0:*)
(*	Applying DisplayForm to inidividual fraction elements is required for quantities deep within (like Pi) to be displayed correctly even if full fraction is displayed in DisplayForm.*)
(*	RowBox seems to give most generous default spacing, which cannot be adjusted.  Applying DisplayForm to individual list elements makes this worse, but apparently only for RowBox.   Therefore, GridBox is preferred.*)


(* ::Subsection:: *)
(*Native fraction*)


Fractionize[x_?NumericQ,Opts___?OptionQ]/;(Denominator[x]==1):=DisplayForm[x];
Fractionize[x_?NumericQ,Opts___?OptionQ]/;(Denominator[x]!=1):=FractionBox[Numerator[x],Denominator[x],Opts];
f:Fractionize[x_List,Opts___?OptionQ]:=Thread[Unevaluated[f],List,1];


(* ::Subsection:: *)
(*Text fraction*)


(* ::Text:: *)
(*In Mathematica 5-ish, apparently RowSpacings->0,RowMinHeight->{1,1.1} worked well.*)
(*In Mathematica 9, a little extra row spacing seems advisable -- RowSpacings->0,RowMinHeight->{1,1.2}.*)
(*Note Smaller might not genuinely give script size.*)


TextFractionBox[a_,b_,Opts:OptionsPattern[]]:=Style[
  GridBox[
    {{a},{b}},
    Opts,
    RowLines->True,RowSpacings->0,RowMinHeight->{1,1.2},RowAlignments->{Bottom,Baseline},ColumnSpacings->0
  ],
  Dashing[{}],AbsoluteThickness[0.5],Smaller
                                              ];


(*Style[TextFractionBox[Row[{"3","n"}],"2"],FontFamily->Times]//DisplayForm*)


TextFractionize[x_?NumericQ,Opts___?OptionQ]/;(Denominator[x]==1):=DisplayForm[x];
TextFractionize[x_?NumericQ,Opts___?OptionQ]/;(Denominator[x]!=1):=
  TextFractionBox[Numerator[x],Denominator[x],Opts];
f:TextFractionize[x_List,Opts___?OptionQ]:=Thread[Unevaluated[f],List,1];


(* ::Subsection:: *)
(*Solidus fraction*)


(* ::Input:: *)
(*(*SolidusFractionBox[x_,y_,Opts___?OptionQ]:=GridBox[{{DisplayForm[x],"/",DisplayForm[y]}},ColumnSpacings\[Rule]-0.1,BaselinePosition\[Rule]{{1,3},Baseline}];*)*)


Options[SolidusFractionBox]={Spacings->{0,0}};
SolidusFractionBox[x_,y_,Opts___?OptionQ]:=Module[
  {FullOpts=Flatten[{Opts,Options[SolidusFractionBox]}]},
  Grid[
    {{x,"/",y}},
    Spacings->{Flatten[{0,(Spacings/.FullOpts),0}]},
    BaselinePosition->{{1,3},Baseline} (* use baseline of denominator *)
  ]
                                           ];


(* ::Text:: *)
(*Random old scratchwork...*)


(* ::Input:: *)
(*(*GridBox[{{"1","/","2"}}]//DisplayForm*)
 (*Grid[{{"1","/","2"}}]//DisplayForm*)
 (*Grid[{{"1","/","2"}},Spacings\[Rule]0]//DisplayForm*)
 (*Grid[{{"1","/",Pi}},Spacings\[Rule]0]//DisplayForm*)*)


(* ::Input:: *)
(*(*Grid[{{"1","/","2"}},Spacings\[Rule]{1,999}]//DisplayForm*)
 (*Grid[{{"1","/","2"}},Spacings\[Rule]{{0,1,3,0},999}]//DisplayForm*)
 (*Grid[{{"1","/","2"}},Spacings\[Rule]{{0,1,3,0}}]//DisplayForm*)
 (*Grid[{{"1","/","2"}},Spacings\[Rule]{{1},{9}}]//DisplayForm*)*)
(**)


SolidusFractionize[x_?NumericQ,Opts___?OptionQ]/;(Denominator[x]==1):=DisplayForm[x];
SolidusFractionize[x_?NumericQ,Opts___?OptionQ]/;(Denominator[x]!=1):=SolidusFractionBox[Numerator[x],Denominator[x],Opts];
f:SolidusFractionize[x_List,Opts___?OptionQ]:=Thread[Unevaluated[f],List,1];


(* ::Input:: *)
(*(*SolidusFractionize[1/2]*)
 (*Style[%,FontFamily->"Times",Large]*)
 (*SolidusFractionize[1/2,ColumnSpacings\[Rule]{-0.1,-0.1}]*)
 (*Style[%,FontFamily->"Times",Large]*)*)


(* ::Subsection:: *)
(*Diagonal fraction*)


(* ::Text:: *)
(*CAVEAT: Spacing in EPS/PDF export and/or printing will differ from spacing on-screen.*)


(* ::Text:: *)
(*Imposing slant on slash seems to have no effect.*)


(* ::Text:: *)
(*ColumnSpacings->-0.1 worked well for Mathematica 5 (with Times), but ColumnSpacings->-0.3 (with Times) needed for Mathematica 6 on screen, and tighter for export.*)
(*Mathematica 10 seems to work well with ColumnSpacings->{-0.1,-0.3} for special case of EPS export of "1/2" in Times.*)
(*Slash is too small (same height as digits) in Times EPS/PDF export if do as superscript, so do full height.*)
(*Alignment of baseline with bottom of denominator -- pre-6 was GridBaseline->{Bottom,{1,3}}, post-6 is BaselinePosition->{{1,3},Bottom}*)
(**)


Options[DiagonalFractionBox]={Spacings->{-0.1,-0.3},Baseline->{0.5,0.0,0.0},KernForSuperscript->-0.15};
DiagonalFractionBox[x_,y_,Opts___?OptionQ]:=Module[
  {FullOpts=Flatten[{Opts,Options[DiagonalFractionBox]}]},
  TagBox[
    StyleBox[
      Grid[
        {{
          AdjustmentBox[SubscriptBox["",DisplayForm[x]],BoxBaselineShift->-((Baseline/.FullOpts)[[1]])],
          (*AdjustmentBox[SubscriptBox["",StyleForm["/",FontSlant->"Oblique",Larger]],BoxBaselineShift\[Rule]-((Baseline/.FullOpts)[[2]])],*)
          AdjustmentBox[StyleForm["/",FontSlant->"Oblique"],BoxBaselineShift->-((Baseline/.FullOpts)[[2]])],
          AdjustmentBox[SubscriptBox["",DisplayForm[y]],BoxBaselineShift->-((Baseline/.FullOpts)[[3]])]
          }},
        Spacings->{Flatten[{0,(Spacings/.FullOpts),0}]},
        BaselinePosition->{{1,3},Bottom}
      ],
      ScriptBaselineShifts->{0,0}
    ],
    DiagonalFractionBox[(KernForSuperscript/.FullOpts)]
  ]
                                            ];


(* ::Text:: *)
(*DiagonalFractionBox kerning setup*)


Unprotect[TagBox];
TagBox/:SuperscriptBox[x:TagBox[_,DiagonalFractionBox[Adjustment_]],n_]:=SuperscriptBox[AdjustmentBox[x,BoxMargins->{{0,Adjustment},{0,0}}],n];
        TagBox/:Superscript[x:TagBox[_,DiagonalFractionBox[Adjustment_]],n_]:=Superscript[AdjustmentBox[x,BoxMargins->{{0,Adjustment},{0,0}}],n];
                Protect[TagBox];


                DiagonalFractionize[x_?NumericQ,Opts___?OptionQ]/;(Denominator[x]==1):=DisplayForm[x];
                DiagonalFractionize[x_?NumericQ,Opts___?OptionQ]/;(Denominator[x]!=1):=DiagonalFractionBox[Numerator[x],Denominator[x],Opts];
                f:DiagonalFractionize[x_List,Opts___?OptionQ]:=Thread[Unevaluated[f],List,1];


                (* ::Subsection:: *)
                (*Solidus fraction as string*)


                FractionString[x_?NumericQ]:=Module[
                  {f,NumeratorString,DenominatorString},
                  f=Rationalize[x];
                  NumeratorString=ToString[Numerator[f]];
                  DenominatorString=ToString[Denominator[f]];
                  Which[
                    f==0,"0",
                    Denominator[f]==1,StringJoin[NumeratorString],
                    Denominator[f]!=1,StringJoin[NumeratorString,"/",DenominatorString]
                  ]
                                             ];


                (* ::Subsection:: *)
                (*Pi fraction as string*)


                (* ::Text:: *)
                (*PiFractionString*)


                PiFractionString[x_?NumericQ]:=Module[
                  {f,NumeratorString,DenominatorString},
                  f=Rationalize[x/Pi];
                  NumeratorString=If[
                    Numerator[f]==1,
                    "",
                    ToString[Numerator[f]]
                                  ];
                  DenominatorString=ToString[Denominator[f]];
                  Which[
                    f==0,"0",
                    Denominator[f]==1,StringJoin[NumeratorString,"\[Pi]"],
                    Denominator[f]!=1,StringJoin[NumeratorString,"\[Pi]","/",DenominatorString]
                  ]
                                               ];


                (* ::Input:: *)
                (*(*PiFractionString[0*Pi]*)
                 (*PiFractionString[1*Pi/2]*)
                 (*PiFractionString[2*Pi]*)
                 (*PiFractionString[2*Pi/3]*)
                 (*PiFractionString[2*E*Pi/3]*)
                 (*PiFractionString[3.9]*)*)


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
