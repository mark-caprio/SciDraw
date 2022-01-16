(* ::Package:: *)

(*Header comments*)


(* :Title: FigGeometry *)
(* :Context: SciDraw` *)
(* :Summary: Geometry utilities needed by figure objects *)
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





(*Scalar/range/region arithmetic*)


(*Single parameter upgrades*)


ScalarParameterPattern=None|(_?NumericQ);
NonNegativeScalarParameterPattern=(None|NonNegativePattern);


UpgradeScalar[None]:=0;
UpgradeScalar[x_?NumericQ]:=x;


(*Interval rescaling*)


(*lambda wrapper for Rescale[u,{u1,u2},{v1,v2}]*)
(*   returns v1+(u-u1)/(u2-u1)*(v2-v1)*)


RescaleInterval[{u1_?NumericQ,u2_?NumericQ},{v1_?NumericQ,v2_?NumericQ}][u_?NumericQ]:=Rescale[u,{u1,u2},{v1,v2}];


(*Range testing*)


InRange[{x1_,x2_},x_]:=(x1<=x)&&(x<=x2);


InRange[{{x1_,x2_},{y1_,y2_}},{x_,y_}]:=InRange[{x1,x2},x]&&InRange[{y1,y2},y];


(*Interval extension by coordinate amount*)


(*1-dimensional*)
(*Note: "Absolute" is SciDraw`Private legacy name.  Abs is accepted as global alternative.*)


ExtendInterval[PRange:{_?NumericQ,_?NumericQ},PDiff:{_?NumericQ,_?NumericQ},Mode:(Abs|Absolute)]:=PRange+PDiff*{-1,+1};
ExtendInterval[PRange:{_?NumericQ,_?NumericQ},PFrac:{_?NumericQ,_?NumericQ},Mode:Scaled]:=PRange+PFrac*{-1,+1}*-Subtract@@PRange;


(*Vector geometry*)


(*Trigonometry for 2D vectors*)


VectorLength[{x_?NumericQ,y_?NumericQ}]:=Sqrt[x^2+y^2];VectorArcTan[{x_?NumericQ,y_?NumericQ}]:=If[{x,y}=={0,0},0.,ArcTan[x,y]];


SegmentLength[{p1:{_?NumericQ,_?NumericQ},p2:{_?NumericQ,_?NumericQ}}]:=VectorLength[p2-p1];
SegmentTangent[{p1:{_?NumericQ,_?NumericQ},p2:{_?NumericQ,_?NumericQ}}]:=(p2-p1)/VectorLength[p2-p1];SegmentArcTan[{p1:{_?NumericQ,_?NumericQ},p2:{_?NumericQ,_?NumericQ}}]:=VectorArcTan[p2-p1];


FromPolar[{rho_,phi_}]:=rho*{Cos[phi],Sin[phi]};


(*Segment interpolation*)


(*Null segment*)


InterpolateSegment[
  s:{p1:{_?NumericQ,_?NumericQ},p2:{_?NumericQ,_?NumericQ}},
  Reference:(Tail|Center|Head),Mode:(Absolute|Scaled):Scaled,
  x_?NumericQ
]/;(Chop[SegmentLength[s]]==0):=p1;


(*Generic segment*)
(*	Curve parameter is always in the *forward* direction (tail to head).*)
(*	Mappings for scaled coordinates:*)
(*		Tail [0,+1] -> [p1,p2]*)
(*		Center [-1,+1] -> [p1,p2] -- so units are scaled by factor of two relative to other cases, consistent with "offset" relative position*)
(*		Head [-1,0] -> [p1,p2]*)


InterpolateSegment[
  s:{p1:{_?NumericQ,_?NumericQ},p2:{_?NumericQ,_?NumericQ}},
  Reference:Tail,
  Mode:(Absolute|Scaled):Scaled,
  x_?NumericQ
]/;(Chop[SegmentLength[s]]!=0):=p1+x*(p2-p1)/Switch[Mode,Absolute,SegmentLength[s],Scaled,1];
InterpolateSegment[
  s:{p1:{_?NumericQ,_?NumericQ},p2:{_?NumericQ,_?NumericQ}},
  Reference:Center,
  Mode:(Absolute|Scaled):Scaled,
  x_?NumericQ
]/;(Chop[SegmentLength[s]]!=0):=(p2+p1)/2+x*(p2-p1)/Switch[Mode,Absolute,SegmentLength[s],Scaled,2];
InterpolateSegment[
  s:{p1:{_?NumericQ,_?NumericQ},p2:{_?NumericQ,_?NumericQ}},
  Reference:Head,
  Mode:(Absolute|Scaled):Scaled,
  x_?NumericQ
]/;(Chop[SegmentLength[s]]!=0):=p2+x*(p2-p1)/Switch[Mode,Absolute,SegmentLength[s],Scaled,1];


(*XY pair geometry*)


(*Numerical pair pattern*)


(*Explicit numerical {x,y} pair*)
(*Note: Text should use FigTextOffsetPattern instead, which allows the value Automatic as well.*)


NumericalPairPattern={_?NumericQ,_?NumericQ};


(*Pair parameter upgrades*)


(*Generic upgrade to equal values -- appropriate for radii and most types of nonnumeric parameters*)


UpgradePair[a:NonListPattern]:={a,a};
UpgradePair[{x:NonListPattern,y:NonListPattern}]:={x,y};


(*Upgrade to default horizontal or default vertical numerical pair -- as in classic Nudge behavior*)


IntervalParametersPattern=(None|(_?NumericQ)|{(_?NumericQ),(_?NumericQ)});
NonNegativeIntervalParametersPattern=(None|NonNegativePattern|{NonNegativePattern,NonNegativePattern});


UpgradePairEqual[None]:={0,0};
UpgradePairEqual[x_?NumericQ]:={x,x};
UpgradePairEqual[{x_?NumericQ,y_?NumericQ}]:={x,y};


UpgradePairHorizontal[None]:={0,0};
UpgradePairHorizontal[x_?NumericQ]:={x,0};
UpgradePairHorizontal[{x_?NumericQ,y_?NumericQ}]:={x,y};


UpgradePairVertical[None]:={0,0};
UpgradePairVertical[y_?NumericQ]:={0,y};
UpgradePairVertical[{x_?NumericQ,y_?NumericQ}]:={x,y};


(*Point/anchor geometry*)


(*Point pattern*)


(*Point specification (including anchor name or object anchor generation).*)


FigCoordinatePointPattern={
  ((_?NumericQ)|(Scaled|Canvas)[(_?NumericQ)]),
  ((_?NumericQ)|(Scaled|Canvas)[(_?NumericQ)])
}|(Scaled|Canvas)[{_?NumericQ,_?NumericQ}];


FigPointPattern=(FigCoordinatePointPattern|ObjectPattern[FigAnchor]|ObjectNamePattern[FigAnchor]);


(*Point resolution*)


(*Resolves a point/anchor specification into canvas coordinates.*)
(**)
(*Note: This function is technically not necessary, since any valid point specification can be converted to an anchor with FigAnchor[p], and then the point part of the anchor can be extracted.  However, this function is more direct for ordinary numerical points, in that it avoids unnecessary creation of anchors for these points.*)


FigResolvePoint[p:{_?NumericQ,_?NumericQ}]:=(CurrentWindow[]@TFunction[])@p;
FigResolvePoint[Canvas[p:{_?NumericQ,_?NumericQ}]]:=p;
FigResolvePoint[Scaled[p:{_?NumericQ,_?NumericQ}]]:=(CurrentWindow[]@ScaledTFunction[])@p;
FigResolvePoint[a:ObjectPattern[FigAnchor]]:=a@GetPoint[];
FigResolvePoint[n:ObjectNamePattern[FigAnchor]]:=Object[n]@GetPoint[];


(*Ad hoc treatment of hybrid coordinates*)


FigResolvePoint[
  p:Except[
    {_?NumericQ,_?NumericQ},
    {
      x:((_?NumericQ)|(Scaled|Canvas)[(_?NumericQ)]),
      y:((_?NumericQ)|(Scaled|Canvas)[(_?NumericQ)])
    }
    ]
]:={FigResolveCoordinate[x,1],FigResolveCoordinate[y,2]};


(*Generic bounding box generation for point-like objects*)


FigPointBoundingBox[p:{x_?NumericQ,y_?NumericQ}]:={{x,x},{y,y}};


(*Displacement geometry*)


(*A "displacement" is a difference of positions, i.e., a vector describing a translation*)


(*Displacement patterns*)


FigDisplacementPattern=None|({_?NumericQ,_?NumericQ}|Scaled[{_?NumericQ,_?NumericQ}]|Canvas[{_?NumericQ,_?NumericQ}])|{
  ((_?NumericQ)|(Scaled|Canvas)[(_?NumericQ)]),
  ((_?NumericQ)|(Scaled|Canvas)[(_?NumericQ)])
                           };
FigDisplacementSequencePattern[n_Integer]:=Repeated[_?(MatchQ[#,FigDisplacementPattern]&),{n,Infinity}];
FigDisplacementSetPattern[n_Integer]:={Repeated[_?(MatchQ[#,FigDisplacementPattern]&),{n,Infinity}]};


(*FigResolveDisplacement*)


FigResolveDisplacement[None]:={0,0};
FigResolveDisplacement[d:{_?NumericQ,_?NumericQ}]:=(CurrentWindow[]@DeltaTFunction[])@d;
FigResolveDisplacement[Canvas[d:{_?NumericQ,_?NumericQ}]]:=d;
FigResolveDisplacement[Scaled[d:{_?NumericQ,_?NumericQ}]]:=(CurrentWindow[]@ScaledDeltaTFunction[])@d;


(*Ad hoc treatment of hybrid coordinates*)


FigResolveDisplacement[
  p:Except[
    {_?NumericQ,_?NumericQ},
    {
      x:((_?NumericQ)|(Scaled|Canvas)[(_?NumericQ)]),
      y:((_?NumericQ)|(Scaled|Canvas)[(_?NumericQ)])
    }
    ]
]:={FigResolveCoordinateDisplacement[x,1],FigResolveCoordinateDisplacement[y,2]};


(*Offset geometry*)


(*An "offset" is a relative position within a rectangle*)


(*Named point offsets*)


(*Note: These are the offsets to the point.  The offset of text attached to the point should be the negative of this.*)


NamedPointPattern=Center|Left|Right|Bottom|Top|TopLeft|TopRight|BottomLeft|BottomRight;


NamedPointOffset[Center]={0,0};
NamedPointOffset[Left]={-1,0};
NamedPointOffset[Right]={+1,0};
NamedPointOffset[Bottom]={0,-1};
NamedPointOffset[Top]={0,+1};


NamedPointOffset[TopLeft]={-1,+1};
NamedPointOffset[TopRight]={+1,+1};
NamedPointOffset[BottomLeft]={-1,-1};
NamedPointOffset[BottomRight]={+1,-1};


(*Realizing offset*)


FigOffsetPattern=NamedPointPattern|NumericalPairPattern;


FigResolveOffset[Offset:NamedPointPattern]:=NamedPointOffset[Offset];
FigResolveOffset[Offset:NumericalPairPattern]:=Offset;


(*Window/panel geometry*)


(*Generic numerical region pattern*)


RangeParametersPattern=(None|(_?NumericQ)|{(_?NumericQ),(_?NumericQ)}|{{(_?NumericQ),(_?NumericQ)},{(_?NumericQ),(_?NumericQ)}});
NonNegativeRangeParametersPattern=(None|NonNegativePattern|{NonNegativePattern,NonNegativePattern}|{{NonNegativePattern,NonNegativePattern},{NonNegativePattern,NonNegativePattern}});
NumericalRegionPattern={{(_?NumericQ),(_?NumericQ)},{(_?NumericQ),(_?NumericQ)}};


(*Generic numerical region upgrade*)


(*for numeric data*)
(**)
(*general rules*)
(*	x -> {x,x}*)
(*	{x,y} -> {{x,x},{y,y}}*)
(*	{{x1,x2},{y1,y2}} left unchanged*)
(*	*)
(*special case*)
(*	None -> {{0,0},{0,0}}*)


UpgradeRangeParameters[xy_?NumericQ]:=UpgradeRangeParameters[{xy,xy}];
UpgradeRangeParameters[{x_?NumericQ,y_?NumericQ}]:=UpgradeRangeParameters[{{x,x},{y,y}}];
UpgradeRangeParameters[{{x1_?NumericQ,x2_?NumericQ},{y1_?NumericQ,y2_?NumericQ}}]:={{x1,x2},{y1,y2}};
UpgradeRangeParameters[None]={{0,0},{0,0}};


(*Region extension by coordinate amount*)


(*2-dimensional*)
(*Note: "Absolute" is SciDraw`Private legacy name.  Abs is accepted as global alternative.*)


ExtendRegion[PRange:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}},PDiff:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}},Mode:(Abs|Absolute)]:=PRange+PDiff*{{-1,+1},{-1,+1}};
ExtendRegion[PRange:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}},PFrac:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}},Mode:Scaled]:=PRange+PFrac*{{-1,+1},{-1,+1}}*-Subtract@@@PRange;


(*Window region specification pattern*)


(*Note: Pattern excludes case {_Integer,_Integer}, for panel in multipanel array, since this case requires special treatment.*)


FigRegionPattern=(All|{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}}|(Scaled|Canvas)[{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}}]);


(*Region resolution*)


FigResolveRegion[r:All]:=(CurrentWindow[]@CanvasRegion[]);
FigResolveRegion[r:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}}]:=TransformRegion[CurrentWindow[]@TFunction[],r];
FigResolveRegion[Canvas[r:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}}]]:=r;
FigResolveRegion[Scaled[r:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}}]]:=TransformRegion[CurrentWindow[]@ScaledTFunction[],r];


(*Region extension*)


(*Programmer level functions -- working in canvas coordinates*)
(*Serve to resolve "delta region patterns" to adjustments which can be handled arithmetically by ExtendRegion*)
(*Here "Scaled" refers to a fraction of the region which is being expanded, not of the current window.*)


(*FigResolveRegionExtension converts a user-acceptable region extension and returns arguments {{{x1,x2},{y1,y2}},{dx1,dx2},{dy1,dy2},Absolute|Scaled}  for use with ExtendRegion.*)


FigResolveRegionExtension[r:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}},d:None]:={r,UpgradeRangeParameters[0],Absolute};
FigResolveRegionExtension[r:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}},d:Automatic]:={r,UpgradeRangeParameters[0.02],Scaled};
FigResolveRegionExtension[r:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}},d:RangeParametersPattern]:={r,TransformRegion[CurrentWindow[]@DeltaTFunction[],UpgradeRangeParameters[d]],Absolute};
FigResolveRegionExtension[r:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}},Canvas[d:RangeParametersPattern]]:={r,UpgradeRangeParameters[d],Absolute};
FigResolveRegionExtension[r:{{_?NumericQ,_?NumericQ},{_?NumericQ,_?NumericQ}},Scaled[d:RangeParametersPattern]]:={r,UpgradeRangeParameters[d],Scaled};


(*General adjustment of region*)


(*User-level wrapper*)
(*	Returns result as valid region specification, wrapped in Canvas[]*)


FigDeltaRegionPattern=Automatic|None|RangeParametersPattern|(Scaled|Canvas)[RangeParametersPattern];


Options[AdjustRegion]={RegionExtension->None,RegionDisplacement->None};
AdjustRegion[r:FigRegionPattern,Opts___?OptionQ]:=Module[
  {
    FullOptions=Flatten[{Opts,Options[AdjustRegion]}],CanvasRegion
  },

  (* validate options *)
  FigCheckOption[AdjustRegion,RegionDisplacement,FigDisplacementPattern,FullOptions];
  FigCheckOption[AdjustRegion,RegionExtension,FigDeltaRegionPattern,FullOptions];

  (* resolve given region to canvas coordinates *)
  CanvasRegion=FigResolveRegion[r];

  (* do displacement *)
  CanvasRegion=CanvasRegion+FigResolveDisplacement[(RegionDisplacement/.FullOptions)];

  (* do extension *)
  CanvasRegion=ExtendRegion@@FigResolveRegionExtension[CanvasRegion,(RegionExtension/.FullOptions)];

  (* return *)
  Canvas[CanvasRegion]

                                                  ];
DeclareFigFallThroughError[AdjustRegion];


(*Region points*)


(*User-level region point extraction*)


RegionPoint[r:FigRegionPattern,Offset:FigOffsetPattern]:=Module[
  {UsedOffset,CanvasRegion,CanvasCenter,CanvasRadius},

  (* resolve geometry *)
  UsedOffset=FigResolveOffset[Offset];
  CanvasRegion=FigResolveRegion[r];
  CanvasCenter=Mean/@CanvasRegion;
  CanvasRadius=-(Subtract@@@CanvasRegion)/2;

  (* return offset point *)
  Canvas[CanvasCenter+UsedOffset*CanvasRadius]
                                                         ];


(*Object set bounding box*)


ExtractBoundingBox[Object[n:ObjectNamePattern[FigAnchor]]|(n:ObjectNamePattern[FigAnchor])]:=FigPointBoundingBox[Object[n]@GetPoint[]];
ExtractBoundingBox[Object[n:ObjectNamePattern[FigObject]]|(n:ObjectNamePattern[FigObject])]:=(Object[n]@MakeBoundingBox[]);


ObjectCanvasBox[ObjectList:{(ObjectPattern[FigAnchor]|ObjectNamePattern[FigAnchor]|ObjectPattern[FigObject]|ObjectNamePattern[FigObject])..}]:=Module[
  {RegionList,ExtremumLists},
  RegionList=(ExtractBoundingBox/@ObjectList);
  ExtremumLists=MapThread[List,RegionList,2]; (* {{xminlist,xmaxlist},{yminlist,ymaxlist}} *)
  MapAt[Max,MapAt[Min,ExtremumLists,{{1,1},{2,1}}],{{1,2},{2,2}}]
                                                                                                                                               ];


(*User-level bounding region function*)


BoundingBox[
  ObjectList:{(ObjectPattern[FigAnchor]|ObjectNamePattern[FigAnchor]|ObjectPattern[FigObject]|ObjectNamePattern[FigObject])..}
]:=Module[
  {},
  FigCheckInFigure[BoundingBox];
  Canvas[ObjectCanvasBox[ObjectList]]
   ];
BoundingBox[
  obj:((ObjectPattern[FigAnchor]|ObjectNamePattern[FigAnchor]|ObjectPattern[FigObject]|ObjectNamePattern[FigObject]))
]:=BoundingBox[{obj}];
DeclareFigFallThroughError[BoundingBox];


(*Edge-type parameter list interpretation*)


(*modeled on FrameLabel, etc.*)
(**)
(*general rules*)
(*	[B -> {B,B} -- contrary to Mathematica FrameLabel behavior {B,X}]*)
(*		This case is usually only reasonable a boolean parameter or a style parameter.  *)
(*			Pattern: EdgeXYSameParametersPattern[patt]*)
(*		Other properties are XY properties, and a value appropriate to X should never propagate to Y, except perhaps the value None.*)
(*			Patterns: EdgeXYUniqueParametersPattern[patt]*)
(*		It is up to the *pattern* to exclude other possibilities, such as AxisLabel->"x".*)
(*	{B,L} -> {{L,X},{B,X}}    no mirroring*)
(*		or {{L,L},{B,B}}    mirroring*)
(*	{{L,R},{B,T}} left unchanged*)
(*legacy list format rules*)
(*	{B} was legitimate legacy case but is disallowed in present conversion*)
(*	{B,L,T} was legitimate legacy case but is disallowed in present conversion*)
(*	{B,L,T,R}->{{L,R},{B,T}}*)
(*where X is given filler*)
(**)
(*No special treatment is imposed on the value None.  For instance, it is not translated into Filler.*)


(*example usage: *)
(*	EdgeParametersPattern[NonListPattern]) -- includes None as allowable case*)
(*	None|EdgeParametersPattern[FlatListPattern]*)


(*Masking of edge options*)


(*EdgeMaskingFunction[DataEntry_,MaskEntry_,ExteriorEdgeMaskEntry_,Filler_] masks DataEntry, i.e., returning either DataEntry or Filler.  DataEntry is returned if the mask value is True or is Exterior on an exterior edge (as indicated by ExteriorEdgeMaskEntry).*)


EdgeMaskingFunction[DataEntry_,MaskEntry_,ExteriorEdgeMaskEntry_,Filler_]:=If[
  (MaskEntry===True)||((MaskEntry===Exterior)&&ExteriorEdgeMaskEntry),
  DataEntry,
  Filler
                                                                           ];


MaskEdgeOption[
  Data:{{_,_},{_,_}},
  Mask:{{Exterior|LogicalPattern,Exterior|LogicalPattern},{Exterior|LogicalPattern,Exterior|LogicalPattern}},ExteriorEdgeMask:{{LogicalPattern,LogicalPattern},{LogicalPattern,LogicalPattern}},
  Filler_
]:=MapThread[
  EdgeMaskingFunction[##,Filler]&,
                     {Data,Mask,ExteriorEdgeMask},
  2
   ];


(*Automatic values for edge options*)


ResolveAutomaticEdgeOption[
  Data:{{_,_},{_,_}},
  Defaults:{{_,_},{_,_}}
]:=MapThread[
  Replace[#1,{Automatic->#2}]&,
         {Data,Defaults},
  2
   ];


ResolveAutomaticEdgeOption[{{1,2},{3,Automatic}},{{5,6},{7,8}}]


(*Single-coordinate geometry*)


(*Single-coordinate pattern*)


FigCoordinatePattern=((_?NumericQ)|(Scaled|Canvas)[(_?NumericQ)]|FigPointPattern);


(*Complementary coordinate index*)


AntiCoordinateIndex[1]=2;
AntiCoordinateIndex[2]=1;


(*Interpret Horizontal/Vertical as coordinate indices*)


FigResolveCoordinateIndex[Horizontal]=1;
FigResolveCoordinateIndex[Vertical]=2;


(*Conversion of coordinate system for individual coordinate*)


(*must come *after* point geometry, since uses FigPointPattern*)
(*can be used eith to convert a single coordinate to canvas coordinates or to extract canvas coordinate from a point*)


(*Conversions*)


FigResolveCoordinate[x_?NumericQ,CoordinateIndex:(1|2)]:=FigResolvePoint[x*UnitVector[CoordinateIndex]][[CoordinateIndex]];
FigResolveCoordinate[Scaled[x_?NumericQ],CoordinateIndex:(1|2)]:=FigResolvePoint[Scaled[x*UnitVector[CoordinateIndex]]][[CoordinateIndex]];
FigResolveCoordinate[Canvas[x_?NumericQ],CoordinateIndex:(1|2)]:=x;
FigResolveCoordinate[p:FigPointPattern,CoordinateIndex:(1|2)]:=FigResolvePoint[p][[CoordinateIndex]];


(*Interpret Horizontal/Vertical as arguments*)


FigResolveCoordinate[x_,Horizontal]:=FigResolveCoordinate[x,1];
FigResolveCoordinate[x_,Vertical]:=FigResolveCoordinate[x,2];


(*Conversion of coordinate system for individual coordinate*)


(*Conversions*)


FigResolveCoordinateDisplacement[x_?NumericQ,CoordinateIndex:(1|2)]:=FigResolveDisplacement[x*UnitVector[CoordinateIndex]][[CoordinateIndex]];
FigResolveCoordinateDisplacement[Scaled[x_?NumericQ],CoordinateIndex:(1|2)]:=FigResolveDisplacement[Scaled[x*UnitVector[CoordinateIndex]]][[CoordinateIndex]];
FigResolveCoordinateDisplacement[Canvas[x_?NumericQ],CoordinateIndex:(1|2)]:=x;
FigResolveCoordinateDisplacement[p:FigPointPattern,CoordinateIndex:(1|2)]:=FigResolveDisplacement[p][[CoordinateIndex]];


(*Interpret Horizontal/Vertical as arguments*)


FigResolveCoordinateDisplacement[x_,Horizontal]:=FigResolveCoordinateDisplacement[x,1];
FigResolveCoordinateDisplacement[x_,Vertical]:=FigResolveCoordinateDisplacement[x,2];


(*Point set geometry*)


(*Point set pattern*)


FigPointSetPattern[n_Integer]:={Repeated[FigPointPattern,{n,Infinity}]};


(*Centroid of point set*)


(*Note: Returns a Canvas point specification, rather than just a canvas coordinate pair.  This is so the user can use PointCentroid[] as a point specification.*)


CentroidPoint[PointSet:FigPointSetPattern[1]]:=Canvas[Mean[FigResolvePoint/@PointSet]];


(*Bounding box of point set*)


FigPointSetBoundingBox[PointSet:FigPointSetPattern[1]]:={{Min[First/@PointSet],Max[First/@PointSet]},{Min[Last/@PointSet],Max[Last/@PointSet]}};


(*Rectangle/ellipse geometry*)


(*Radius pattern*)


FigRadiusPattern=NonNegativeIntervalParametersPattern|((Horizontal|Vertical|Canvas|Scaled)[NonNegativeIntervalParametersPattern])|{
  ((_?NonNegative)|(Scaled|Canvas)[(_?NonNegative)]),
  ((_?NonNegative)|(Scaled|Canvas)[(_?NonNegative)])
                                                     };


(*Geometry calculations*)


(*calculate {CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle} for a rectangle*)
(*this function takes care of point resolution for the arguments but does no option validation*)


(*given center (with option Radius)*)


MakeRectangleGeometry[p:FigPointPattern,FullOptions_List]:=Module[
  {Anchor,UsedAnchorOffset,UsedPivotOffset,
   CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle},

  (* resolve arguments *)
  Anchor=FigAnchor[p];
  CanvasRadius=FigResolveRadius[(Radius/.FullOptions)];
  UsedAnchorOffset=FigResolveOffset[(AnchorOffset/.FullOptions)];
  UsedPivotOffset=FigResolveOffset[ResolveOption[PivotOffset,{Automatic->UsedAnchorOffset},FullOptions]];
  RotationAngle=UpgradeScalar[ResolveOption[Rotate,{Automatic->AnchorAngle[Anchor]},FullOptions]];

  (* derived geometry *)
  CanvasCenter=FigResolvePoint[Anchor]-UsedAnchorOffset*CanvasRadius;
  CanvasPivot=CanvasCenter+UsedPivotOffset*CanvasRadius;

  (* combined answer *)
  {CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle}
                                                           ];


(*given diametric corners*)


MakeRectangleGeometry[p1:FigPointPattern,p2:FigPointPattern,FullOptions_List]:=Module[
  {CanvasCorner1,CanvasCorner2,CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedRoundingRadius},

  (* generate geometry *)
  CanvasCorner1=FigResolvePoint[p1];
  CanvasCorner2=FigResolvePoint[p2];
  CanvasCenter=(CanvasCorner2+CanvasCorner1)/2;
  CanvasRadius=Abs[(CanvasCorner2-CanvasCorner1)/2];  (* Abs allows any two points to be taken, not just lower left to upper right *)

  (* invoke base form *)
  MakeRectangleGeometry [
    Canvas[CanvasCenter],
    Join[{AnchorOffset->Center,Radius->Canvas[CanvasRadius]},FullOptions]
  ]

                                                                               ];


(*given region specification*)


MakeRectangleGeometry[r:FigRegionPattern,FullOptions_List]:=Module[
  {CanvasRegion,CanvasCorner1,CanvasCorner2,CanvasCenter,CanvasPivot,CanvasRadius,RotationAngle,UsedRoundingRadius},

  (* generate geometry *)
  CanvasRegion=FigResolveRegion[r];
  {CanvasCorner1,CanvasCorner2}=Transpose@CanvasRegion;
  CanvasCenter=(CanvasCorner2+CanvasCorner1)/2;
  CanvasRadius=Abs[(CanvasCorner2-CanvasCorner1)/2]; (* Abs allows any two points to be taken, not just lower left to upper right *)

  (* invoke base form *)
  MakeRectangleGeometry [
    Canvas[CanvasCenter],
    Join[{AnchorOffset->Center,Radius->Canvas[CanvasRadius]},FullOptions]
  ]
                                                            ];


(*Make region given rectangle specification*)


(*User-level function to convert a rectangle specification to a Canvas region specification*)
(*For rotated rectangle, circumscribing bounding box is used*)


(*TODO option resultion and validation*)


(*RectangleRegion[Args___]:=Canvas[FigRectangleBoundingBox@@MakeRectangleGeometry[Args]];*)


(*Standard rectangle geometry options*)


FigRectangleOptions={
  (* geometry *)
  Radius->1,AnchorOffset->Center,PivotOffset->Automatic,Rotate->None};


FigCheckRectangleOptions[Self_Object]:=Module[
  {},
  FigCheckOption[Self,Radius,FigRadiusPattern,FigOptions];
  FigCheckOption[Self,AnchorOffset,FigOffsetPattern,FigOptions];
  FigCheckOption[Self,PivotOffset,Automatic|FigOffsetPattern,FigOptions];
  FigCheckOption[Self,Rotate,Automatic|ScalarParameterPattern,FigOptions];
                                       ];


(*FigResolveRadius*)


(*closely related to FigResolveDisplacement:*)
(*	-- upgrades single radius argument*)
(*	-- allows for "vertical" or "horizontal" units*)
(*	-- requires nonnegative arguments*)


(*User coordinates: None or r or {rx,ry}*)


FigResolveRadius[r:NonNegativeIntervalParametersPattern]:=(CurrentWindow[]@DeltaTFunction[])@UpgradePairEqual[r];


(*Horizontal user coordinates: Horizontal[None] or Horizontal[r] or Horizontal[{rx,ry}]*)


FigResolveRadius[Horizontal[r:NonNegativeIntervalParametersPattern]]:=Module[
  {LengthUnit},
  LengthUnit=First[(CurrentWindow[]@DeltaTFunction[])@{1,0}];
  LengthUnit*UpgradePairEqual[r]
                                                                      ];


(*Vertical user coordinates: Vertical[None] or Vertical[r] or Vertical[{rx,ry}]*)


FigResolveRadius[Vertical[r:NonNegativeIntervalParametersPattern]]:=Module[
  {LengthUnit},
  LengthUnit=Last[(CurrentWindow[]@DeltaTFunction[])@{0,1}];
  LengthUnit*UpgradePairEqual[r]
                                                                    ];


(*Canvas coordinates: Canvas[None] or Canvas[r] or Canvas[{rx,ry}]*)


FigResolveRadius[Canvas[r:NonNegativeIntervalParametersPattern]]:=UpgradePairEqual[r];


(*Scaled coordinates: Scaled[None] or Scaled[r] or Scaled[{rx,ry}]*)


FigResolveRadius[Scaled[r:NonNegativeIntervalParametersPattern]]:=(CurrentWindow[]@ScaledDeltaTFunction[])@UpgradePairEqual[r];


(*Ad hoc treatment of hybrid coordinates*)


FigResolveRadius[
  p:Except[
    {_?NumericQ,_?NumericQ},
    {
      x:((_?NonNegative)|(Scaled|Canvas)[(_?NonNegative)]),
      y:((_?NonNegative)|(Scaled|Canvas)[(_?NonNegative)])
    }
    ]
]:=FigResolveDisplacement[p];


(*Rectangle "circumference" points*)


(*used also for circle geometry*)


RectangleOffsetPoint[CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,Offset:{_?NumericQ,_?NumericQ}]:=RotationTransform[RotationAngle,CanvasPivot]@(CanvasCenter+CanvasRadius*Offset);


(*segments orientations are in the "upward" and "rightward" directions (with respect to the original unrotated rectangle)*)


RectangleSideSegment[CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,Side:Left]:={
  RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,{-1,-1}],
  RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,{-1,+1}]
  };
RectangleSideSegment[CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,Side:Right]:={
  RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,{+1,-1}],
  RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,{+1,+1}]
  };
RectangleSideSegment[CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,Side:Bottom]:={
  RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,{-1,-1}],
  RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,{+1,-1}]
  };
RectangleSideSegment[CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,Side:Top]:={
  RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,{-1,+1}],
  RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,{+1,+1}]
  };


(*Rectangle anchors*)


(*A "rectangle" here more generally means a tilted rectangle*)


(*Named points: Center (default), Left/Right, Bottom/Top*)
(*	identical to circle, sans AngleRange parameter*)


FigRectangleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  Name:NamedPointPattern,
  Arg:None
]:=FigAnchor[Canvas[RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,NamedPointOffset[Name]]],-NamedPointOffset[Name],RotationAngle];


FigRectangleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  Name:Offset,
  OffsetValue:FigOffsetPattern
]:=FigAnchor[Canvas[RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,FigResolveOffset[OffsetValue]]],{0,0},RotationAngle];


(*Side interpolations: {Left|Right|Bottom|Top,x}*)
(*Note: Simple side points could be absorbed as special case by making x default to 0 but are already handled as named points above.*)
(*Reference:(Tail|Center|Head):Center,Mode:(Absolute|Scaled):Scaled*)


FigRectangleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  Name:(Left|Right|Bottom|Top),
  x_?NumericQ
]:=FigAnchor[
  Canvas[
    InterpolateSegment[
      RectangleSideSegment[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,Name],
      Center,Scaled,x
    ]
  ],
  -NamedPointOffset[Name],
  RotationAngle
   ];


(*Rectangle bounding box*)


FigRectangleBoundingBox[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ
]:=FigPointSetBoundingBox[Table[
  RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,Offset],
  {Offset,{{-1,-1},{-1,+1},{+1,-1},{+1,+1}}}
                          ]
   ];


(*Circle anchors*)


(*A "circle" here more generally means a tilted ellipse*)


(*Named points: Center (default), Left/Right, Bottom/Top*)


FigCircleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},AngleRange:{theta1_?NumericQ,theta2_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  Name:(Center|Left|Right|Bottom|Top),
  Arg:None
]:=FigAnchor[Canvas[RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,NamedPointOffset[Name]]],-NamedPointOffset[Name],RotationAngle];


FigCircleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},AngleRange:{theta1_?NumericQ,theta2_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  Name:Offset,
  OffsetValue:FigOffsetPattern
]:=FigAnchor[Canvas[RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,FigResolveOffset[OffsetValue]]],{0,0},RotationAngle];


(*For a circle, the conventional direction is the "positive theta" direction from polar coordinates, i.e., CCW.  For instance, this is the convention followed by the ordering of the arc angle arguments {theta1,theta2} in the Mathematica Circle and Disk primatives.  However, it is more natural for the text baseline to follow the "negative theta" tangent, so that the text "sits on" the outside of the circle.  This is the convention adopted with the anchors for Point, for instance.  For the most part, TextRectify renders this problem irrelevant.  However, the orientation of the tangent also affect arrowheads.  *)


(*Curve anchors:*)
(*	Normal/Tangent, t|Scaled[t]*)


(*Previously: FigCircle sorted the AngleRange angles to *enforce* a CCW curve.  The solution was to use the "negative theta" tangent, but still have the scaled curve parameter run from 0=Tail to 1=Head going CCW, and then flip the tangent used in drawing arrowheads (with RotateAnchor[Self@MakeAnchor[Tail,None],Pi],RotateAnchor[Self@MakeAnchor[Head,None],Pi]).*)


(*FigCircleAnchor[*)
(*   CanvasCenter : {_?NumericQ, _?NumericQ}, CanvasRadius : {_?NumericQ, _?NumericQ}, AngleRange : {theta1_?NumericQ, theta2_?NumericQ}, CanvasPivot : {_?NumericQ, _?NumericQ}, RotationAngle_?NumericQ,*)
(*   OrthoMode : (Normal | Tangent), Arg : ((t_?NumericQ) | Scaled[t_?NumericQ])*)
(*   ] := Module[*)
(*   {theta, AnchorPoint, TangentAngle, AnchorAngle, AnchorOffset},*)
(*   *)
(*   theta = Switch[*)
(*     Arg,*)
(*     _?NumericQ, t, (* literal angle, before rotation *)*)
(*     Scaled[_?NumericQ], RescaleInterval[{0, 1}, {theta1, theta2}][t]  (* scaled from 0 to 1 on displayed arc *)*)
(*     ];*)
(*   AnchorPoint = RotationTransform[RotationAngle, CanvasPivot]@(CanvasCenter + CanvasRadius*{Cos[theta], Sin[theta]});*)
(*   AnchorOffset = Switch[OrthoMode, Normal, {-1, 0}, Tangent, {0, -1}];*)
(*   TangentAngle = VectorArcTan[CanvasRadius*{Sin[theta], -Cos[theta]}] + RotationAngle; (* tangent vector in negative theta sense *)*)
(*   AnchorAngle = TangentAngle + Switch[OrthoMode, Normal, +Pi/2, Tangent, 0];*)
(*   *)
(*   FigAnchor[Canvas[AnchorPoint], AnchorOffset, AnchorAngle]*)
(*   ];*)


(*Revised: FigCircle leaves the angles unsorted, so curve sense can be either way.  The d(theta) for the tangent is in the same sense as the delta(theta) of the angles.  The scaled curve parameter runs from 0=Tail to 1=Head.*)


FigCircleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},AngleRange:{theta1_?NumericQ,theta2_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  OrthoMode:(Normal|Tangent),
  Arg:((t_?NumericQ)|{"Angle",t_?NumericQ})
]:=Module[
  {theta,AnchorPoint,TangentAngle,AnchorAngle,AnchorOffset,ArcSense},

  (* arc sense: +1 for CCW or -1 for CW *)
  ArcSense=Sign[theta2-theta1];

  theta=Switch[
    Arg,
    {"Angle",_?NumericQ},t, (* literal angle, before rotation *)
    _?NumericQ,RescaleInterval[{0,1},{theta1,theta2}][t]  (* scaled from 0 to 1 on displayed arc *)
        ];
  AnchorPoint=RotationTransform[RotationAngle,CanvasPivot]@(CanvasCenter+CanvasRadius*{Cos[theta],Sin[theta]});
  AnchorOffset=Switch[OrthoMode,Normal,{1,0},Tangent,{0,1}]*ArcSense;
  (* trig gives tangent vector in negative theta sense -- for arc tangent, this must be flipped if delta theta is positive; for outward normal, this must have Pi/2 added  *)
  TangentAngle=VectorArcTan[CanvasRadius*{Sin[theta],-Cos[theta]}]+RotationAngle; 
  (* trig gives tangent vector in negative theta sense, which y  *)
  AnchorAngle=TangentAngle+Switch[OrthoMode,Normal,+Pi/2,Tangent,Switch[ArcSense,+1,Pi,-1|0,0]];

  FigAnchor[Canvas[AnchorPoint],AnchorOffset,AnchorAngle]
   ];
FigCircleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},AngleRange:{theta1_?NumericQ,theta2_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  OrthoMode:(Normal|Tangent),
  Arg:None]:=FigCircleAnchor[CanvasCenter,CanvasRadius,AngleRange,CanvasPivot,RotationAngle,
                             OrthoMode,0.5
             ];


(*Head/Tail anchors -- tangent at endpoint of arc*)


FigCircleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},AngleRange:{theta1_?NumericQ,theta2_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  Name:Tail,
  Arg:None
]:=FigCircleAnchor[CanvasCenter,CanvasRadius,AngleRange,CanvasPivot,RotationAngle,Tangent,0];
FigCircleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},AngleRange:{theta1_?NumericQ,theta2_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  Name:Head,
  Arg:None
]:=FigCircleAnchor[CanvasCenter,CanvasRadius,AngleRange,CanvasPivot,RotationAngle,Tangent,1];


(*Bounding radii*)
(*	default text offset chosen to put labels outside if arc is in CCW (positive) sense*)


FigCircleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},AngleRange:{theta1_?NumericQ,theta2_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  Name:(HeadRadius|TailRadius),
  Arg:(p_?NumericQ)
]:=Module[
  {CenterPoint,EndPoint},
  CenterPoint=FigResolvePoint[FigCircleAnchor[CanvasCenter,CanvasRadius,AngleRange,CanvasPivot,RotationAngle,Center,None]];
  EndPoint=FigResolvePoint[FigCircleAnchor[CanvasCenter,CanvasRadius,AngleRange,CanvasPivot,RotationAngle,Switch[Name,HeadRadius,Head,TailRadius,Tail],None]];
  FigCurveAnchorFromPoints[{CenterPoint,EndPoint},Switch[Name,HeadRadius,Left,TailRadius,Right],p]
   ];
FigCircleAnchor[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},AngleRange:{theta1_?NumericQ,theta2_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ,
  Name:(HeadRadius|TailRadius),
  Arg:None]:=FigCircleAnchor[CanvasCenter,CanvasRadius,AngleRange,CanvasPivot,RotationAngle,
                             Name,0.5
             ];


(*Circle bounding box*)


(*currently just calculated as bounding box of principal axis end points*)
(*	-- works for nonrotated ellipse*)
(*	-- will underestimage bounding box for tilted ellipse*)
(*	-- neglects possible restriction to arc*)


FigCircleBoundingBox[
  CanvasCenter:{_?NumericQ,_?NumericQ},CanvasRadius:{_?NumericQ,_?NumericQ},AngleRange:{theta1_?NumericQ,theta2_?NumericQ},CanvasPivot:{_?NumericQ,_?NumericQ},RotationAngle_?NumericQ
]:=FigPointSetBoundingBox[Table[
  RectangleOffsetPoint[CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle,Offset],
  {Offset,{{0,-1},{0,+1},{-1,0},{+1,0}}}
                          ]
   ];


(*Anchor manipulation and retrieval*)


(*Anchor translation (displacement)*)


(*	Note: These functions return an anchor *object*, not an anchor name.*)
(*	These definitions must come after all relevant argument patterns have been defined above.*)


DisplacePoint[p:FigPointPattern,Args:FigDisplacementSequencePattern[0]]:=Module[
  {a,CanvasPoint},
  FigCheckInFigure[DisplacePoint];
  a=FigAnchor[p];
  CanvasPoint=a@GetPoint[]+Plus@@(FigResolveDisplacement/@{Args});
  FigAnchor[Canvas[CanvasPoint],a@GetOffset[],a@GetAngle[]]
                                                                         ];
DeclareFigFallThroughError[DisplacePoint];


ProjectPoint[p:FigPointPattern,CoordinateIndexName:(Horizontal|Vertical),x:FigCoordinatePattern]:=Module[
  {CoordinateIndex,xc,a,CanvasPoint},

  FigCheckInFigure[ProjectPoint];

  (* resolve coordinate *)
  CoordinateIndex=FigResolveCoordinateIndex[CoordinateIndexName];
  xc=FigResolveCoordinate[x,CoordinateIndex];

  (* substitute coordinate *)
  a=FigAnchor[p];
  CanvasPoint=ReplacePart[(a@GetPoint[]),CoordinateIndex->xc];
  FigAnchor[Canvas[CanvasPoint],a@GetOffset[],a@GetAngle[]]
                                                                                                  ];
DeclareFigFallThroughError[ProjectPoint];


DisplaceAlongAnchor[p:FigPointPattern,d_?NumericQ]/;(SciDraw`Private`$InFigure):=Module[
  {a,theta,CanvasPoint},
  FigCheckInFigure[DisplacePoint];
  a=FigAnchor[p];
  theta=a@GetAngle[];
  CanvasPoint=a@GetPoint[]+d*{Cos[theta],Sin[theta]};
  FigAnchor[Canvas[CanvasPoint],a@GetOffset[],a@GetAngle[]]
                                                                                 ];
DeclareFigFallThroughError[DisplaceAlongAnchor];


(*Anchor rotation*)


RotateAnchor[p:FigPointPattern,theta:ScalarParameterPattern]/;(SciDraw`Private`$InFigure):=Module[
  {Anchor,CanvasPoint},
  FigCheckInFigure[RotateAnchor];
  Anchor=FigAnchor[p];
  FigAnchor[Canvas[Anchor@GetPoint[]],Anchor@GetOffset[],Anchor@GetAngle[]+UpgradeScalar[theta]]
                                                                                           ];


(*Anchor angle retrieval*)


(*User-level anchor retrieval functions*)


AnchorAngle[p:FigPointPattern]:=Module[
  {Anchor},
  FigCheckInFigure[AnchorAngle];
  Anchor=FigAnchor[p];
  Anchor@GetAngle[]
                                ];


(*Anchor offset retrieval*)


(*User-level anchor retrieval functions*)


AnchorOffset[p:FigPointPattern]:=Module[
  {Anchor},
  FigCheckInFigure[AnchorAngle];
  Anchor=FigAnchor[p];
  Anchor@GetOffset[]
                                 ];


(*Anchor point retrieval -- in user coordinates*)


AnchorCoordinates[p:FigPointPattern]:=Module[
  {Anchor,CanvasCoordinates},
  FigCheckInFigure[AnchorCoordinates];
  Anchor=FigAnchor[p];
  CanvasCoordinates=Anchor@GetPoint[];
  (CurrentWindow[]@InverseTFunction[])@CanvasCoordinates
                                      ];


(*Canvas angle for ray between anchors*)


CanvasRayAngle[{p1:FigPointPattern,p2:FigPointPattern}]:=Module[
  {CanvasCoordinates1,CanvasCoordinates2},
  FigCheckInFigure[CanvasAngle];
  VectorArcTan[FigResolvePoint[p2]-FigResolvePoint[p1]]
                                                         ];


(*Curve geometry*)


(*Curve pattern*)


FigCurvePointPattern=FigPointPattern|(DisplaceTail|DisplaceHead)[FigDisplacementSequencePattern[0]]|(DisplaceAlongTail|DisplaceAlongHead)[(_?NumericQ)]|(ProjectTail|ProjectHead)[(Horizontal|Vertical),FigCoordinatePattern];
FigCurvePointSetPattern={Repeated[FigCurvePointPattern,{2,Infinity}]};
FigCurvePattern=FigCurvePointSetPattern|(_Graphics)|(_ContourGraphics);


(*Standard curve options*)


FigCurveOptions={
  (* curve geometry *)
  TailRecess->None,HeadRecess->None,

  (* curve extraction *)
  Line->1

};


FigCheckCurveOptions[Self_Object]:=Module[
  {},
  FigCheckOption[Self,TailRecess,ScalarParameterPattern,FigOptions];
  FigCheckOption[Self,HeadRecess,ScalarParameterPattern,FigOptions];
  FigCheckOption[Self,Line,Join|All|((_Integer)?Positive),FigOptions];

                                   ];


(*Curve resolution -- point set*)


(*Resolves list of two or more points into canvas coordinates, making use of the special curve options for head and tail positioning.*)


General::figcurveref="Circular reference in curve specification, with tail point `1` and head point `2`.";


FigResolveCurve[Self_Object,Points:FigCurvePointSetPattern,FullOptions_List]:=Module[
  {p1,p2,DereferencedPoints,CanvasPoints,CanvasHead,CanvasTail},

  (* resolve tail and head displacement references *)
  p1=First[Points];
  p2=Last[Points];
  If[
    (* sanity checks -- head cannot be relative to head, tail cannot be relative to tail, and head can't be chasing tail circularly *)
    MatchQ[p1,(_DisplaceTail)|(_DisplaceAlongTail)|(_ProjectTail)]||MatchQ[p2,(_DisplaceHead)|(_DisplaceAlongHead)|(_ProjectHead)]||(MatchQ[p1,(_DisplaceHead)|(_DisplaceAlongHead)|(_ProjectHead)]&&MatchQ[p2,(_DisplaceTail)|(_DisplaceAlongTail)|(_ProjectTail)]),
    FigError[Self,"figcurveref",p1,p2]
  ];
  DereferencedPoints=Replace[Points,{
    DisplaceTail[ArgSeq___]:>DisplacePoint[p1,ArgSeq],
    ProjectTail[ArgSeq___]:>ProjectPoint[p1,ArgSeq],
    DisplaceAlongTail[ArgSeq___]:>DisplaceAlongAnchor[p1,ArgSeq],
    DisplaceHead[ArgSeq___]:>DisplacePoint[p2,ArgSeq],
    ProjectHead[ArgSeq___]:>ProjectPoint[p2,ArgSeq],
    DisplaceAlongHead[ArgSeq___]:>DisplaceAlongAnchor[p2,ArgSeq]
                             },{1}];

  (* resolve points *)
  CanvasPoints=FigResolvePoint/@DereferencedPoints;

  (* do tail and head recess *)
  CanvasTail=InterpolateSegment[
    Take[CanvasPoints,2],
    Tail,Absolute,
    UpgradeScalar[(TailRecess/.FullOptions)]
             ];
  CanvasHead=InterpolateSegment[
    Take[CanvasPoints,-2],
    Head,Absolute,
    -UpgradeScalar[(HeadRecess/.FullOptions)]
             ];
  ReplacePart[CanvasPoints,{1->CanvasTail,-1->CanvasHead}]

                                                                              ];


(*Curve resolution -- from graphics*)


FigResolveCurve[Self_Object,G:((_Graphics)|(_ContourGraphics)),FullOptions_List]:=Module[
  {GrabbedPoints,CanvasPoints,CanvasTail,CanvasHead},

  (* resolve points *)
  GrabbedPoints=GrabPoints[G,FilterRules[FullOptions,Options[GrabPoints]]];
  (* flatten a list of curves *)
  If[
    ((Line/.FullOptions)==All)&&(Length[GrabbedPoints]!=0),
    GrabbedPoints=Flatten[GrabbedPoints,1]
  ];
  FigCheckValue[Self,GrabbedPoints,{Repeated[NumericalPairPattern,{2,Infinity}]},"list of extracted points"];
  CanvasPoints=FigResolvePoint/@GrabbedPoints;

  (* do tail and head recess *)
  CanvasTail=InterpolateSegment[
    Take[CanvasPoints,2],
    Tail,Absolute,
    UpgradeScalar[(TailRecess/.FullOptions)]
             ];
  CanvasHead=InterpolateSegment[
    Take[CanvasPoints,-2],
    Head,Absolute,
    -UpgradeScalar[(HeadRecess/.FullOptions)]
             ];
  ReplacePart[CanvasPoints,{1->CanvasTail,-1->CanvasHead}]

                                                                                  ];


(*Curve point geometry*)


(*Generation of underlying interpolation functions*)
(*Returns: {fRx,fRy} where fRx and fRy are scalar-valued functions of the scaled arc-length parameter [0,1]*)
(*	fRx: u -> Rx*)
(*	fRy: u -> Ry*)
(*The curve which these functions interpolate along is the simple segment-by-segment linear interpolation of the given curve points.	*)


LinearInterpolationFunctionsRxRy[CanvasPoints_List]:=Module[
  {
    Segments,SegmentLengths,TotalLength,CurveParameterValues,
    XFunction,YFunction,DXFunction,DYFunction
  },
  Segments=Partition[CanvasPoints,2,1];
  SegmentLengths=SegmentLength/@Segments;
  CurveParameterValues=Prepend[Accumulate[SegmentLengths],0];
  TotalLength=Last[CurveParameterValues];
  CurveParameterValues=CurveParameterValues/TotalLength;

  (* set up curve interpolation *)
  (* functions for x[d], y[d], theta[d] *)

  XFunction=Interpolation[Transpose[{CurveParameterValues,First/@CanvasPoints}],InterpolationOrder->1];
  YFunction=Interpolation[Transpose[{CurveParameterValues,Last/@CanvasPoints}],InterpolationOrder->1];

  {XFunction,YFunction}

                                                     ];


(*Interpolation functions -- LEGACY packaging*)
(**)
(*Generation of underlying interpolation functions*)
(*	for curve itself and for tangent*)
(*Returns: {R,T} where R and T are vector-valued functions of the scaled arc-length parameter [0,1]*)
(*	R: u -> {Rx,Ry}*)
(*	T: u -> {Tx,Ty}*)
(*The curve which these functions interpolate along is the simple segment-by-segment linear interpolation of the given curve points.	*)


LinearInterpolationFunctionPair[CanvasPoints_List]:=Module[
  {
    XFunction,YFunction,DXFunction,DYFunction
  },
  {XFunction,YFunction}=LinearInterpolationFunctionsRxRy[CanvasPoints];
  {DXFunction,DYFunction}=Derivative[1]/@{XFunction,YFunction};

  (* construct functions *)
  (* note: use of With is necessary to force correct functions into pure function definitions, rather than just leaving references to the local symbols XFunction, etc. *)
  With[
    {x=XFunction,y=YFunction,dx=DXFunction,dy=DYFunction},
    {{x[#],y[#]}&,{dx[#],dy[#]}&}
  ]

                                                    ];


(*Segment extraction*)


CurveSegment::badsegment="A curve with `1` points does not have a segment numbered `2`.";
CurveSegment::badsegmentzero="Zero is not a valid segment number.";


CurveSegment[Points_List,s_Integer]:=Module[
  {n=Length[Points]},
  Which[
    s>n-1,FigError[CurveSegment,"badsegment",n,s],
    s<-(n-1),FigError[CurveSegment,"badsegment",n,s],
    s==0,FigError[CurveSegment,"badsegmentzero"],
    s>0,Points[[s;;s+1]],
    s<0,Points[[s-1;;s]]
  ]
                                     ];


(*Point extraction*)


CurvePoint::badpoint="A curve with `1` points does not have a point numbered `2`.";
CurvePoint::badpointzero="Zero is not a valid segment number.";


CurvePoint[Points_List,k_Integer]:=Module[
  {n=Length[Points]},
  Which[
    k>n,FigError[CurvePoint,"badsegment",n,k],
    k<-n,FigError[CurvePoint,"badsegment",n,k],
    k==0,FigError[CurvePoint,"badsegmentzero"],
    k>0,Points[[k]],
    k<0,Points[[k]]
  ]
                                   ];


(*Wrapper for case where interpolation should be linear interpolation of points*)


(* generate interpolating functions and pass these through to FigCurveAnchor functions (or pass dummies None if interpolating functions not needed) *)


FigCurveAnchorFromPoints[Points_List,Name:Left|Center|Right|Head|Tail,Arg_]:=FigCurveAnchor[Points,LinearInterpolationFunctionPair[Points],Name,Arg];
FigCurveAnchorFromPoints[Points_List,Name:Point,Arg_]:=FigCurveAnchor[Points,{None,None},Name,Arg];


(*Curve anchor -- on a segment*)


(*wraps recursive call to FigCurveAnchor zooming on given segment*)
(**)
(*Argument: {s,...} -- within segment s*)


FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Name_,{s_Integer,Arg_}]:=FigCurveAnchorFromPoints[
  CurveSegment[Points,s],
  Name,
  Arg
                                                                                                                        ];


(*Curve anchors relative to full curve -- Left/Center/Right*)


(*Argument: None -- resolves to scaled arc length 0.5*)


FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Name:(Left|Center|Right),None]:=FigCurveAnchor[Points,InterpolationFunctionPair,Name,0.5];


(*Argument: u -- scaled arc length*)


FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Name:(Left|Center|Right),u:(_?NumericQ)]:=Quiet[
  FigAnchor[
    Canvas[InterpolationFunction[u]],
    Switch[Name,Right,{0,+1},Center,{0,0},Left,{0,-1}],
    VectorArcTan[TangentFunction[u]] 
  ],
  {InterpolatingFunction::dmval}  (* allow anchors past end of line *)
                                                                                                                                         ];


(*Arg: {DisplaceAlongHead|DisplaceAlongTail,d} -- tangent displacement from tail/head*)


FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Name:(Left|Center|Right),{DisplaceAlongTail,d_?NumericQ}]:=DisplaceAlongAnchor[
  FigCurveAnchor[Points,InterpolationFunctionPair,Center,0],
  d
                                                                                                                                                          ];
FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Name:(Left|Center|Right),{DisplaceAlongHead,d_?NumericQ}]:=DisplaceAlongAnchor[
  FigCurveAnchor[Points,InterpolationFunctionPair,Center,1],
  d
                                                                                                                                                          ];


(*Arg: {Horizontal|Vertical,x} -- at given horizontal/vertical coordinate (with x in general syntax for individual coordinate)*)
(**)
(*Arg: {Horizontal|Vertical,x,{u0,u1},iRoot} -- at given horizontal/vertical coordinate (with x in general syntax for individual coordinate), seeking root using starting guesses {u0,u1} (default {0,1}) for the curve parameter (see help for FindRoot) and taking root indexed by iRoot (default -1, i.e., last root)*)


(*
FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Name:(Left|Center|Right),{CoordinateIndexName:(Horizontal|Vertical),x:FigCoordinatePattern}
]:=Module[
{fRc,xc,uRule,u,CoordinateIndex,CanvasPoint},
fRc=LinearInterpolationFunctionsRxRy[Points];  (* AD HOC -- perhaps to be revised if these become the stored interpolation functions *)
CoordinateIndex=FigResolveCoordinateIndex[CoordinateIndexName];
xc=FigResolveCoordinate[x,CoordinateIndex];

(* recover curve parameter *)
Quiet[
uRule=
FindRoot[fRc[[CoordinateIndex]][u]-xc,{u,0,1}];
u=Last@Last@uRule;
CanvasPoint=InterpolationFunction[u];
FigAnchor[
Canvas[CanvasPoint],
Switch[Name,Right,{0,+1},Center,{0,0},Left,{0,-1}],
VectorArcTan[TangentFunction[u]] 
],
{InterpolatingFunction::dmval}  (* allow anchors past end of line *)
]
];
 *)


(*
FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Name:(Left|Center|Right),{CoordinateIndexName:(Horizontal|Vertical),x:FigCoordinatePattern}
]:=FigCurveAnchor[Points,InterpolationFunctionPair,Name,{CoordinateIndexName,x,{0,1},-1}];
 *)


FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Name:(Left|Center|Right),{CoordinateIndexName:(Horizontal|Vertical),x:FigCoordinatePattern,RootFindingParameters:{_?NumericQ,_?NumericQ}:{0,1},iRoot:(_Integer):-1}
]:=Module[
  {fRc,xc,uRule,u,u0,u1,CoordinateIndex,CanvasPoint},
  fRc=LinearInterpolationFunctionsRxRy[Points];  (* AD HOC -- perhaps to be revised if these become the stored interpolation functions *)
  CoordinateIndex=FigResolveCoordinateIndex[CoordinateIndexName];
  xc=FigResolveCoordinate[x,CoordinateIndex];

  (* recover curve parameter *)
  Quiet[
    {u0,u1}=RootFindingParameters;
    uRule=
    FindRoot[fRc[[CoordinateIndex]][u]-xc,{u,u0,u1}];
    u=Last[uRule[[iRoot]]];

    CanvasPoint=InterpolationFunction[u];
    FigAnchor[
      Canvas[CanvasPoint],
      Switch[Name,Right,{0,+1},Center,{0,0},Left,{0,-1}],
      VectorArcTan[TangentFunction[u]] 
    ],
    {InterpolatingFunction::dmval}  (* allow anchors past end of line *)
  ]
   ];


 (*Curve anchors at ends of curve -- Tail/Head*)


 (*Argument: None*)


 FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Tail,None]:=FigAnchor[FigCurveAnchor[Points,InterpolationFunctionPair,Center,0],Right];
 FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Head,None]:=FigAnchor[FigCurveAnchor[Points,InterpolationFunctionPair,Center,1],Left];


 (*Curve anchor at enumerated point -- Point*)


 (*Argument: None*)


 FigCurveAnchor[Points_List,InterpolationFunctionPair:{InterpolationFunction_,TangentFunction_},Point,k_Integer]:=FigAnchor[Canvas[CurvePoint[Points,k]]];


 (*Bounding box (extremal values)*)


 FigCurveBoundingBox[CanvasCurve:{{_?NumericQ,_?NumericQ}...}]/;(Length[CanvasCurve]<1):=None;
 FigCurveBoundingBox[CanvasCurve:{{_?NumericQ,_?NumericQ}...}]/;(Length[CanvasCurve]>=1):={
   {Min[First/@CanvasCurve],Max[First/@CanvasCurve]},
   {Min[Last/@CanvasCurve],Max[Last/@CanvasCurve]}
  };


 (*Standard arrowhead options*)


 (*End-tagged versions are for bracket, with default equal head and tail parameters EndLength and EndLip*)


 FigArrowheadOptions[st:LogicalPattern,sh:LogicalPattern,len_:6,lip_:3]:={
   TailLength->len,TailLip->lip,ShowTail->st,
   HeadLength->len,HeadLip->lip,ShowHead->sh
  };
 FigArrowheadOptions[show:LogicalPattern,len_,lip_,End]:={
   TailLength->Automatic,TailLip->Automatic,ShowTail->Automatic,
   HeadLength->Automatic,HeadLip->Automatic,ShowHead->Automatic,
   EndLength->len,EndLip->lip,ShowEnd->show
  };


 FigCheckArrowheadOptions[Self_Object]:=Module[
   {},

   FigCheckOption[Self,TailLength,_?NumericQ,FigOptions];
   FigCheckOption[Self,HeadLength,_?NumericQ,FigOptions];

   FigCheckOption[Self,TailLip,NonNegativePattern|{NonNegativePattern,NonNegativePattern},FigOptions];
   FigCheckOption[Self,HeadLip,NonNegativePattern|{NonNegativePattern,NonNegativePattern},FigOptions];

   FigCheckOption[Self,ShowTail,LogicalPattern,FigOptions];
   FigCheckOption[Self,ShowHead,LogicalPattern,FigOptions];
                                        ];


 FigCheckArrowheadOptions[Self_Object,End]:=Module[
   {},

   FigCheckOption[Self,EndLength,_?NumericQ,FigOptions];
   FigCheckOption[Self,TailLength,Automatic|(_?NumericQ),FigOptions];
   FigCheckOption[Self,HeadLength,Automatic|(_?NumericQ),FigOptions];

   FigCheckOption[Self,EndLip,NonNegativePattern|{NonNegativePattern,NonNegativePattern},FigOptions];
   FigCheckOption[Self,TailLip,Automatic|NonNegativePattern|{NonNegativePattern,NonNegativePattern},FigOptions];
   FigCheckOption[Self,HeadLip,Automatic|NonNegativePattern|{NonNegativePattern,NonNegativePattern},FigOptions];

   FigCheckOption[Self,ShowEnd,LogicalPattern,FigOptions];
   FigCheckOption[Self,ShowTail,Automatic|LogicalPattern,FigOptions];
   FigCheckOption[Self,ShowHead,Automatic|LogicalPattern,FigOptions];

                                            ];


 (*Arrowhead generation*)


 (*BasicArrowheadPoints::usage="BasicArrowheadPoints[anchor,headlength,{headlipl,headlipr},sense] generates the points for an arrowhead with location and orientation given by anchor.";*)


 BasicArrowheadPoints[a:ObjectPattern[FigAnchor],HeadLength_?NumericQ,{HeadLipL_?NumericQ,HeadLipR_?NumericQ},Sense:(-1|1)]:=Module[
   {P,theta},

   P=(a@GetPoint[]);
   theta=(a@GetAngle[]);
   {
     P+RotationTransform[N@theta][{HeadLength,HeadLipL}*{-Sense,+1}*If[HeadLipL>0,1,0]],
     P,
     P+RotationTransform[N@theta][{HeadLength,HeadLipR}*{-Sense,-1}*If[HeadLipR>0,1,0]]
   }
                                                                                                                             ];


 FigCurveArrowheadPoints[TailAnchor:ObjectPattern[FigAnchor],HeadAnchor:ObjectPattern[FigAnchor],FullOptions_List]:=Module[
   {},

   {

     (* tail *)
     If[
       (ShowTail/.FullOptions),
       BasicArrowheadPoints[TailAnchor,(TailLength/.FullOptions),UpgradePair[(TailLip/.FullOptions)],-1],
       {}
     ],

     (* head *)
     If[
       (ShowHead/.FullOptions),
       BasicArrowheadPoints[HeadAnchor,(HeadLength/.FullOptions),UpgradePair[(HeadLip/.FullOptions)],+1],
       {}
     ]

   }

                                                                                                                    ];


 FigCurveArrowheadPoints[TailAnchor:ObjectPattern[FigAnchor],HeadAnchor:ObjectPattern[FigAnchor],ReversedSide:LogicalPattern,FullOptions_List,End]:=Module[
   {UsedTailLength,UsedHeadLength,UsedTailLip,UsedHeadLip,UsedShowTail,UsedShowHead},

   {

     (* tail *)
     UsedShowTail=ResolveOption[ShowTail,{Automatic:>(ShowEnd/.FullOptions)},FullOptions];
     If[
       UsedShowTail,
       UsedTailLength=ResolveOption[TailLength,{Automatic:>-(EndLength/.FullOptions)},FullOptions];
       UsedTailLip=If[ReversedSide,Reverse,Identity]@UpgradePair[ResolveOption[TailLip,{Automatic:>(EndLip/.FullOptions)},FullOptions]];
       BasicArrowheadPoints[TailAnchor,UsedTailLength,UsedTailLip,-1],
       {}
     ],

     (* head *)
     UsedShowHead=ResolveOption[ShowHead,{Automatic:>(ShowEnd/.FullOptions)},FullOptions];
     If[
       UsedShowHead,
       UsedHeadLength=ResolveOption[HeadLength,{Automatic:>-(EndLength/.FullOptions)},FullOptions];
       UsedHeadLip=If[ReversedSide,Reverse,Identity]@UpgradePair[ResolveOption[HeadLip,{Automatic:>(EndLip/.FullOptions)},FullOptions]];
       BasicArrowheadPoints[HeadAnchor,UsedHeadLength,UsedHeadLip,+1],
       {}
     ]

   }

                                                                                                                                                    ];


 (*Curve extraction from graphics*)


 GrabPoints::nocurve="No curves found.";
 GrabPoints::fewcurve="Requested Line->`1` is greater than the number `2` of curves found.";
 GrabPoints::shortcurve="Curve obtained `1` has fewer than two points and therefore may be unsuitable for use as an argument to functions or objects.";


 Point2D3DPattern={_?NumericQ,_?NumericQ}|{_?NumericQ,_?NumericQ,_?NumericQ};
 Options[GrabPoints]={Line->1};
 GrabPoints[G:(_Graphics|_Graphics3D),Opts___?OptionQ]:=Module[
   {FullOptions=Flatten[{Opts,Options[GrabPoints]}],LineSet,CurveSet,Curve},

   (*validate options*)
   FigCheckOption[GrabPoints,Line,Join|All|((_Integer)?Positive),FullOptions];

   (*grab all curves*)
   (* use Normal to resolve GraphicsComplex into Line (e.g., in output of ContourPlot) *)
   LineSet=Cases[Normal[G],Line[{Point2D3DPattern..}]|Line[{{Point2D3DPattern..}..}],Infinity];
   CurveSet=Replace[LineSet,{Line[c:{Point2D3DPattern..}]:>{c},Line[cc:{{Point2D3DPattern..}..}]:>cc},{1}];
   CurveSet=Flatten[CurveSet,1];
   If[Length[CurveSet]==0,FigMessage[GrabPoints,"nocurve"];
                          Return[{}]];

   (*process resulting curve set*)
   Switch[
     (Line/.FullOptions),
     (*case:grab specific curve*)
     _Integer,
     If[
       Length[CurveSet]<(Line/.FullOptions),
       FigMessage[GrabPoints,"fewcurve",(Line/.FullOptions),Length[CurveSet]];
       Return[{}]
     ];
     Curve=CurveSet[[(Line/.FullOptions)]];
     If[Length[Curve]<2,FigMessage[GrabPoints,"shortcurve",Curve]];
     Curve,
     (*case:merge all curves*)
     Join,
     Curve=Join@@CurveSet;
     If[Length[Curve]<2,FigMessage[GrabPoints,"shortcurve",Curve]];
     Curve,
     (*case:return curve list*)
     All,
     Do[
       If[Length[Curve]<2,FigMessage[GrabPoints,"shortcurve",Curve]],
       {Curve,CurveSet}
     ];
     CurveSet
   ]
                                                        ];
 GrabPoints[CG:(_ContourGraphics),Opts___?OptionQ]:=GrabPoints[Graphics[CG],Opts];


 (*List manipulation for curves*)


 SplitByDelimiter[l_List,d_Symbol]:=Module[
   {SplitList},
   SplitList=Split[l,!MemberQ[{#1,#2},d]&];
   Cases[SplitList,Except[{d}]]
                                    ];
 PurgeOfDelimiter[l_List,d_Symbol]:=Module[
   {},
   Cases[l,Except[d]]
                                    ];


 (*SplitByDelimiter[{1,2,Split,4,Split,Split},Split]
PurgeOfDelimiter[{1,2,Split,4,Split,Split},Split]*)


 AppendFirst[l_List]:=Append[l,First[l]];


 (*AppendFirst[{1,2,3}]*)


 (*End package*)


 (*Exit private context*)


 End[];


 (*Exit package context*)


 Protect[Evaluate[$Context<>"*"]];
 Unprotect[Evaluate[$Context<>"$*"]];
 EndPackage[];
