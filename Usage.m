(* ::Package:: *)

(*Header comments*)


(* :Title: Usage *)
(* :Context: SciDraw` *)
(* :Summary: Usage messages *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Copyright: Copyright FIGYEAR, Mark A. Caprio *)
(* :Package Version: FIGVERSION *)
(* :Mathematica Version: MATHVERSION *)
(* :Discussion: FIGDISCUSSION *)
(* :History: See main package file. *)


(*Begin package*)


(*Package context definition*)


BeginPackage["SciDraw`",SciDraw`Private`$ExternalContexts];


(*Reserved symbols*)


(*Some common definitions*)


SciDraw::usage="SciDraw";


Width::usage="Option name for use with figure objects.";


(*Mathematica version compatibility*)


(* note: this definition needs to be moved inside the "unprotect" region to avoid "Tag \[NoBreak]ProvideSymbol\[NoBreak] in \[NoBreak]ProvideSymbol[name_String,body_,else_:None]\[NoBreak] is Protected" error *)
(*SetAttributes[ProvideSymbol,HoldRest];
ProvideSymbol[name_String,body_,else_:None]:=
(* Add symbol to namespace and evaluate body (which would typically define messages and/or definitions for this symbol), only if symbol does not already exist as a *System* symbol.

This is designed to accommodate System name clashes introduced with new versions of Mathematica.  An else clause is provided.  Even if no body is provided, the symbol will be created as a symbol in the present context.
All occurrences of the symbol in the provided code should be as Symbol[name] to prevent the symbol from entering the namespace *before* the test for definition is carried out.

Arguments:
name (String): name of symbol (to be sought in System` or defined in $Context)
    body: code to execute for definition of symbol (if not found in System)
else: code to execute as fallback if found in System
 *)
Module[
{defined},
defined=NameQ[StringJoin["System`",name]];
If[
!defined,
Symbol[name];
body,
else
]
]; 
 *)


(*Core modules*)


(*FigAnchor*)


FigAnchor::usage="FigAnchor is an object type representing a point, default text offset, and default tangent angle within a figure.  The anchor may be created from several different sources of information: (1) From a coordinate point -- FigAnchor[{x,y}] with optional arguments FigAnchor[{x,y},{xo,yo},theta] returns an anchor with point {x,y}, text offset {xo,yo} (default {0,0}), and tangent angle theta (default 0). (2) From a scaled coordinate point -- FigAnchor[Scaled[{x,y}]] with optional arguments FigAnchor[Scaled[{x,y}],{xo,yo},theta].  (3) From a canvas coordinate point -- FigAnchor[Canvas[{x,y}]] with optional arguments FigAnchor[Canvas[{x,y}],{xo,yo},theta]. (4) As a copy from another anchor, optionally overriding the offset and angle values -- FigAnchor[anchorobject|anchorname] with optional arguments FigAnchor[anchorobject|anchorname,{xo,yo},theta] returns an anchor with canvas point taken from given anchor, text offset {xo,yo} (default from given anchor), and tangent angle theta (default from given anchor). (5) From figure object MakeAnchor method -- FigAnchor[objectname,args]. (DEPRECATED: Direct access to FigAnchor is deprecated in favor of the wrapper functions Anchor and GetAnchor, in anticipation to revisions to the SciDraw internal representation of anchors.)";
Absolute::usage="Absolute[{x,y}] represents a point in canvas coordinates (printer's points from lower left corner of main canvas region) as an argument to FigAnchor.";  (* DEBUGGING: Symbol Canvas is entirely blocked from namespace (not just shadowed!) by special symbol System`Canvas introduced in Mathematica 12.2 *)
ShowAnchor::usage="ShowAnchor[p] displays the canvas coordinates of a point and the associated text anchoring information (offset and tilt angle).";


(*FigArgument*)


ReplaceSequential::usage="ReplaceSequential[x,{rule1,rule2,...}] applies each rule once in succession as a replacement rule for x.";
ResolveOption::usage="ResolveOption[value,{rule1,rule2,...},options] returns value, subject to replacement by each of rule1, rule2, ..., in succession.";
NonNegativePattern::usage="Pattern matching nonnegative numbers, i.e., in [0,Infinity).";
PositivePattern::usage="Pattern matching positive numbers, i.e., in (0,Infinity).";
NonNegativeIntegerPattern::usage="Pattern matching nonnegative integers, i.e., in [0,Infinity).";
PositiveIntegerPattern::usage="Pattern matching positive integers, i.e., in [1,Infinity).";
UnitIntervalPattern::usage="Pattern matching numbers in the unit interval [0,1].";
LogicalPattern::usage="Pattern matching True or False.";
NonListPattern::usage="Pattern matching any expression except a list.  Equivalent to Except[_List].";
FlatListPattern::usage="Pattern matching any list of nonlists (of length 0 or more).";


SizePattern::usage="Pattern matching size directives.";
ColorDirectivePattern::usage="Pattern matching GrayLevel, RGBColor, CMYKColor, or Hue directives.";
FigThicknessPattern::usage="Pattern matching a valid thickness option for a figure object.";


FigResolveThickness::usage="FigResolveThickness[thickness] converts thickness to a standard thickness directive, if it is not one already.  In addition to standard thickness directives, thickness may be an acceptable argument to AbsoluteThickness, in which case it is taken as specifying the absolute thickness.";
FigDashingPattern::usage="Pattern matching a valid dashing option for a figure object.";
FigResolveDashing::usage="FigResolveDashing[dashing] converts dashing to a standard dashing directive, if it is not one already.  In addition to standard dashing directives, dashing may be None, or dashing may be an acceptable argument to AbsoluteDashing, in which case it is taken as specifying the absolute dashing.";
FigPointSizePattern::usage="Pattern matching a valid point size option for a figure object.";
FigResolvePointSize::usage="FigResolvePointSize[size] converts size to a standard dashing directive, if it is not one already.  In addition to standard point size directives, size may be an acceptable argument to AbsolutePointSize, in which case it is taken as specifying the absolute point sizeq.";
FontFamilyPattern::usage="Pattern matching font family option values.";
FontSizePattern::usage="Pattern matching font size option values.";
FontWeightPattern::usage="Pattern matching font weight option values.";
FontSlantPattern::usage="Pattern matching font slant option values.";
FontTrackingPattern::usage="Pattern matching font tracking option values.";


FigTextOffsetPattern::usage="Pattern matching a valid text offset specification for figure objects.";
FigTextOrientationPattern::usage="Pattern matching a valid text orientation specification for figure objects.";


(*FigElement*)


FigElement::usage="FigElement is the parent class for all figure element classes.";
FigLineElement::usage="FigLineElement is a figure element class, for Line graphics.";
FigPolygonElement::usage="FigFillElement is a figure element class, for polygons and other filled graphics objects (that is, those to which EdgeForm and FaceForm apply, such as Polygon, Rectangle, or Disk).";
FigPointElement::usage="FigPointElement is a figure element class, for Point graphics.";
FigVerbatimElement::usage="FigVerbatimElement is a figure element class, for graphics which requires no styling primatives.";
FigStyledText::usage="FigStyledText[text,fullopts] returns text in DisplayForm with the standard SciDraw text appearance options applied.  This styling does *not* provide a frame or background.";
FigTextElement::usage="FigTextElement is a figure element class, for text.";
FigTextFrameElement::usage="FigFrameElement is a figure element class, for text frames.";


CollectGraphicalElements::usage="CollectGraphicalElements[body,canvaswindow,background] compiles a list of graphical elements drawn using the coordinate system set by the given window.";
FigAssemblePrimatives::usage="FigAssemblePrimatives[elements] sorts the graphical element objects in elements according to layer and extracts to resulting primatives.";
FigWindowPrimatives::usage="FigWindowPrimatives[region,primatives] returns a list of primatives {Transparent,Rectangle[...],Inset[Graphics[...]]} which effectively clips the given primatives to the given canvas region.";
FigWindowPrimativesRaster::usage="FigWindowPrimatives[region,resolution,primatives] returns a list of primatives {Transparent,Rectangle[...],Inset[Graphics[...]]} which effectively clips the given primatives to the given canvas region then rasterizes them at the given resolution in dpi.";
FigCompositeElement::usage="FigCompositeElement is a figure element class, for assembling simpler elements into a flattened, possibly clipped, possibly rasterized rectangular window.";


FigureGroup::usage="FigureGroup[body] assembles figure objects into a flattened, possibly clipped, possibly rasterized rectangular window.";


(*FigError*)


FigMessage::usage="FigMessage[object,message,args] or FigMessage[symbol,message,args] displays the given message (with given arguments)."; 
FigError::usage="FigError[object,message,args] or FigError[symbol,message,args]  displays the given message (with given arguments) and throws an Abort signal."; 
FigWarnObject::usage="FigWarnObject[object,action] displays a notification message indicating which object is being constructed (or whatever action is named by the string action).";
FigCheckInFigure::usage="FigCheckInFigure[object] generates an error if the check finds itself outside of a Figure environment.";
FigCheckObjectName::usage="FigCheckObjectName[object] emits a warning message if the object name does not conform to the recommended conventions.";
FigFallThroughError::usage="FigFallThroughError[function,expr] warns of invalid arguments and throws an Abort signal.  The special case of Null arguments is flagged."; 
DeclareFigFallThroughError::usage="DeclareFigFallThroughError[function] defines a syntax fall-through case of function, which invokes FigFallThroughError, to warn of invalid arguments and throws an Abort signal.  The special case of Null arguments is flagged."; 



FigCheckOption::usage="FigCheckOption[object|function,optionname,pattern,optionlist] validates the given option optionname against pattern, after determining its value from optionlist.  If the validation fails, messages are emitted identifying the object|function and throwing an error.";
FigCheckValue::usage="FigCheckValue[object|function,value,pattern,description] validates the given value against pattern.  If the validation fails, messages are emitted identifying the object|function and throwing an error.";
FigCheckValueList::usage="FigCheckValueList[object|function,valuelist,pattern,description] validates the given values against pattern.  If the validation fails, messages are emitted identifying the object|function and throwing an error.";


SuppressMessage::usage="SuppressMessage[message,body] evaluates body with message suppressed.";


(*FigFigure*)


Figure::usage="Figure[body] returns a drawing constructed from the objects created in the code given in body.";
CanvasSize::usage="Option name for use with Figure.  CanvasSize->{xsize,ysize} sets the canvas size (before padding) to {xsize,ysize}, in units determined by the CanvasUnits option.";
CanvasMargin::usage="Option name for use with Figure.  The most complete specification is of the form CanvasMargin->{{xL,xR},{yL,yR}}.  The values *must* be given in the same units as the CanvasSize.  (For instance, in contrast to the Mathematica PlotRangePadding option, Scaled values cannot be used.)";
CanvasUnits::usage="Option name for use with Figure.  CanvasUnits->unit indicates the unit in terms of which CanvasSize and CanvasMargin are to be interpreted.  Must be a valid length unit (default Inches) from the Mathematica Units package.";
CanvasFrame::usage="Option name for use with Figure.";
ExportDirectory::usage="Option name for use with Figure.";
ExportFileName::usage="Option name for use with Figure.";
ExportFormat::usage="Option name for use with Figure.";
ExportOptions::usage="Option name for use with Figure.";
ExportStamp::usage="Option name for use with Figure.";


CurrentWindow::usage="CurrentWindow[] returns the current window object.";
CurrentBackground::usage="CurrentBackground[] returns the current window object.";


(*FigGeometry*)


(*Scalar/pair/range arithmetic*)


ScalarParameterPattern::usage="Pattern matching expression for scalar parameter.";
NonNegativeScalarParameterPattern::usage="Pattern matching expression for scalar parameter, with nonnegative value.";


UpgradeScalar::usage="Upgrade[x] yields x.  The argument None yields 0.  The data must be numerical.";


RescaleInterval::usage="RescaleInterval[{u1,u2},{v1,v2}] defines the linear transformation which maps the interval [u1,u2] to [v1,v2].  RescaleInterval[{u1,u2}] maps [u1,u2] onto the unit interval [0,1].  The functionality is similar to RescalingTransform, restricted to one dimension, but does not require the extra wrapping to turn the coordinates into one-dimensional vectors.";
InRange::usage="For a single interval, InRange[{x1,x2},x] tests whether or not x is in the closed interval {x1,x2}.  For a two-dimensional region, InRange[{{x1,x2},{y1,y2}},{x,y}] tests whether or not {x,y} is in the closed region {{x1,x2},{y1,y2}}.";
ExtendInterval::usage="ExtendInterval[{x1,x2},{fx1,fx2},mode] extends an interval by the specified amounts in each direction.  If mode is Absolute, these are additive amounts, or, if mode is Scaled, these are fractional amounts.  (WARNING: \"Absolute\" is a SciDraw Private symbol.  \"Abs\" is accepted as alternative.)";
ExtendRegion::usage="ExtendRegion[{{x1,x2},{y1,y2}},{{fx1,fx2},{fy1,fy2}},mode] extends a rectangular region by the specified amounts in each direction.  If mode is Absolute, these are additive amounts, or, if mode is Scaled, these are fractional amounts.  (WARNING: \"Absolute\" is a SciDraw Private symbol. \"Abs\" is accepted as alternative.)"; 
VectorLength::usage="VectorLength[{x,y}] returns the length of {x,y}, i.e., Sqrt[x^2+y^2].  It is therefore equivalent to Norm[{x,y}] but provides stronger argument type checking.";
VectorArcTan::usage="VectorArcTan[{x,y}] returns the polar angle of the vector {x,y}, i.e., ArcTan[x,y], or, arbitrarily, the horizontal angle 0. if {x,y} is the null vector.";
SegmentLength::usage="SegmentLength[{p1,p2}] returns the length of the segment {p1,p2}, i.e., of the displacement vector p2-p1.";
SegmentTangent::usage="SegmentTangent[{p1,p2}] returns the unit vector along the segment {p1,p2}.";
SegmentArcTan::usage="SegmentArcTan[{p1,p2}] returns the polar angle of the segment {p1,p2}, i.e., of the displacement vector p2-p1, or, arbitrarily, the horizontal angle 0. if {p1,p2} is a null segment.";
FromPolar::usage="FromPolar[{r,phi}] returns the Cartesian form of these polar coordinates.";
InterpolateSegment::usage="InterpolateSegment[{p1,p2},reference,mode,x] maps the parameter x onto the line defined by the points p1 and p2,starting from the given reference point (Tail,Center,or Head) either as an absolute length displacement (Absolute) or scaled to the length of the segment (Scaled) as specified by mode.The mappings for the scaled case are: Tail [0,1]\[Rule][p1,p2],Center [-1,+1]\[Rule][p1,p2], Head [-1,0]\[Rule][p1,p2].";


(*Single-coordinate geometry*)


AntiCoordinateIndex::usage="AntiCoordinateIndex[index] returns the index for the \"other\" coordinate, i.e., it maps 1->2 and 2->1.";
FigCoordinatePattern::usage="Pattern matching expression for a single-coordinate specifier.  This may be of the form x, Scaled[x], Absolute[x], or a general figure point specification.";
FigResolveCoordinate::usage="FigResolveCoordinate[x,direction] resolves a coordinate specification x to canvas coordinates.  The direction may be Horizontal or Vertical.";
FigResolveCoordinateDisplacement::usage="FigResolveCoordinateDisplacement[x,direction] resolves a coordinate displacement specification x to canvas coordinates.  The direction may be Horizontal or Vertical.";


(*XY pair parameters*)


NumericalPairPattern::usage="Pattern matching pattern for an explicit numerical pair.";
IntervalParametersPattern::usage="Pattern matching expression for interval parameters.";
NonNegativeIntervalParametersPattern::usage="Pattern matching expression for interval parameters, with all nonnegative values..";


UpgradePair::usage="UpgradePair[x] or UpgradePair[{x,y}] yields a pair of the form {x,y}.  The data must be nonlist.";
UpgradePairEqual::usage="UpgradePairEqual[x] or UpgradePairEqual[{x,y}] yields a pair of the form {x,y}, with default {x,x}.  The argument None yields {0,0}.  The data must be numerical.";UpgradePairHorizontal::usage="UpgradePair[x] or UpgradePair[{x,y}] yields a pair of the form {x,y}, with default {x,0}.  The argument None yields {0,0}.  The data must be numerical.";
UpgradePairVertical::usage="UpgradePair[x] or UpgradePair[{x,y}] yields a pair of the form {x,y}, with default {0,y}.  The argument None yields {0,0}.  The data must be numerical.";


(*Regions*)


RangeParametersPattern::usage="Pattern matching expression for range parameters.";
NonNegativeRangeParametersPattern::usage="Pattern matching expression for range parameters, with all nonnegative values.";
NumericalRegionPattern::usage="Pattern matching expression for a numerical region {{x1,x2},{y1,y2}}.";


FigRegionPattern::usage="Pattern matching a valid region specification for figure objects.";
FigResolveRegion::usage="FigResolveRegion[region] converts a general figure region specification to a canvas region {{x1,x2},{y1,y2}}.";
FigDeltaRegionPattern::usage="Pattern matching a valid region *extension* specification for figure objects.";
FigResolveRegionExtension::usage="FigResolveRegionExtension[{{x1,x2},{y1,y2}},delta] takes a canvas region and a general figure region *extension* specification, and resolves this to arguments {{{x1,x2},{y1,y2}},{dx1,dx2},{dy1,dy2},Absolute|Scaled} for use with ExtendRegion.  Scaled[] in the extension refers to a fraction of the given region not the current window region.";
AdjustRegion::usage="AdjustRegion[region,RegionExtension->delta] takes a general figure region specification, expands it by a general figure region *extension* specification, and returns an appropriate figure region specification.  A Scaled[] value for delta refers to a fraction of the given region not the current window region.  AdjustRegion[region,RegionDisplacment->delta] takes a general figure region specification, translates it by a general figure displacement, and returns an appropriate figure region specification.";
RegionExtension::usage="Option name for use with AdjustRegion.";
RegionDisplacement::usage="Option name for use with AdjustRegion.";
BoundingBox::usage="BoundingBox[{p1,p2,...,obj1,obj2,...}] returns a region specification Absolute[{{xmin,xmax},{ymin,ymax}}] for the bounding box surrounding the given objects or points/anchors.";  
RegionPoint::usage="RegionPoint[region,{xo,yo}] returns a point specification Absolute[{x,y}] determined by its \"offset\" coordinates with respect to the region, e.g., {-1,-1} for the lower left corner.";


UpgradeRangeParameters::usage="UpgradeRangeParameters[x], UpgradeRangeParameters[{x,y}], or UpgradeRangeParameters[{{x1,x2},{y1,y2}}] yields a range specification of the form {{x1,x2},{y1,y2}}.  The data must be numeric.  The argument None yields {{0,0},{0,0}}.";


(*Edge parameters*)


EdgeXYSameParametersPattern::usage="EdgeXYStyleParametersPattern[patt] is a pattern matching expression for edge parameters for which a single value can be specified for both the X and Y values.";EdgeXYUniqueParametersPattern::usage="EdgeXYUniqueParametersPattern[patt] is a pattern matching expression for edge parameters for which the X and Y values would always be defined separately (uniquely) except the value None.";


UpgradeEdgeNonList::usage="UpgradeEdgeNonList[b,filler,mirror],  UpgradeEdgeNonList[{b,l},filler,mirror], or UpgradeEdgeNonList[{{l,r},{b,t}},filler,mirror] yields an axis parameter specification of the form {{l,r},{b,t}}, with unspecified entries given by filler.  UpgradeEdgeNonList[None,filler,mirror] yields all filler.  The legacy argument ordering {b,l,t,r} is also supported.  When the boolean argument mirror is true, if the r and t values are unspecified, they are set to the l and b values, respectively.  When the boolean argument mirror is false, if the r and t values are unspecified, they are set to filler.  The parameters must be nonlist expressions.";
UpgradeEdgeFlatList::usage="Same as UpgradeEdgeNonList, but for parameters which are instead flat lists.";
MaskEdgeOption::usage="MaskEdgeOption[{{L,R},{B,T}},mask:{{mL,mR},{mB,mT}},exterioredgemask:{{extL,extR},{extB,extT}},X] replaces the entries in {{L,R},{B,T}} with filler X if the entry is masked off.  An entry is \"on\" if masked as True or is masked as Exterior on an exterior edge.  An entry is \"off\" is masked as False entry or is masked as Exterior on a non-exterior edge.";
ResolveAutomaticEdgeOption::usage="ResolveAutomaticEdgeOption[{{L,R},{B,T}},defaults:{{dL,dR},{dB,dT}}] replaces the entries in {{L,R},{B,T}} with the default value from the corresponding entry of {{dL,dR},{dB,dT}} if the entry value is Automatic.";
Exterior::usage="Option value which evaluates to True for exterior edges of a figure panel in a multipanel array.";


(*Point/anchor*)


FigCoordinatePointPattern::usage="Pattern matching a valid point specification for figure objects, restricted to numerical coordinates rather than anchors.";
FigPointPattern::usage="Pattern matching a valid point specification for figure objects.";
FigDisplacementPattern::usage="Pattern matching a valid point specification for figure objects.";
FigDisplacementSequencePattern::usage="FigDisplacementSequencePattern[n] yields a pattern matching a valid *sequence* of displacement specifications for figure objects, consisting of a list of n or more displacements.";
FigDisplacementSetPattern::usage="FigDisplacementSetPattern[n] yields a pattern matching a valid set of displacement specifications for figure objects, consisting of a list of n or more displacements.";


FigResolvePoint::usage="FigResolvePoint[point] converts a general figure point/anchor specification to a canvas point.";
FigResolveAnchor::usage="FigResolveAnchor[point] converts a general figure point/anchor specification to an anchor object reference.";
FigPointAnchor::usage="FigPointAnchor[{{x1,y1},{x2,y2},...},arguments] returns an anchor  various possible anchors apropriate to point data.";FigPointBoundingBox::usage="FigPointBoundingBox[{x,y}] returns {{x,x},{y,y}}.";
FigPointSetBoundingBox::usage="FigPointSetBoundingBox[{{x1,y1},{x2,y2},...}] returns the bounding region {{xmin,xmax},{ymin,ymax}}.  (Note: This function is meant primarily for internal calculations in canvas coordinates.  ObjectBox should be used instead for user-level calculations in a figure.)";
FigResolveDisplacement::usage="FigResolveDisplacment[displacement] converts a general figure displacement specification to a canvas displacement.";


DisplacePoint::usage="DisplacePoint[p,d1,d2,...] returns a point (actually, anchor) obtained by starting from the point (or anchor) p and displacing it by the displacements d1, d2, ...  If p is an anchor, the offset and orientation information is retained.";
ProjectPoint::usage="ProjectPoint[p,Horizontal,x]  or ProjectPoint[p,Vertical,y] returns a point (actually, anchor) obtained by starting from the point (or anchor) p and displacing it in the Horizontal or Vertical direction (as given by direction) to coordinate value x.";DisplaceAlongAnchor::usage="DisplaceAlongAnchor[anchor,dist] returns an anchor obtained by moving, from the anchor, a distance dist in printer's points along the anchor's orientation.";
Anchor::usage="Anchor returns an anchor, that is, representing a point, default text offset, and default tangent angle within a figure.  The anchor may be created from several different sources of information: (1) From a coordinate point -- FigAnchor[{x,y}] with optional arguments FigAnchor[{x,y},{xo,yo},theta] returns an anchor with point {x,y}, text offset {xo,yo} (default {0,0}), and tangent angle theta (default 0). (2) From a scaled coordinate point -- FigAnchor[Scaled[{x,y}]] with optional arguments FigAnchor[Scaled[{x,y}],{xo,yo},theta].  (3) From a canvas coordinate point -- FigAnchor[Absolute[{x,y}]] with optional arguments FigAnchor[Absolute[{x,y}],{xo,yo},theta]. (4) As a copy from another anchor, optionally overriding the offset and angle values: FigAnchor[anchorobject|anchorname] with optional arguments FigAnchor[anchorobject|anchorname,{xo,yo},theta] returns an anchor with canvas point taken from given anchor, text offset {xo,yo} (default from given anchor), and tangent angle theta (default from given anchor).";
GetAnchor::usage="GetAnchor[objectname,args] generates an anchor from an existing object.";
RotateAnchor::usage="RotateAnchor[p,angle] returns an anchor obtained by starting from the point specification p, resolving it to an anchor, and rotating the anchor orientation by angle.  The angle may be a number or None.";
AnchorAngle::usage="AnchorAngle[p] returns the orientation angle of anchor p.";
AnchorCoordinates::usage="AnchorCoordinates[p] returns the coordinates of anchor p, as current user coordinates.";
AnchorOffset::usage="AnchorOffset[p] returns the offset part of anchor p.  Also, option name for use with figure objects.  Specifies the anchor point for positioning the object in fractional coordinates relative to the center of the object.";
CanvasRayAngle::usage="CanvasRayAngle[{p1,p2}] returns an angle for the ray from anchor p1 to anchor p2, as measured on the canvas.  The cut used is (-Pi,Pi], as for ArcTan[x,y].";


(*Offset*)


FigOffsetPattern::usage="Pattern matching an offset given as {xo,yo} or a point name (Center, Left, Right, Bottom, Top, TopLeft, TopRight, BottomLeft, BottomRight).";
FigResolveOffset::usage="FigResolveOffset[offset] converts a general figure offset specification to a numerical offset.";


NamedPointPattern::usage="NamedPointPattern matches Center, Left, Right, Bottom, Top, TopLeft, TopRight, BottomLeft, and BottomRight.";
TopLeft::usage="Corner name.";
TopRight::usage="Corner name.";
BottomLeft::usage="Corner name.";
BottomRight::usage="Corner name.";
NamedPointOffset::usage="NamedPointOffset[side] provides a shorthand for the offset coordinates to the given point -- Left/Right/Bottom/Top/Center.";


(*Point sets*)


CentroidPoint::usage="CentroidPoint[{p1,p2,...}] returns a point obtained as the centroid of the given points p1, p2, ...";


(*Circle/rectangle*)


PivotOffset::uage="Option name for use with figure objects.  Specifies the pivot point for object rotations in fractional coordinates relative to the center of the object.";
Radius::uage="Option name for use with figure objects.  Specifies radius or minor and major radii.";
FigRectangleOptions::usage="FigRectangleOptions gives the standard set of additional options for figure objects accepting parameters like those of a circle.";
FigCheckRectangleOptions::usage="FigCheckRectangleOptions[obj] validates the corresponding set of options.";
FigRadiusPattern::usage="Pattern matching a valid radius specification for figure objects.";
FigResolveRadius::usage="FigResolveRadius[r] converts a general figure radius specification to a canvas radius {rx,ry}.";
MakeRectangleGeometry::usage="MakeRectangleGeometry[p,optionlist] returns the resolved values of {CanvasCenter,CanvasRadius,CanvasPivot,RotationAngle} as a list given the anchor and options for a rectangle-like object.";
RectangleRegion::usage="RectangleRegion[center] or RectangleRegion[corner1,corner2] or RectangleRegion[region] generates a region specification from the arguments which would normally be used to generate a FigRectangle.";


FigCircleAnchor::usage="FigCircleAnchor[p,{rx,ry},{theta1,theta2},phi,arguments] returns various possible anchors apropriate to a circle (rotated ellipse).";
FigCircleBoundingBox::usage="FigCircleBoundingBox[p,{rx,ry},{theta1,theta2},phi] returns the bounding box {{xmin,xmax},{ymin,ymax}} -- presently defined as the bounding box of the principal axis end points (after rotation), which is not the true bounding box for a tilted ellipse nor for a subarc.";


HeadRadius::usage="Symbol representing the second bounding radius of a sector.";
TailRadius::usage="Symbol representing the first bounding radius of a sector.";


FigRectangleAnchor::usage="FigRectangleAnchor[p,{rx,ry},phi,arguments] returns various possible anchors apropriate to a rectangle (or rotated rectangle).";
FigRectangleBoundingBox::usage="FigRectangleBoundingBox[p,{rx,ry},phi] returns the bounding box {{xmin,xmax},{ymin,ymax}} of a rectangle (or rotated rectangle).";


(*Curve/arrowhead*)


Tail::usage="Tail is a symbol which represents the tail of a figure object, for purposes of positioning.";
Tangent::usage="Tangent is a symbol which represents a tangent anchor point on a figure object, for purposes of positioning.";
TailLength::usage="Option name for use with figure objects.  Gives arrow tail length in printer's points.";
TailLip::usage="Option name for use with figure objects.  Gives arrow tail lip in printer's points.  Separate lengths for the left and right sides may be given as {lipl,lipr}.";
ShowTail::usage="Option name for use with figure objects.  Specifies whether or not arrow tail should be shown.";
HeadLength::usage="Option name for use with figure objects.  Gives arrow head length in printer's points.";
HeadLip::usage="Option name for use with figure objects.  Gives arrow head lip in printer's points.  Separate lengths for the left and right sides may be given as {lipl,lipr}.";
ShowHead::usage="Option name for use with figure objects.  Specifies whether or not arrow head should be shown.";
EndLength::usage="Option name for use with figure objects.  Gives head and tail length in printer's points, but in outward sense (i.e., negated relative to usual arrow head direction).";
EndLip::usage="Option name for use with figure objects.  Gives head and tail lip in printer's points.";
ShowEnd::usage="Option name for use with figure objects.  Specifies whether or not arrow head and tail should be shown.";
TailRecess::usage="Option name for use with figure objects.  Specifies the distance a figure object curve's tail should be recessed from the given endpoint.";
HeadRecess::usage="Option name for use with figure objects.  Specifies the distance a figure object curve's head should be recessed from the given endpoint.";
DisplaceTail::usage="DisplaceTail[d1,d2,...] is a symbol which represents a point on a curve, to be obtained as a displacement from the tail point of the figure object.";
DisplaceHead::usage="DisplaceHead[d1,d2,...] is a symbol which represents a point on a curve, to be obtained as a displacement from the head point of the figure object.";
ProjectTail::usage="ProjectTail[direction,x] is a symbol which represents a point on a curve, to be obtained as a projection of the tail point of the figure object.";
ProjectHead::usage="ProjectHead[direction,x] is a symbol which represents a point on a curve, to be obtained as a projection of the head point of the figure object.";
DisplaceAlongTail::usage="DisplaceAlongTail[dist] is a symbol which represents a point on a curve, to be obtained as a displacement from the tail point of the figure object.";
DisplaceAlongHead::usage="DisplaceAlongHead[dist] is a symbol which represents a point on a curve, to be obtained as a displacement afrom the head point of the figure object.";



LinearInterpolationFunctionsRxRy::usage="LinearInterpolationFunctionsRxRy[pts] returns the linear interpolation functions {fRx,fRy} for the given points, which map u->Rx or u->Ry, respectively, with curve parameter u in [0,1].";
LinearInterpolationFunctionPair::usage="LinearInterpolationFunctionPair[pts] returns the linear interpolation and tangent functions for the given points, which map u->{x,y} with curve parameter u in [0,1].  (DEPRECATED)";
LinearTangentFunction::usage="LinearTangentFunction[pts] returns the derivative of the linear spline function for the given points, which maps u->{x',y'} with curve parameter u in [0,1].";
FigCurveAnchor::usage="FigCurveAnchor[pts,interpolationfcn,tangentfcn,arguments] returns various possible anchors apropriate to curve data.";
CurveSegment::usage="CurveSegment[pts,k] returns the endpoints of the kth segment of the curve given by pts.  For a list of n points, k may be 1, 2, ..., n-1 counting from the beginning or -1, -2, ..., -(n-1) counting back from the end.";
CurvePoint::usage="CurvePoint[pts,k] returns the kth point of the curve given by pts.  For a list of n points, k may be 1, 2, ..., n counting from the beginning or -1, -2, ..., -n counting back from the end.";
FigCurveAnchorFromPoints::usage="FigCurveAnchor[pts,arguments] returns various possible anchors apropriate to curve data, simply using linear interpolation of the points.";FigCurveBoundingBox::usage="FigCurveBoundingBox[{{x1,y1},{x2,y2},...}] returns the bounding box {{xmin,xmax},{ymin,ymax}}.  For a null list of points, CurveBoundingBox returns None.";


FigPointSetPattern::usage="FigPointSetPattern[n] yields a pattern matching a valid point set specification for figure objects, consisting of a list of n or more points.";
FigCurvePointPattern::usage="Pattern matching a valid curve point specification for figure objects, consisting of an ordinary point specification or a FromTail/FromHead specification.";
FigCurvePointSetPattern::usage="Pattern matching a valid curve specification for figure objects, consisting of a list of two or more points, but not matching Graphics/ContourGraphics.";
FigCurvePattern::usage="Pattern matching a valid curve specification for figure objects, consisting of a list of two or more points or Graphics/ContourGraphics.";
FigResolveCurve::usage="FigResolveCurve[self,{p1,p2,...},options] converts a general figure curve specification to a canvas point list.";


FigArrowheadOptions::usage="FigArrowheadOptions[showtail,showhead] gives the standard set of arrowhead options for use in defining figure objects.";
FigCheckArrowheadOptions::usage="FigCheckArrowheadOptions[obj] validates the corresponding set of options.";
FigCurveOptions::usage="FigCurveOptions gives the standard set of additional options for figure objects accepting curve arguments.";
FigCheckCurveOptions::usage="FigCheckCurveOptions[obj] validates the corresponding set of options.";
FigCurveArrowheadPoints::usage="FigCurveArrowheadPoints[tailanchor,headanchor,options] generates the arrowheads to go on the curve specified by canvaspoints, the arrowheads themselves then returned as lists of canvas points {taillist,headlist}.";


GrabPoints::usage="GrabPoints[graphics] returns a curve (point list) extracted from the Line objects in a Graphics, ContourGraphics, or Graphics3D object.  With option Line->n, the points from the nth curve found in graphics are returned (by default, Line->1).  With Line->Join, all curves are joined end to end.  With Line->All, a list of all curves is returned.";


(*List manipulation for curves*)


SplitByDelimiter::usage="SplitBySplit[list,delimiter] splits list into sublists, delimited by delimiter.";
PurgeOfDelimiter::usage="PurgeOfDelimiter[list,delimiter] purges list of delimiter.";
AppendFirst::usage="AppendFirst[list] appends the first element of list at the end, e.g., to close an open curve."; 


(*FigObject*)


(*FigObject machinery*)


$FigClassRegistry::usage="$FigClassRegistry is a list of all registered figure object classes.";
$FigClassAttachedLabels::usage="$FigClassAttachedLabels[class] contains a list of all registered predefined labels for class.";
RegisterFigOptions::usage="RegisterFigOptions[symbol] registers the options for symbol to be scoped along with those of figure objects.";
DeclareFigClass::usage="DeclareFigClass[class,[parent],{data1,data2,...},{method1,method2,...},{label1,label2,...}] declares a figure object class, inheriting from FigObject (or an intermediate parent class parent descended from FigObject), and with given data members.  This function also carries out other necessary registration and setup tasks for a figure object.";
DefineFigClassOptions::usage="DefineFigClassOptions[class,[parent],{option1->rule1,...}] defines the options and option inheritance rules for a figure object class.  The FigObject options are automatically inherited (or those of an intermediate parent class parent descended from FigObject), and options are defined for each of the convenience labels declared for this class in DeclareFigClass.  Defaults for inherited options may be overridden from Inherited by DefineFigClassOptions[class,{parentoption1->newdefault1,...},{option1->rule1,...}].";
FigCheckBaseOptions::usage="FigCheckBaseOptions[self,optionlist] carries out option validation on the base FigObject options.";
FigRealizeOptions::usage="FigRealizeOptions[self,class,optionlist] carries out option inheritance and realization for figure objects.";
FigObject::usage="FigObject is the parent class for all figure object classes.";
FigObjectWrapper::usage="FigObjectWrapper[class,self,optionlist,body] provides the standardized right hand side for a function defining a figure object.";
FigMakeAnchorWrapper::usage="FigMakeAnchorWrapper[class,self,name,arg,expr] provides a standardized right hand side with error trapping for a function defining a figure object MakeAnchor method.";
FigObjectAnchorWrapper::usage="FigObjectAnchorWrapper[class,self,name,optionlist,body] provides the standardized right hand side for a function defining a figure object anchor method. (UNDER DEVELOPMENT -- stagnated)";
FigMakeBoundingBoxWrapper::usage="FigMakeBoundingBoxWrapper[class,self,expr] provides a standardized right hand side with error trapping for a function defining a figure object MakeBoundingBox method.";
FigOptions::usage="FigOptions evaluates to the list of option rules resolved by FigObjectWrapper.  FigOptions is only for use inside FigObjectWrapper.";
FigAnchorOptions::usage="FigAnchorOptions evaluates to the list of option rules resolved by FigObjectAnchorWrapper.  FigAnchorOptions is only for use inside FigObjectWrapper.";
ScopeOptions::usage="ScopeOptions[body] evaluates body, localizing all changes to the default options for figure objects.";
SetOptionOverrides::usage="SetOptionOverrides[namepatt->options] or SetOptionOverrides[{namepatt1->options1,...}] adds the rule that options should be applied to all figure objects with names matching namepatt, within the current option override scope.  Rules are appended, i.e., at lower precedence, to prior rules.";
ScopeOptionOverrides::usage="ScopeOptionOverrides[body] evaluates body, localizing all changes to the option override rules for figure objects.";
ResolveOptionOverrides::usage="ResolveOptionOverrides[name] resolves all option overrides presently defined for a figure object with the given name.";


(*FigObject standard options*)


Color::usage="Option name for use with figure objects.";
Directives::usage="Option name for use with figure objects.";
ShowLine::usage="Option name for use with figure objects.";
LineColor::usage="Option name for use with figure objects.";
LineOpacity::usage="Option name for use with figure objects.";
LineThickness::usage="Option name for use with figure objects.";
LineDashing::usage="Option name for use with figure objects.";
LineCapForm::usage="Option name for use with figure objects.";
LineJoinForm::usage="Option name for use with figure objects.";
LineDirectives::usage="Option name for use with figure objects.";
ShowFill::usage="Option name for use with figure objects.";
FillColor::usage="Option name for use with figure objects.";
FillOpacity::usage="Option name for use with figure objects.";
FillDirectives::usage="Option name for use with figure objects.";
ShowPoint::usage="Option name for use with figure objects.";
PointColor::usage="Option name for use with figure objects.";
PointOpacity::usage="Option name for use with figure objects.";
PointDirectives::usage="Option name for use with figure objects.";
ShowText::usage="Option name for use with figure objects.";
TextColor::usage="Option name for use with figure objects.";
TextOpacity::usage="Option name for use with figure objects.";
TextBackground::usage="Option name for use with figure objects.";
TextStyleOptions::usage="Option name for use with figure objects.";
TextFrame::usage="Option name for use with figure objects.";
TextFrameOpacity::usage="Option name for use with figure objects.";
TextFrameColor::usage="Option name for use with figure objects.";
TextFrameThickness::usage="Option name for use with figure objects.";
TextFrameDashing::usage="Option name for use with figure objects.";
TextFrameDirectives::usage="Option name for use with figure objects.";
TextRoundingRadius::usage="Option name for use with figure objects.";
TextMargin::usage="Option name for use with figure objects.";
TextPadding::usage="Option name for use with figure objects.";
TextOffset::usage="Option name for use with figure objects.";
TextOrientation::usage="Option name for use with figure objects.";
TextRectify::usage="Option name for use with figure objects.";
TextBaseBuffer::usage="Option name for use with figure objects.";
TextBuffer::usage="Option name for use with figure objects.";
TextNudge::usage="Option name for use with figure objects.";
TextCallout::usage="Option name for use with figure objects.";
TextCalloutColor::usage="Option name for use with figure objects.";
TextCalloutOpacity::usage="Option name for use with figure objects.";
TextCalloutThickness::usage="Option name for use with figure objects.";
TextCalloutDashing::usage="Option name for use with figure objects.";
TextCalloutCapForm::usage="Option name for use with figure objects.";
TextCalloutJoinForm::usage="Option name for use with figure objects.";
TextCalloutDirectives::usage="Option name for use with figure objects.";
Layer::usage="Option name for use with figure objects.";


(*Attached labels*)


BaseOutlineOptionList::usage="List of {option,pattern} pairs for the base FigObject outline options.";
BaseFillOptionList::usage="List of {option,pattern} pairs for the base FigObject fill options.";
BasePointOptionList::usage="List of {option,pattern} pairs for the base FigObject point options.";
BaseTextOptionList::usage="List of {option,pattern} pairs for the base FigObject text options.";
BaseAnchorOptionList::usage="List of {option,pattern} pairs for the base FigObject anchor options.";
FigDerivedLabelOptions::usage="FigDerivedLabelOptions[{side1,side2,...},extras,fontsize] gives the standard set of additional options for a figure object convenience label at the given side or location, with the given default for FontSize (usually Default, sometimes Automatic, e.g., for tick labels).  These are sideLabel, sidePosition, plus the various text appearance and positioning options of the form sideTextXXXX and sideFontXXXX.";
FigCheckDerivedLabelOptions::usage="FigCheckDerivedLabelOptions[self,side,extras] validates the corresponding set of options.";
FigResolveDerivedLabelOptions::usage="FigResolveAttachedLabelOptions[side] resolves the options determining the content, anchor, and style/positioning options for a convenience label, as a list {label,anchorargs,{textoption1->value1,...}}.";
SidifyOptionName::usage="SidifyOptionName[Side][Option] returns the symbol SciDraw`SideOption.";
FigSpawnAttachedLabel::usage="FigSpawnAttachedLabel[self,side,{content,positionargs,newoptions},fulloptions] spawns a given convenience label.";
LabelPosition::usage="SciDraw reserved symbol.";
(*Orientation::usage="SciDraw reserved symbol (base for anchor option names).";*)
(*Buffer::usage="SciDraw reserved symbol (base for anchor option names).";*)
(*Nudge::usage="SciDraw reserved symbol (base for anchor option names).";*)


(*FigStyle*)


(*
FigStyle::usage="FigStyle is the figure object style class.  FigStyle[[name]][{symbol1,options1},{symbol2,options2},...,commonopts] defines options for the given symbols (e.g., figure object classes or Figure itself).  First the default options are temporarily set to the values given -- by evaluating SetOptions[symbol,options] (evaluation runs from left to right in the argument list).  Any options given as commonopts are also used, if they are defined for the given symbol.  Then, for each of these symbols, all options which now still have the value Inherited are given their explicit inherited values.  The resulting full set of option values (excluding Style and Debug) is saved for each of the symbols.";
ShowFigStyle::usage="ShowFigStyle[style] displays the full set of options for each figure object class as specified in the given style.";
WithStyle::usage="WithStyle[style,body] evaluates body with the default options set according to style.";
 *)


(*FigText*)


textup::usage="textup[text] gives non-italic text.";
textsl::usage="textsl[text] gives slanted text.";
textit::usage="textit[text] gives italic text.";
textmd::usage="textmd[text] gives non-bold text.";
textbf::usage="textbf[text] gives bold text.";
textrm::usage="textrm[text] gives text in the Times font.";
texttt::usage="texttt[text] gives text in the Courier font.";
textsf::usage="textsf[text] gives text in the Helvetica font.";

textsize::usage="textsize[size,text] gives text with the specified point size.";
textcolor::usage="textcolor[color,text] gives text with the specified color.";
texttracking::usage="texttracking[tracking,text] gives text with the specified tracking.";
textfamily::usage="textfamily[family,text] gives text with the specified family.";
texthidden::usage="texthidden[text] gives invisible text.";

textsubscript::usage="UNDOCUMENTED";
textsuperscript::usage="UNDOCUMENTED";
textsubsuperscript::usage="UNDOCUMENTED";

hspace::usage="hspace[width] produces box of given width in ems, which may be negative.";

StackText::usage="StackText[alignment,linegap,{line1,...}] produces a multiline label.";


SuperPrimeBox::usage="SuperPrimeBox[x] places x in a SuperscriptBox with superscript prime. SuperPrimeBox[x,n] produces n primes. (UNDOCUMENTED)";
SuperPrime::usage="SuperPrime[x] superscripts x with a prime, in analogy to SuperPlus, SuperMinus, etc. -- since uses Superscript, behaves like mathematical expression, including automatic parenthesization.   SuperPrime[x,n] produces n primes.  (DEPRECATED)";
UnitsLabel::usage="UNDOCUMENTED";
MultipletLabel::usage="MultipletLabel[{n1,n2,...}] displays as \"(n1,n2,...)\".  The option EntrySeparator specifies a separator to be used instead of commas, or None.  The option Delimiter->{left,right} specifies a delimiter to be used instead of left and right parentheses.";
EntrySeparator::usage="Option for MultipletLabel.";

NucleusBox::usage="UNDOCUMENTED (Limitation: Left subscript and superscript are left aligned.)";
NuclearA::usage="UNDOCUMENTED";
NuclearN::usage="UNDOCUMENTED";
NuclearZ::usage="UNDOCUMENTED";
SpectroscopicLetter::usage="SpectroscopicLetter[l] returns the letter s, p, d, ... representing angular momentum l, for 0\[LessEqual]l\[LessEqual]8.";
ShellLabel::usage="ShellLabel[{n,l,j}] or ShellLabel[{l,j}] returns the nlj shell label in spectroscopic notation.";
ElementAbbreviation::usage="ElementAbbreviation[Z] returns the abbreviation for atomic number Z.  The function provides an offline alternative to the data provided by Mathematica's ElementData[Z,\"Abbreviation\"] and is therefore not dependent upon internet access.  Data is from the legacy ChemicalElements package (version 1.4).";
ElementName::usage="ElementName[Z] returns the element name for atomic number Z.  The function provides an offline alternative to the data provided by Mathematica's ElementData[Z,\"Abbreviation\"] and is therefore not dependent upon internet access.  Data is from the legacy ChemicalElements package (version 1.4).";
StableIsotopes::usage="StableIsotopes[Z] returns a list of stable isotopes of element Z.  The function provides an offline alternative to the data provided by Mathematica's IsotopeData[{Z,A},\"Stable\"] and is therefore not dependent upon internet access.  Data is from the legacy ChemicalElements package (version 1.4).";IsotopeIsStable::usage="IsotopeIsStable[{Z,A}] returns true if mass A is a stable isotope of element Z.  The function provides an offline alternative to the data provided by Mathematica's IsotopeData[{Z,A},\"Stable\"] and is therefore not dependent upon internet access.  Data is from the legacy ChemicalElements package (version 1.4).";
Isotope::usage="UNDOCUMENTED -- inspired by usage of LaTeX isotope package";
LabelJP::usage="LabelJP[J,P], or Label[J] for P=+1, produces a level spin label, with rational number fractions converted to typeset fractions.  DEPRECATED in favor of LevelLabel.";
LabelJiP::usage="LabelJiP[J,i,P], or LabelJiP[J,i] for P=+1, produces a level spin label, with rational number fractions converted to typeset fractions, with subscript i.  DEPRECATED in favor of LevelLabel.";
LevelLabel::usage="UNDOCUMENTED";
Parity::usage="Option for LevelLabel";
MultipolaritySymbols::usage="Option for RTPLabel";
MultipolarityStyle::usage="Option for RTPLabel";
EnergyLabel::usage="E(level) UNDOCUMENTED";
RTPLabel::usage="UNDOCUMENTED";
RMELabel::usage="UNDOCUMENTED";
MomentLabel::usage="UNDOCUMENTED";

TextFractionBox::usage="TextFractionBox[x,y] typesets x/y as a vertical fraction, but with smaller size and tighter spacing than provided by Mathematica (as in a LaTeX \\tfrac).";
SolidusFractionBox::usage="SolidusFractionBox[x,y] typesets x/y as a solidus fraction.";
DiagonalFractionBox::usage="DiagonalFractionBox[x,y] typesets x/y as a diagonal fraction.  The option Spacings->(spacing) controls the horizontal separation between the elements of the fraction.  The option Baseline->{numeratorheight,slashheight,denominatorheight} controls the vertical positioning of the different elements of the fraction.  KernForSuperscript->(adjustment) introduces a horizontal adjustment to the position of any superscript attached to the fraction.  The default option values produce visually reasonable results for the Times New Roman font.";
KernForSuperscript::usage="KernForSuperscript is an option for DiagonalFractionBox.";

Fractionize::usage="Fractionize[expr] converts any expression with nonzero denominator into a FractionBox.";
TextFractionize::usage="TextFractionize[expr] converts any expression with nonzero denominator into a TextFractionBox.";
SolidusFractionize::usage="SolidusFractionize[expr] converts any expression with nonzero denominator into a SolidusFractionBox.";
DiagonalFractionize::usage="DiagonalFractionize[expr] converts any expression with nonzero denominator into a DiagonalFractionBox.";

FractionString::usage="Provides a compact solidus-delimited fraction representation of a rational number, in string form. (UNDOCUMENTED)";
PiFractionString::usage="Provides a compact solidus-delimited fraction representation of a rational multiple of Pi, in string form. (UNDOCUMENTED)";

SignString::usage="SignString[x] returns \"+\", \"\", or \"-\", depending upon the value of Sign[x].";
GradeSignString::usage="SignString[g] returns \"+\" or \"-\", depending upon the value of (-)^g.";

Sqrtize::usage="Sqrtize[x] formats a fraction which is the square root of a rational number entirely under the radical (e.g., the usual format for Clebsch-Gordan coefficients).";
Radicalize::usage="Radicalize[x,n] formats a fraction involving nth roots entirely under the radical.";

SubarrayEllipsis::usage="SubarrayEllipsis[m,{rows,cols}] prints a Subarray of m with ellipses appended.";

AlignmentBox::usage= "UNDOCUMENTED  (AlignmentBox options: Any option for GridBox may be used. ColumnWidths is crucial option for alignment across labels. Options[AlignmentBox] overrides GridBox defaults of ColumnAlignments and ColumnSpacings. Any option for StyleBox may be used. Background is the most likely to be needed.)";
Align::usage="Align is an option for AlignmentBox. (UNDOCUMENTED)";
LaTeXTableEntryValue::usage="LaTeXTableEntryValue[Str] takes a typical LaTeX table entry and converts it to a number, by stripping it of any leading nonnumeric characters, any \"&\", and any trailing error estimate following \"(\" or \"\[PlusMinus]\".   (UNDOCUMENTED)";

PageBreak::usage="PageBreak[] prints a cell with attribute PageBreakBelow set to True.";


(*FigWindow*)


TransformRegion::usage="TransformRegion[tfcn,{{x1,x2},{y1,y2}}] applies tranformation function tfcn to the given Cartesian rectangular region.";


FigWindow::usage="Class representing window coordinate system within a figure.";


WithOrigin::usage="WithOrigin[p,body] shifts the origin of coordinates to point p while evaluating body.  WithOrigin[x,body] produces a horizontal shift of x, equivalent to WithOrigin[{x,0},body].";
WithClipping::usage="WithClipping[r,body] sets the region to r while evaluating body.  (UNDER DEVELOPMENT)";


(*End package*)


(*Exit package context*)


EndPackage[];
