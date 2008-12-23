'\" t
.TH Vectors_params 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Vectors_params - This document briefly describes all Vectors
internal parameters.
.SH DESCRIPTION 
Parameter descriptions follow, in alphabetical order. Each
description begins with a line giving the three-character
mnemonic name of the parameter, the phrase for which the
mnemonic stands, the intrinsic type of the parameter, and
an indication of whether or not it is an array.
.IP "ACM - Arrow Color Mode - Integer"

ACM controls how color is applied to filled vector arrows. It applies
only when AST has the value 1. Its
behavior also depends on the setting of the parameter CTV. Assuming
that CTV is set to a non-zero value, implying that multi-colored
vectors are desired, ACM has the following settings:

.sp
.TS
tab(/);
l l l l.
Value/Effect
-----/------
-2/Multi-colored fill; outline off
-1/Fill off; multi-colored outline
0/Multi-colored fill; mono-colored outline
1/Mono-colored fill; multi-colored outline
2/Multi-colored fill; multi-colored outline
.TE
.sp
Mono-colored outlines use the current GKS polyline color index. Mono-colored
fill uses the current GKS fill color index. When CTV is set to 0, both the
fill and the outlines become mono-colored, and therefore only modes
-2, -1, and 0 remain distinguishable. The default value is 0.
.IP "AFO - Arrow Fill Over Arrow Lines - Integer"
If AFO is set to 1, the perimeter outline of a filled vector arrow is
drawn first, underneath the fill. In this case, you must set the line
thickness parameter (LWD) to a value greater than unity in order for
the line to appear completely. The advantage of drawing the line
underneath is that the full extent of the fill appears, resulting in a
crisper, more sharply defined arrow; when the line is drawn on top of
the fill using a different color index, the fill color may be
partially or completely obscured, especially for small vector
arrows. AFO has an effect only when the parameter AST is set to 1.
The default value of AFO is 1.
.IP "AIR - Arrow Interior Reference Fraction  - Real"
AIR specifies the distance from the point of the arrowhead of a filled
vector arrow drawn at the reference length to the point where the
arrowhead joins with the line extending to the tail of the arrow. Its
value represents a fraction of the reference length.  This distance is
adjusted proportionally to the X component of the arrowhead size for
vector arrows whose length differs from the reference length.  See VRL
for an explanation of how the reference length is determined.  AIR has
an effect only when AST is set to 1. AIR is allowed to vary between
0.0 and 1.0 and its default value is 0.33.
.IP "AMN - Arrow Head Minimum Size - Real"
Specifies a minimum length for the two lines representing the point of
the vector arrow head, as a fraction of the viewport width. AMN has an
effect only for line-drawn vector arrows (parameter AST set to
0). Normally the arrow head size is scaled proportionally to the
length of the vector. This parameter allows you to ensure that the
arrow head will remain recognizable even for very short vectors. Note
that you can cause all the arrowheads in the plot to be drawn at the
same size if you set AMN and AMX to the same value. If you set both AMN
and AMX to 0.0 the arrowheads will not be drawn at all.
The default value is 0.005. 
.IP "AMX - Arrow Head Maximum Size - Real"
Specifies a maximum length for the two lines representing the point of
the vector arrow head, as a fraction of the viewport width.  AMX has
an effect only for line-drawn vector arrows (parameter AST set to
0). Normally the arrow head is scaled proportionally to the length of
the vector. This parameter allows you to ensure that the arrow heads
do not become excessively large for high magnitude vectors. Note
that you can cause all the arrowheads in the plot to be drawn at the
same size if you set AMN and AMX to the same value. If you set both AMN
and AMX to 0.0 the arrowheads will not be drawn at all. The
default value is 0.05.
.IP "AST - Arrow Style - Integer"

If AST is set to 0, the vector arrows are drawn using lines only. When
AST is set to 1, the vectors are plotted using variable width filled
arrows, with an optional outline. If AST is set to 2, wind barb glyphs
are used to represent the vectors.There are parameters for controlling
the appearance of each style. These have an effect only for one value
of AST.  However, certain parameters apply to all arrow styles. Here
is a table of parameters that affect the appearance of vectors and how
their behavior is affected by the setting of AST:
.sp
.TS
tab(/);
l l l l.
Parameter/Line-Drawn Arrows/Filled Arrows/Wind Barbs/
---------/-----------------/-------------/----------
ACM//x//
AFO//x//
AIR//x//
AMN/x///
AMX/x///
AWF//x//
AWR//x//
AXF//x//
AXR//x//
AYF//x//
AYR//x//
CLR/x/x/x/
CTV/x/x/x/
LWD/x/x/x/
NLV/x/x/x/
PAI/x/x/x/
TVL/x/x/x/
WBA///x/
WBC///x/
WBD///x/
WBS///x/
WBT///x/
.TE
.sp
When filled arrows are used, colors associated with the threshold
levels may be applied to either or both the fill or the outline of the
arrow.  When fill is drawn over the outline (AFO set to 1), LWD should
be set to a value greater than 1.0 in order for the outline to be fully
visible.  The default value of AST is 0.
.IP "AWF - Arrow Width Fractional Minimum - Real"
AWF specifies the width of a filled arrow drawn at the minimum length,
as a fraction of the width of an arrow drawn at the reference
length. If AWF has the value 0.0, then the ratio of the arrow width to
the arrow length will be constant for all arrows in the plot.  If
given the value 1.0, the width will itself be constant for all arrows
in the plot, regardless of length. See VFR for a discussion of how the
minimum length is determined. AWF has an effect only when AST is set
to 1.  AWF is allowed to vary between 0.0 and 1.0 and its default
value is 0.0.
.IP "AWR - Arrow Width Reference Fraction - Real"
AWR specifies the width of a filled vector arrow drawn at the
reference length, as a fraction of the reference length.  See VRL for
an explanation of how the reference length is determined.  AWR has an
effect only when AST is set to 1. AWR is allowed to vary between 0.0
and 1.0 and its default value is 0.03.
.IP "AXF - Arrow X-Coord Fractional Minimum - Real"
AXF specifies the X component of the head of a filled vector arrow
drawn at the minimum length, as a fraction of the X component of the
head of an arrow drawn at the reference length. The X component of the
arrowhead is the distance from the point of the arrowhead to a point
along the centerline of the arrow perpendicular the arrowhead\'s rear
tips. If AXF has the value 0.0, then the ratio of the X component of
the arrowhead size to the arrow length will be constant for all
vectors in the plot. If given the value 1.0, the arrowhead X component
will itself be constant for all arrows in the plot, regardless of
their length. See VRL for an explanation of how the reference length
is determined.  AXF has an effect only when AST is set to 1. AXF is
allowed to vary between 0.0 and 1.0 and its default value is 0.0.
.IP "AXR - Arrow X-Coord Reference Fraction - Real"
AXR specifies the X component of the head of a filled vector arrow
drawn at the reference length, as a fraction of reference length. The
X component of the arrowhead is the distance from the point of the
arrowhead to a point along the centerline of the arrow perpendicular
the arrowhead\'s rear tips.  See VRL for an explanation of how the
reference length is determined.  AXR has an effect only when AST is
set to 1. AXR is allowed to vary between 0.0 and 2.0 and its default
value is 0.36.
.IP "AYF - Arrow Y-Coord Fractional Minimum - Real"
The value of this parameter, when added to the minimum width value,
specifies the Y component length of the arrowhead size for a filled
arrow drawn at the minimum length, as a fraction of the length
specified by AYF. If given the value 1.0, the arrowhead Y component
will extend the same distance perpendicularly from the edge of all
arrows in the plot, regardless of their length and width. This can be
a useful resource to adjust to ensure that the points of even very
short vector arrows remain visible. See VFR for a discussion of how
the minimum length is determined.  AYF has an effect only when AST is
set to 1. AYF is allowed to vary between 0.0 and 1.0 and its default
value is 0.25.
.IP "AYR - Arrow Y-Coord Reference Fraction - Real"
AYR specifies the perpendicular distance from one side of a filled
vector arrowdrawn at the reference length to one of the back tips of
the arrowhead. The value represents a fraction of the value of of the
reference length and, when added to half the arrow width, determines
the Y component of the arrowhead size.  See VRL for an explanation of
how the reference length is determined.  AYR has an effect only when
AST is set to 1.  AYR is allowed to vary between 0.0 and 1.0 and its
default value is 0.12.
.IP "CLR - Array of GKS Color Indices - Integer Array"
This parameter represents an array containing the GKS color index to
use for coloring the vector when the scalar quantity is less than or
equal to the threshold value with the same index in the TVL threshold
value array. Depending on the settings of AST and ACM it may specify a
set of fill color indexes, a set of line color indexes, or both. In
order to access a particular element of the CLR array, you must first
set the value of PAI, the parameter array index parameter, to the
value of the array element\'s index. All elements of the array are set
to one initially. Note that the Vectors utility makes no calls to set
the GKS color representation (GSCR), nor ever modifies the contents of
the CLR array; therefore you are responsible for creating a suitably
graduated color palette and assigning the color index values into the
CLR array, prior to calling VVECTR. Typically, assuming the desired
RGB values have been previously stored in a 2 dimensional 3 x n array
called RGB, you loop through the calls that set up the color
representation and color index as in the following example for a
fourteen color palette:
.sp
.in 15
.nf
    DO 100 I=1,14,1
.br
        CALL GSCR (1,I,RGB(1,I),RGB(2,I),RGB(3,I))
.br
        CALL VVSETI(\'PAI -- Parameter Array Index\', I)
.br
        CALL VVSETI(\'CLR -- GKS Color Index\', I)
.br
100 CONTINUE
.in -15
.fi
.IP ""
See the descriptions of CTV, NLV, and TVL for details on
configuring the vector coloring scheme.
.IP "CPM - Compatibility Mode - Integer"
Controls the degree of compatibility between pre-Version 3.2
capabilities of the Vectors utility and later versions. You can 
independently control three behaviors using the nine
settings provided:
.RS
.IP \(bu
use of VELVCT and VELVEC input parameters
.IP \(bu
use of variables initialized in the VELDAT block data
statement
.IP \(bu
use of the old mapping routines, FX, FY, MXF, and MYF.
.RE
.IP ""
Note, however, that when using the Version 3.2 entry points
VVINIT and VVECTR, only the third behavior option has any
meaning.
.sp
When CPM is set to 0, its default value, the Vectors utility\'s
behavior varies depending on whether you access it through one of the
pre-Version 3.2 entry points (VELVCT, VELVEC, and EZVEC), or through
the VVINIT/VVECTR interface. Otherwise, positive values result in
invocation of the pre-Version 3.2 mapping routines (FX, FY, MXF, and
MYF) for the conversion from data to user coordinates. Negative values
cause VVMPXY or perhaps VVUMXY to be used instead. When using the
pre-Version 3.2 interface, odd values of CPM cause the data values in
the VELDAT block data subroutine to override corresponding values
initialized in the Version 3.2 VVDATA block data subroutine, or set by
the user calling VVSETx routines. Values of CPM with absolute value
greater than two cause some of the input arguments to VELVEC and
VELVCT to be ignored. These include FLO, HI, NSET, ISPV, SPV and (for
VELVCT only) LENGTH.
.sp
Here is a table of the nine settings of CPM and their
effect on the operation of the Vectors utility:
.sp
.TS
tab(/);
l l l l.
Value/Use FX, FY, etc./Use VELDAT data/Use input args
-----/----------------/---------------/--------------
-4/no/no/no
-3/no/yes/no
-2/no/no/yes
-1/no/yes/yes
0/old - yes; new - no (*)/yes/yes
1/yes/yes/yes
2/yes/no/yes
3/yes/yes/no
4/yes/no/no
.TE
.sp
(*) Old means EZVEC, VELVEC, VELVCT entry point; new, VVINIT/VVECTR.
Only the first column applies to the VVINIT/VVECTR interface. See the
velvct man page for more detailed emulation information.
.IP "CTV - Color Threshold Value Control - Integer"
In conjunction with NLV, this parameter controls vector coloring and
the setting of threshold values. The vectors may be colored based on
on the vector magnitude or on the contents of a scalar array
(VVINIT/VVECTR input argument, P). A table of supported options
follows:
.RS
.IP Value 15
Action
.IP -2 15
Color vector arrows based on scalar array
data values; the user is responsible for
setting up threshold level array, TVL
.IP -1 15
Color vector arrows based on vector
magnitude; the user is responsible for
setting up values of threshold level array.
.IP "0(default)" 15
Color all vectors according to the current
GKS polyline color index value. Threshold
level array, TVL and GKS color index
array, CLR are not used.
.IP 1 15
Color vector arrows based on vector
magnitude; VVINIT assigns values to the
first NLV elements of the threshold level
array, TVL.
.IP 2 15
Color vector arrows based on scalar array
data values; VVINIT assigns values to the
first NLV elements of the threshold level
array, TVL.
.RE
.IP " "
If you make CTV positive, you must initialize Vectors with a call to
VVINIT after the modification.
.IP "DMN - NDC Minimum Vector Size - Real, Read-Only"
This parameter is read-only and has a useful
value only following a call to VVECTR (directly or through
the compatibility version of VELVCT). You may retrieve it
in order to determine the length in NDC space of the
smallest vector actually drawn (in other words, the
smallest vector within the boundary of the user coordinate
space that is greater than or equal in magnitude to the
value of the VLC parameter). It is initially set to a value
of 0.0.
.IP "DMX - NDC Maximum Vector Size - Real, Read-Only"
Unlike DMN this read-only parameter has a potentially useful value
betweens calls to VVINIT and VVECTR. However, the value it reports may
be different before and after the call to VVECTR. Before the VVECTR call
it contains the length in NDC space that would be used to
render the maximum size vector assuming the user-settable parameter,
VRL is set to its default value of 0.0.  After the VVECTR call it
contains the NDC length used to render the largest vector actually
drawn (in other words, the largest vector within the boundary of the
user coordinate space that is less than or equal in magnitude to the
value of the VHC parameter). See the section on the VRL parameter for
information on using the value of DMX after the VVINIT call in order
to adjust proportionally the lengths of all the vectors in the plot.
It is initially set to a value of 0.0.
.IP "DPF - Vector Label Decimal Point Control Flag - Integer"
If DPF is set to a non-zero value, and the optional vector
magnitude labels are enabled, the magnitude values are
scaled to fit in the range 1 to 999. The labels will
contain 1 to 3 digits and no decimal point. Otherwise, the
labels will consist of a number up to six characters long,
including a decimal point. By default DPF is set to the
value 1.
.IP "LBC - Vector Label Color - Integer"
This parameter specifies the color to use for the optional
vector magnitude labels, as follows:
.RS
.IP Value 15
Action
.IP "< -1" 15
Draw labels using the current GKS text
color index
.IP "-1 (default)" 15
Draw labels using the same color as the
corresponding vector arrow
.IP >=0 15
Draw labels using the LBC value as the GKS
text color index
.RE
.IP "LBL - Vector Label Flag - Integer"
If set non-zero, Vectors draws labels representing the vector
magnitude next to each arrow in the field plot.  The vector labels are
primarily intended as a debugging aid, since in order to avoid
excessive overlap, you must typically set the label text size too
small to be readable without magnification. For this reason, as well
as for efficiency, unlike the other graphical text elements supported
by the Vectors utility, the vector labels are rendered using low
quality text.
.IP "LBS - Vector Label Character Size - Real"
This parameter specifies the size of the characters used
for the vector magnitude labels as a fraction of the
viewport width. The default value is 0.007.
.IP "LWD - Vector Linewidth - Real"

LWD controls the linewidth used to draw the lines that form vector
arrows and wind barbs. When the arrows are filled (AST is set to 1)
LWD controls the width of the arrow's outline. If the fill is drawn
over the outline (AFO set to 1) then LWD must be set to a value
greater than 1.0 in order for the outline to appear properly. When AST
has the value 2, LWD controls the width of the line elements of wind
barbs. When AST is set to 0, specifying line-drawn vector arrows, the
linewidth applies equally to the body of the vector and the
arrowhead. Overly thick lines may cause the arrow heads to appear
smudged. This was part of the motivation for developing the option of
filled vector arrows. Note that since linewidth in NCAR Graphics is
always calculated relative to a unit linewidth that is dependent on
the output device, you may need to adjust the linewidth value
depending on the intended output device to obtain a pleasing plot. The
default is 1.0, specifying a device-dependent minimum linewidth.

.IP "MAP - Map Transformation Code - Integer"
MAP defines the transformation between the data and user
coordinate space. 
Three MAP
parameter codes are reserved for pre-defined
transformations, as follows:
.RS
.IP Value 15
Mapping transformation
.IP "0 (default)" 15
Identity transformation between data and
user coordinates: array indices of U, V,
and P are linearly related to data
coordinates.
.IP 1 15
Ezmap transformation: first dimension
indices of U, V, and P are linearly
related to longitude; second dimension
indices are linearly related to latitude.
.IP 2 15
Polar to rectangular transformation: first
dimension indices of U, V, and P are
linearly related to the radius; second
dimension indices are linearly related to
the angle in degrees.
.RE
.IP ""
If MAP has any other value, Vectors invokes the user-modifiable
subroutine, VVUMXY, to perform the mapping.  The default version of
VVUMXY simply performs an identity transformation. Note that, while
the Vectors utility does not actually prohibit the practice, the user
is advised not to use negative integers for user-defined mappings,
since other utilities in the NCAR Graphics toolkit attach a special
meaning to negative mapping codes.
.sp
For all the predefined mappings, the linear relationship between the
grid array indices and the data coordinate system is established using
the four parameters, XC1, XCM, YC1, and YCN. The X parameters define a
mapping for the first and last indices of the first dimension of the
data arrays, and the Y parameters do the same for the second
dimension. If MAP is set to a value of one, be careful to ensure that
the SET parameter is given a value of zero, since the Ezmap routines
require a specific user coordinate space for each projection type, and
internally call the SET routine to define the user to NDC mapping.
Otherwise, you may choose whether or not to issue a SET call prior to
calling VVINIT, modifying the value of SET as required.  See the
description of the parameter, TRT, and the vvumxy man page for more
information.
.IP "MNC - Minimum Vector Text Block Color - Integer"
MNC specifies the color of the minimum vector graphical
text output block as follows:
.RS
.IP "Value" 15
Action
.IP <-2 15
Both the vector arrow and the text are
colored using the current text color index.
.IP -2 15
If the vectors are colored by magnitude,
both the vector arrow and the text use the
GKS color index associated with the
minimum vector magnitude. Otherwise, the
vector arrow uses the current polyline
color index and the text uses the current
text color index.
.IP "-1 (default)" 15
If the vectors are colored by magnitude,
the vector arrow uses the GKS color index
associated with the minimum vector
magnitude. Otherwise the vector arrow uses
the current polyline color index. The text
is colored using the current text color
index in either case.
.IP ">= 0" 15
The value of MNC is used as the color
index for both the text and the vector
arrow
.RE
.IP " "
See the description of MNT for more information about the minimum
vector text block.
.IP "MNP - Minimum Vector Text Block Positioning Mode - Integer"
This parameter allows you to justify the minimum vector text block,
taken as a single unit, relative to the text block position
established by the parameters, MNX and MNY. Nine positioning modes are
available, as follows:
.RS
.IP Mode 15
Justification
.IP -4 15
The lower left corner of the text block is
positioned at MNX, MNY.
.IP -3 15
The center of the bottom edge is
positioned at MNX, MNY.
.IP -2 15
The lower right corner is positioned at
MNX, MNY.
.IP -1 15
The center of the left edge is positioned
at MNX, MNY.
.IP 0 15
The text block is centered along both axes
at MNX, MNY.
.IP 1 15
The center of the right edge is positioned
at MNX, MNY.
.IP 2 15
The top left corner is positioned at MNX,
MNY.
.IP 3 15
The center of the top edge is positioned
at MNX, MNY.
.IP "4 (default)" 15
The top right corner is positioned at MNX,
MNY.
.RE
.IP " "
See the description of MNT for more information about the minimum
vector text block.
.IP "MNS - Minimum Vector Text Block Character Size - Real"
MNS specifies the size of the characters used in the minimum vector
graphics text block as a fraction of the viewport width. See the
description of MNT for more information about the minimum vector text
block. The default value of MNS is 0.0075.
.IP "MNT - Minimum Vector Text String - Character* 36"
The minimum vector graphics text block consists of a user-definable
text string centered underneath a horizontal arrow. If the
parameter VLC is set negative the arrow is rendered at the size of the
reference minimum magnitude vector (which may be smaller than any
vector that actually appears in the plot). Otherwise, the arrow is the
size of the smallest vector in the plot. Directly above
the arrow is a numeric string in exponential format that represents
the vector's magnitude.
.sp
Use MNT to modify the text appearing below the vector in
the minimum vector graphics text block. Currently the
string length is limited to 36 characters. Set MNT to a
single space (\' \') to remove the text block, including the
vector arrow and the numeric magnitude string, from the
plot. The default value is \'Minimum Vector\'
.IP "MNX - Minimum Vector Text Block X Coordinate - Real"
MNX establishes the X coordinate of the minimum vector graphics text
block as a fraction of the viewport width.  Values less than 0.0 or
greater than 1.0 are permissible and respectively represent regions to
the left or right of the viewport. The actual position of the block
relative to MNX depends on the value assigned to MNP. See the
descriptions of MNT and MNP for more information about the minimum
vector text block. The default value of MNX is 0.475.
.IP "MNY - Minimum Vector Text Block Y Coordinate - Real"
MNY establishes the Y coordinate of the minimum vector graphics text
block as a fraction of the viewport height.  Values less than 0.0 or
greater than 1.0 are permissible and respectively represent regions
below or above the viewport. The actual position of the block relative
to MNY depends on the value assigned to MNP. See the descriptions of
MNT and MNP for more information about the minimum vector text
block. The default value of MNY is -0.01.
.IP "MSK - Mask To Area Map Flag - Integer"
Use this parameter to control masking of vectors to an existing area
map created by routines in the Areas utility.  When MSK is greater
than 0, masking is enabled and an the area map must be set up prior to
the call to VVECTR. The area map array and, in addition, the name of a
user-definable masked drawing routine, must be passed as input
parameters to VVECTR. Various values of the MSK parameter have the
following effects:
.RS
.IP Value 15
Effect
.IP "<= 0 (default)" 15
No masking of vectors.
.IP 1 15
The subroutine ARDRLN is called internally to decompose the vectors
into segments contained entirely within a single area.  ARDRLN calls
the user-definable masked drawing subroutine.
.IP >1 15
Low precision masking. ARGTAI is called internally to get the area
identifiers for the vector base position point. Then the
user-definable masked drawing subroutine is called to draw the
vector. Vectors with nearby base points may encroach into the intended
mask area.
.RE
.IP ""
See the man page vvudmv 
for further explanation of
masked drawing of vectors
.IP "MXC - Maximum Vector Text Block Color - Integer"
MXC specifies the color of the maximum vector graphical text output
block as follows:
.RS
.IP Value 15
Action
.IP <-2 15
Both the vector arrow and the text are colored using the current text
color index.
.IP -2 15
If the vectors are colored by magnitude, both the vector arrow and the
text use the GKS color index associated with the minimum vector
magnitude. Otherwise, the vector arrow uses the current polyline color
index and the text uses the current text color index.
.IP "-1 (default)" 15
If the vectors are colored by magnitude, the vector arrow uses the GKS
color index associated with the minimum vector magnitude. Otherwise
the vector arrow uses the current polyline color index. The text is
colored using the current text color index in either case.
.IP ">= 0" 15
The value of MXC is used as the color index for both the text and the
vector arrow
.RE
.IP " "
See the description of MXT for more information about the maximum
vector text block.
.IP "MXP - Maximum Vector Text Block Positioning Mode - Integer"
This parameter allows you to justify the maximum vector text block,
taken as a single unit, relative to the text block position
established by the parameters, MXX and MXY. Nine positioning modes are
available, as follows:
.RS
.IP Mode 15
Justification
.IP -4 15
The lower left corner of the text block is positioned at MXX, MXY.
.IP -3 15
The center of the bottom edge is positioned at MXX, MXY.
.IP -2 15
The lower right corner is positioned at MXX, MXY.
.IP -1 15
The center of the left edge is positioned at MXX, MXY.
.IP 0 15
The text block is centered along both axes at MXX, MXY.
.IP 1 15
The center of the right edge is positioned at MXX, MXY.
.IP 2 (default) 15
The top left corner is positioned at MXX, MXY.
.IP 3 15
The center of the top edge is positioned at MXX, MXY.
.IP 4 15
The top right corner is positioned at MXX,
MXY.
.RE
.IP " "
See the description of MXT for more information about the maximum
vector text block.
.IP "MXS - Maximum Vector Text Block Character Size - Real"
MXS specifies the size of the characters used in the maximum vector
graphics text block as a fraction of the viewport width. See the
description of MXT for more information about the maximum vector text
block. The default value is 0.0075.
.IP "MXT - Maximum Vector Text String - Character* 36"
The maximum vector graphics text block consists of a user-definable
text string centered underneath a horizontal arrow. If the parameter
VHC is set negative the arrow is rendered at the size of the
reference maximum magnitude vector (which may be larger than any
vector that actually appears in the plot). Otherwise, the arrow is the
size of the largest vector in the plot. Directly above
the arrow is a numeric string in exponential format that represents
the magnitude of this vector. 
.sp
Use MXT to modify the text appearing below the vector in the maximum
vector graphics text block. Currently the string length is limited to
36 characters. Set MXT to a single space (\' \') to completely remove
the text block, including the vector arrow and the numeric magnitude
string, from the plot. Note that the name "Maximum Vector Text Block"
is no longer accurate, since using the parameter VRM it is now
possible to establish a reference magnitude that is smaller than the
maximum magnitude in the data set. A more accurate name would be
"Reference Vector Text Block".  The default value of MXT is
\'Maximum Vector\'.
.IP "MXX - Maximum Vector Text Block X Coordinate - Real"
MXX establishes the X coordinate of the maximum vector graphics text
block as a fraction of the viewport width.  Values less than 0.0 or
greater than 1.0 are permissible and respectively represent regions
below or above of the viewport. The actual position of the block
relative to MXX depends on the value assigned to MXP. See the
descriptions of MXT and MXP for more information about the maximum
vector text block. The default value is 0.525.
.IP "MXY - Maximum Vector Text Block Y Coordinate - Real"
MXY establishes the Y coordinate of the maximum vector graphics text
block as a fraction of the viewport width.  Values less than 0.0 or
greater than 1.0 are permissible and respectively represent regions
below or above the viewport. The actual position of the block relative
to MXY depends on the value assigned to MXP. See the descriptions of
MXT and MXP for more information about the maximum vector text block.
The default value is -0.01.
.IP "NLV - Number of Colors Levels - Integer"
NLV specifies the number of color levels to use when coloring the
vectors according to data in a scalar array or by vector magnitude.
Anytime CTV has a non-zero value, you must set up the first NLV
elements of the color index array CLR. Give each element the value of
a GKS color index that must be defined by a call to the the GKS
subroutine, GSCR, prior to calling VVECTR. If CTV is less than 0, in
addition to setting up the CLR array, you are also responsible for
setting the first NLV elements of the threshold values array, TVL to
appropriate values. NLV is constrained to a maximum value of
255. The default value of NLV is 0, specifying that vectors are colored
according to the value of the GKS polyline color index currently in
effect, regardless of the value of CTV.  If CTV is greater than 0, you
must initialize Vectors with a call to VVINIT after modifying this
parameter.
.IP "PAI - Parameter Array Index - Integer"
The value of PAI must be set before calling VVGETC, VVGETI, VVGETR,
VVSETC, VVSETI, or VVSETR to access any parameter which is an array;
it acts as a subscript to identify the intended array element. For
example, to set the 10th color threshold array element to 7, use code
like this:
.sp
.in 15
CALL VVSETI (\'PAI - PARAMETER ARRAY INDEX\',10)
.br
CALL VVSETI (\'CLR - Color Index\',7)
.in -15
.IP ""
The default value of PAI is one.
.IP "PLR - Polar Input Mode - Integer"
When PLR is greater than zero, the vector component arrays are
considered to contain the field data in polar coordinate form: the U
array is treated as containing the vector magnitude and the V array as
containing the vector angle. Be careful not to confuse the PLR
parameter with the MAP parameter set to polar coordinate mode (2). The
MAP parameter relates to the location of the vector, not its
value. Here is a table of values for PLR:
.RS
.IP Value 15
Meaning
.IP "0 (default)" 15
U and V arrays contain data in cartesian
component form.
.IP 1 15
U array contains vector magnitudes; V array contains vector angles in
degrees.
.IP 2 15
U array contain vector magnitudes; V array contains vector angles in
radians.
.RE
.IP " "
You must initialize Vectors with a call to VVINIT after modifying this
parameter.
.IP "PMN - Minimum Scalar Array Value - Real, Read-Only"
You may retrieve the value specified by PMN at any
time after a call to VVINIT. It will contain a copy of the
minimum value encountered in the scalar data array. If no
scalar data array has been passed into VVINIT it will have
a value of 0.0.
.IP "PMX - Maximum Scalar Array Value - Real"
You may retrieve the value specified by PMX at any
time after a call to VVINIT. It contains a copy of the
maximum value encountered in the scalar data array.  If no
scalar data array has been passed into VVINIT it will have
a value of 0.0.
.IP "PSV - P Array Special Value - Real"
Use PSV to indicate the special value that flags an unknown data value
in the P scalar data array. This value will not be considered in the
determination of the data set maximum and minimum values. Also,
depending on the setting of the SPC parameter, the vector may be
specially colored to flag the unknown data point, or even eliminated
from the plot. You must initialize Vectors with a call to VVINIT after
modifying this parameter.
.IP "SET - SET Call Flag - Integer"
Give SET the value 0 to inhibit the SET call VVINIT performs by
default. Arguments 5-8 of a SET call made by the user must be
consistent with the ranges of the user coordinates expected by
Vectors. This is determined by the mapping from grid to data
coordinates as specified by the values of the parameters XC1, XCM,
YC1, YCN, and also by the mapping from data to user coordinates
established by the MAP parameter. You must initialize Vectors with a
call to VVINIT after modifying this parameter. The default value of
SET is 1.
.IP "SPC - Special Color - Integer"
SPC controls special value processing for the optional
scalar data array used to color the vectors, as follows:
.RS
.IP Value 15
Effect
.IP "< 0 (default)" 15
The P scalar data array is not examined
for special values.
.IP 0 15
Vectors at P scalar array special value
locations are not drawn.
.IP "> 0" 15
Vectors at P scalar array special value
locations are drawn using color index SPC.
.RE
.IP " "
You must initialize Vectors with a call to VVINIT after modifying this
parameter.
.IP "SVF - Special Value Flag - Integer"
The special value flag controls special value processing for the U and
V vector component data arrays. Special values may appear in either
the U or V array or in both of them. Five different options are
available (although the usefulness of some of the choices is
debatable):
.RS
.IP Value 15
Effect
.IP "0 (default)" 15
Neither the U nor the V array is examined
for special values
.IP 1 15
Vectors with special values in the U array
are not drawn
.IP 2 15
Vectors with special values in the V array
are not drawn
.IP 3 15
Vectors with special values in either the
U or V array are not drawn
.IP 4 15
Vectors with special values in both the U
and V arrays are not drawn
.RE
.IP ""
The U and V special values are defined by setting parameters USV and
VSV. You must initialize Vectors with a call to VVINIT after modifying
this parameter.
.IP "TRT - Transformation Type - Integer"
As currently implemented, TRT further qualifies the mapping
transformation specified by the MAP parameter, as follows:
.RS
.IP Value 15
Effect
.IP -1 15
Direction, magnitude, and location are all transformed. This option is
not currently supported by any of the pre-defined coordinate system
mappings.
.IP 0 15
Only location is transformed
.IP "1 (default)" 15
Direction and location are transformed
.RE
.IP ""
This parameter allows you to distinguish between a system that
provides a mapping of location only into an essentially cartesian
space, and one in which the space itself mapped. To understand the
difference, using polar coordinates as an example, imagine a set of
wind speed monitoring units located on a radial grid around some
central point such as an airport control tower. Each unit\'s position
is defined in terms of its distance from the tower and its angular
direction from due east. However, the data collected by each
monitoring unit is represented as conventional eastward and northward
wind components.  Assuming the towers\'s location is at a moderate
latitude, and the monitoring units are reasonably \'local\', this is
an example of mapping a radially defined location into a nearly
cartesian space (i.e. the eastward components taken alone all point in
a single direction on the plot, outlining a series of parallel
straight lines). One would set MAP to two (for the polar
transformation) and TRT to zero to model this data on a plot generated
by the Vectors utility.
.sp
On the other hand, picture a set of wind data, again given as eastward
and northward wind components, but this time the center of the polar
map is actually the south pole. In this case, the eastward components
do not point in a single direction; instead they outline a series of
circles around the pole. This is a space mapping transformation: one
would again set MAP to two, but TRT would be set to one to transform
both direction and location.
.sp
Changing the setting of this parameter affects the end results only
when a non-uniform non-linear mapping occurs at some point in the
transformation pipeline. For this discussion a uniform linear
transformation is defined as one which satisfies the following
equations:
.sp
.in 15
x_out = x_offset + scale_constant * x_in
.br
y_out = y_offset + scale_constant * y_in
.in -15
.IP ""
If scale_constant is not the same for both the X axis and the Y axis
then the mapping is non-uniform.
.sp
This option is currently implemented only for the pre-defined MAP
parameter codes, 0 and 2, the identity mapping and the polar
coordinate mapping. However, it operates on a different stage of the
transformation pipeline in each case. The polar mapping is non-linear
from data to user coordinates. The identity mapping, even though
necessarily linear over the data to user space mapping, can have a
non-uniform mapping from user to NDC space, depending on the values
given to the input parameters of the SET call. This will be the case
whenever the LL input parameter is other than one, or when LL equals
one, but the viewport and the user coordinate boundaries do not have
the same aspect ratio. Thus for a MAP value of 2, TRT affects the
mapping between data and user space, whereas for MAP set to 0, TRT
influences the mapping between user and NDC space.
.IP "TVL - Array of Threshold Values - Real Array"
TVL is an array of threshold values that is used to determine the
individual vector color, when CTV and NLV are both non-zero. For each
vector the TVL array is searched for the smallest value greater than or
equal to the scalar value associated with the vector. The array
subscript of this element is used as an index into the CLR array.
Vectors uses the GKS color index found at this element of the CLR
array to set the color for the vector. Note that Vectors assumes that
the threshold values are monotonically increasing.
.sp
When CTV is less than 0, you are responsible for assigning values to
the elements of TVL yourself. To do this, first set the PAI parameter
to the index of the threshold level element you want to define, then
call VVSETR to set TVL to the appropriate threshold value for this
element. Assuming the desired values have previously been stored in a
array named TVALS, you could assign the threshold values for a
fourteen level color palette using the following loop:
.sp
.in 15
.nf
    DO 100 I=1,14,1
.br
        CALL VVSETI(PAI -- Parameter Array Index, I)
.br
        CALL VVSETR(TVL -- Threshold Value, TVALS(I))
.br
100 CONTINUE
.fi
.in -15
.IP ""
When CTV is greater than 0, Vectors assigns values into TVL
itself. Each succeeding element value is greater than the
preceding value by the value of the expression:
.sp
.in 15
(maximum_data_value - minimum_data_value) / NLV
.in -15
.IP ""
where the data values are either from the scalar data array or are the
magnitudes of the vectors in the vector component arrays. The first
value is equal to the minimum value plus the expression; the final
value (indexed by the value of NLV) is equal to the maximum value. If
Vectors encounters a value greater than the maximum value in the TVL
array while processing the field data, it gives the affected vector
the color associated with the maximum TVL value.
.IP "USV - U Array Special Value - Real"
USV is the U vector component array special value. It is a value
outside the range of the normal data used to indicate that there is no
valid data for this grid location. When SVF is set to 1 or 3, Vectors
will not draw a vector whose U component has the special value. You
must initialize Vectors with a call to VVINIT after modifying this
parameter. It has a default value of 1.0 E12.
.IP "VFR - Minimum Vector Fractional Length - Real"
Use this parameter to adjust the realized size of the reference
minimum magnitude vector relative to the reference maximum magnitude
vector in order to improve the appearance or perhaps the information
content of the plot. Specify VFR as a value between 0.0 and 1.0, where
0.0 represents an unmodified linear scaling of the realized vector
length, in proportion to magnitude, and 1.0 specifies that the
smallest vector be represented at 1.0 times the length of the largest
vector, resulting in all vectors, regardless of magnitude, having the
same length on the plot. A value of 0.5 means that the smallest
magnitude vector appears half as long as the largest magnitude vector;
intermediate sizes are proportionally scaled to lengths between these
extremes. Where there is a wide variation in magnitude within the
vector field, you can use this parameter to increase the size of the
smallest vectors to a usefully visible level. Where the variation is
small, you can use the parameter to exaggerate the differences that do
exist. See also the descriptions of VRL, VLC, VHC, and VRM. The default
value is 0.0.
.IP "VHC - Vector High Cutoff Value - Real"
If the parameter VRM is set to a value greater than 0.0, it supercedes
the use of VHC to specify the reference magnitude. VRM allows greater
flexibility in that it can be used to specify an arbitrary reference
magnitude that need not be the maximum magnitude contained in the data
set. VHC can still be used to set a high cutoff value -- no vectors
with magnitude greater than the cutoff value will be displayed in the
plot.
.sp
If VRM has its default value, 0.0, VHC specifies the reference maximum
magnitude represented by an arrow of length VRL (as a fraction of the
viewport width). The realized length of each individual vector in the
plot is based on its magnitude relative to the reference maximum
magnitude and, if VFR is non-zero, the reference minimum magnitude (as
specified by VLC). Note that the reference maximum magnitude may be
greater than the magnitude of any vector in the dataset. The effect of
this parameter varies depending on its value, as follows:
.RS
.IP Value 15
Effect
.IP "< 0.0" 15
The absolute value of VHC unconditionally determines the reference
maximum magnitude. Vectors in the dataset with magnitude greater than
VHC are not displayed.
.IP "0.0 (default)" 15
The vector with the greatest magnitude in the dataset determines the
reference maximum magnitude.
.IP "> 0.0" 15
The minimum of VHC and the vector with the greatest magnitude in the
data set determines the reference maximum magnitude. Vectors in the
dataset with magnitude greater than VHC are not displayed.
.RE
.IP ""
Typically, for direct comparison of the output of a series of plots,
you would set VHC to a negative number, the absolute value of which is
greater than any expected vector magnitude in the series. You can turn
on Vectors statistics reporting using the parameter VST in order to
see if any vectors in the datasets do exceed the maximum magnitude you
have specified. See also the descriptions of the parameters VRM, VRL, DMX,
VLC, and VFR.
.IP "VLC - Vector Low Cutoff Value - Real"
Use this parameter to prevent vectors smaller than the specified
magnitude from appearing in the output plot. VLC also specifies the
reference minimum magnitude that is rendered at the size specified by
the product of VRL and VFR (as a fraction of the viewport width), when
VFR is greater than 0.0. Note that the reference minimum magnitude may
be smaller than the magnitude of any vector in the dataset. The effect
of this parameter varies depending on its value, as follows:
.RS
.IP Value 15
Effect
.IP "< 0.0" 15
The absolute value of VLC unconditionally determines the reference
minimum magnitude. Vectors in the dataset with magnitude less than VLC
do not appear.
.IP "0.0 (default)" 15
The vector with the minimum magnitude in the dataset determines the
reference minimum magnitude.
.IP "> 0.0" 15
The maximum of VLC and the vector with the least magnitude in the
data set determines the reference minimum magnitude. Vectors in the
dataset with magnitude less than VLC do not appear.
.RE
.IP ""
The initialization subroutine, VVINIT, calculates the magnitude of
all the vectors in the vector field, and stores the maximum and
minimum values. You may access these values by retrieving the
read-only parameters, VMX and VMN.  Thus it is possible to remove the
small vectors without prior knowledge of the data domain. The
following code fragment illustrates how the smallest 10% of the
vectors could be removed:
.sp
.in 15
CALL VVINIT(...
.br
CALL VVGETR(\'VMX - Vector Maximum Magnitude\', VMX)
.br
CALL VVGETR(\'VMN - Vector Minimum Magnitude\', VMN)
.br
CALL VVSETR(\'VLC - Vector Low Cutoff Value\', 
.in -1
+     VMN+0.1*(VMX-VMN))
.in +1
CALL VVECTR(...
.sp
.in -15
.IP " "
On the other hand, when creating a series of plots that you would like
to compare directly and you are using VFR to set a minimum realized
size for the vectors, you can ensure that all vectors of a particular
length represent the same magnitude on all the plots by setting both
VHC and VLC to negative values. If you do not actually want to remove
any vectors from the plot, make VLC smaller in absolute value than any
expected magnitude. You can turn on Vectors statistics reporting using
the parameter VST in order to see if any vectors in the datasets
are less the minimum magnitude you have specified. See also the
descriptions of parameters VFR, VRL, VHC, DMN, and VRM.
.IP "VMD - Vector Minimum Distance - Real"
If VMD is set to a value greater than 0.0, it specifies, as a fraction
of the viewport width, a minimum distance between adjacent vectors
arrows in the plot. The distribution of vectors is analyzed and then
vectors are selectively removed in order to ensure that the remaining
vectors are separated by at least the specified distance. The thinning
algorithm requires that you supply Vectors with a work array twice the
size of the VVINIT arguments N and M multiplied together. Use of this
capability adds some processing time to the execution of Vectors. If
VMD is set to a value greater than 0.0 and no work array is provided,
an error condition results. 
.sp
If the data grid is transformed in such a way that
adjacent grid cells become very close in NDC space, as for instance in
many map projections near the poles, you can use this parameter to
reduce the otherwise cluttered appearance of these regions of the
plot. The default value of VMD is 0.0.
.IP "VMN - Minimum Vector Magnitude - Real, Read-Only"
After a call to VVINIT, VMN contains the value of the minimum vector
magnitude in the U and V vector component arrays. Later, after VVECTR
is called, it is modified to contain the magnitude of the smallest
vector actually displayed in the plot. This is the vector with the
smallest magnitude greater than or equal to the value specified by
VLC, the vector low cutoff parameter, (0.0 if VLC has its default
value) that falls within the user coordinate window boundaries. The
value contained in VMN is the same as that reported as the 'Minimum
plotted vector magnitude' when Vectors statistics reporting is
enabled. It may be larger than the reference minimum magnitude
reported by the minimum vector text block if you specify the VLC
parameter as a negative value. VMN is initially set to a value of 0.0.
.IP "VMX - Maximum Vector Magnitude - Real, Read-Only"
After a call to VVINIT, VMX contains the value of the maximum vector
magnitude in the U and V vector component arrays. Later, after VVECTR
is called, it is modified to contain the magnitude of the largest
vector actually displayed in the plot. This is the vector with the
largest magnitude less than or equal to the value specified by VHC,
the vector high cutoff parameter, (the largest floating point value
available on the machine if VHC has its default value, 0.0) that falls
within the user coordinate window boundaries. The value contained in
VMX is the same as that reported as the 'Maximum plotted vector
magnitude' when Vectors statistics reporting is enabled. It may be
smaller than the reference maximum magnitude reported by the maximum
vector text block if you specify the VHC parameter as a negative
value. VMX is initially set to a value of 0.0.
.IP "VPB - Viewport Bottom - Real"
The parameter VPB has an effect only when SET is non-zero, specifying
that Vectors should do the call to SET. It specifies a minimum
boundary value for the bottom edge of the viewport in NDC space, and
is constrained to a value between 0.0 and 1.0. It must be less than
the value of the Viewport Top parameter, VPT. The actual value of the
viewport bottom edge used in the plot may be greater than the value of
VPB, depending on the setting of the Viewport Shape parameter, VPS.
You must initialize Vectors with a call to VVINIT after modifying this
parameter. The default value of VPB is 0.05.
.IP "VPL - Viewport Left - Real"
The parameter VPL has an effect only when SET is non-zero, specifying
that Vectors should do the call to SET. It specifies a minimum
boundary value for the left edge of the viewport in NDC space, and is
constrained to a value between 0.0 and 1.0. It must be less than the
value of the Viewport Right parameter, VPR. The actual value of the
viewport left edge used in the plot may be greater than the value of
VPL, depending on the setting of the Viewport Shape parameter, VPS.
You must initialize Vectors with a call to VVINIT after modifying this
parameter. The default value of VPL is 0.05.
.IP "VPO - Vector Positioning Mode - Integer"
VPO specifies the position of the vector arrow in relation
to the grid point location of the vector component data.
Three settings are available, as follows:
.RS
.IP Value 15
Effect
.IP <0 15
The head of the vector arrow is placed at
the grid point location
.IP "0 (default)" 15
The center of the vector arrow is placed
at the grid point location
.IP >0 15
The tail of the vector arrow is placed at
the grid point location
.RE
.IP "VPR - Viewport Right - Real"
The parameter VPR has an effect only when SET is non-zero, specifying
that Vectors should do the call to SET. It specifies a maximum
boundary value for the right edge of the viewport in NDC space, and is
constrained to a value between 0.0 and 1.0. It must be greater than
the value of the Viewport Left parameter, VPL. The actual value of the
viewport right edge used in the plot may be less than the value of
VPR, depending on the setting of the Viewport Shape parameter, VPS.
You must initialize Vectors with a call to VVINIT after modifying this
parameter. The default value of VPR is 0.95.
.IP "VPS - Viewport Shape - Real"
The parameter VPS has an effect only when SET is non-zero,
specifying that Vectors should do the call to SET; it
specifies the desired viewport shape, as follows:
.RS
.IP Value 15
Effect
.IP <0.0 15
The absolute value of VPS specifies the
shape to use for the viewport., as the
ratio of the viewport width to its height,
.IP 0.0 15
The viewport completely fills the area
defined by the boundaries specifiers, VPL,
VPR, VPB, VPT
.IP ">0.0,<1.0 (0.25, default)" 15
Use R = (XCM-XC1)/(YCN-YC1) as the
viewport shape if MIN(R, 1.0/R) is greater
than VPS. Otherwise determine the shape as
when VPS is equal to 0.0.
.IP ">= 1.0" 15
Use R = (XCM-XC1)/(YCN-YC1) as the
viewport shape if MAX(R, 1.0/R) is less
than VPS. Otherwise make the viewport a
square.
.RE
.IP ""
The viewport, whatever its final shape, is centered in, and made as
large as possible in, the area specified by the parameters VPB, VPL,
VPR, and VPT. You must initialize Vectors with a call to VVINIT after
modifying this parameter. The default value of VPS is 25.
.IP "VPT - Viewport Top - Real"
The parameter VPT has an effect only when SET is non-zero, specifying
that Vectors should do the call to SET. It specifies a maximum
boundary value for the top edge of the viewport in NDC space, and is
constrained to a value between 0.0 and 1.0. It must be greater than
the value of the Viewport Bottom parameter, VPB. The actual value of
the viewport top edge used in the plot may be less than the value of
VPT, depending on the setting of the Viewport Shape parameter, VPS.
You must initialize Vectors with a call to VVINIT after modifying this
parameter. The default value of VPT is 0.95.
.IP "VRL - Vector Reference Length - Real"
Use this parameter to specify the realized length of the reference
magnitude vector as a fraction of the viewport width. Based on
this value a reference length in NDC units is established, from
which the length of all vectors in the plot is derived. The
relationship between magnitude and length also depends on the setting
of the minimum vector magnitude fraction parameter, VFR, but, given
the default value of VFR (0.0), the length of each vector is simply
proportional to its relative magnitude. Note that the arrow size
parameters, AMN and AMX, allow independent control over the
minimum and maximum size of the vector arrowheads.
.sp
Given a reference length, Vectors calculates a maximum length based on
the ratio of the reference magnitude to the larger of the maximum
magnitude in the data set and the reference magnitude itself. This
length is accessible in units of NDC via the read-only parameter,
DMX. If VRL is set less than or equal to 0.0, VVINIT calculates a
default value for DMX, based on the size of a grid box assuming a
linear mapping from grid coordinates to NDC space. The value chosen is
one half the diagonal length of a grid box. By retrieving the value of
DMX and calling GETSET to retrieve the viewport boundaries after the
call to VVINIT, you can make relative adjustments to the vector
length, as shown by the following example, where the maximum vector
length is set to 1.5 times its default value:
.sp
.in 15
CALL VVINIT(...
.br
CALL VVGETR(\'DMX - NDC Maximum Vector Size\', DMX)
.br
CALL GETSET(VL,VR,VB,VT,UL,UR,UB,UT,LL)
.br
VRL = 1.5 * DMX / (VR - VL)
.br
CALL VVSETR(\'VRL - Vector Realized Length\', VRL)
.br
CALL VVECTR(...
.in -15
.IP ""
When VVECTR sees that VRL is greater than 0.0, it will calculate a new
value for DMX. If VRL is never set, the initially calculated value of
DMX is used as the reference length. Do not rely on the internal
parameters used for setting the viewport, VPL, VPR, VPB and VPT to
retrieve information about viewport in lieu of using the GETSET
call. These values are ignored entirely if the SET parameter is zero,
and even if used, the viewport may be adjusted from the specified
values depending on the setting of the viewport shape parameter,
VPS. See also the descriptions of VFR, VRM, and VHC. The default value
of VRL is 0.0.
.IP "VRM - Vector Reference Magnitude - Real"
The introduction of the parameter VRM means that it is now possible to
specify an arbitrary vector magnitude as the reference magnitude
appearing in the "Maximum Vector Text Block" annotation. The reference
magnitude no longer needs to be greater or equal to the largest
magnitude in the data set.  When VRM has a value greater than 0.0, it
specifies the magnitude of the vector arrow drawn at the reference
length. See VRL for an explanation of how the reference length is
determined. If VRM is less than or equal to 0.0, the reference
magnitude is determined by the value of VHC, the vector high cutoff
value. If, in turn, VHC is equal to 0.0 the maximum magnitude in the
vector field data set becomes the reference magnitude. The default
value of VRM is 0.0.
.IP "VST - Vector Statistics Output Flag - Integer"
If VST is set to one, VVECTR writes a summary of its
operation to the default logical output unit, including the
number of vectors plotted, number of vectors rejected,
minimum and maximum vector magnitudes, and if coloring the
vectors according to data in the scalar array, the maximum
and minimum scalar array values encountered. Here is a
sample of the output:
.in 15
.sp
.nf
VVECTR Statistics
.ta 36nR +2n
.br
	Vectors plotted:	906
.br
	Vectors rejected by mapping routine:	0
.br
	Vectors under minimum magnitude:	121
.br
	Vectors over maximum magnitude:	0
.br
	Other zero length vectors:	0
.br
	Rejected special values:	62
.ta 36nR +13nR
.br
	Minimum plotted vector magnitude:	9.94109E-02
.br
	Maximum plotted vector magnitude:	1.96367
.br
	Minimum scalar value:	-1.00000
.br
	Maximum scalar value:	1.00000
.br
.fi
.in -15
.IP "VSV - V Array Special Value - Real"
VSV is the V vector component array special value. It is a value
outside the range of the normal data used to indicate that there is no
valid data for this grid location. When SVF is set to 2 or 3, Vectors
will not draw a vector whose V component has the special value. You
must initialize Vectors with a call to VVINIT after modifying this
parameter. It has a default value of 1.0 E12.

.IP "WBA - Wind Barb Angle - Real"

WBA sets the angle of the wind barb ticks in degrees as
measured clockwise from the vector direction. It also sets the angle
between the hypotenuse of the triangle defining the pennant polygon
and the vector direction. You can render southern hemisphere wind
barbs, which by convention, have their ticks and pennants on the other
side of the shaft, by setting WBA to a negative value. WBA
has an effect only when AST has the value 2.

.IP "WBC - Wind Barb Calm Circle Size - Real"

WBC sets the diameter of the circle used to represent small vector
magnitudes (less than 2.5) as a fraction of the overall wind barb
length (the value of the VRL
parameter). WBC has an effect only when 
AST has the value 2.

.IP "WBD - Wind Barb Distance Between Ticks - Real"

WBD sets the distance between adjacent wind barbs ticks along the wind
barb shaft as a fraction of the overall wind barb length (the value of
the VRL parameter). Half this distance is
used as the spacing between adjacent wind barb pennants. Note that
there is nothing to to prevent ticks and/or pennants from continuing
off the end of the shaft if a vector of high enough magnitude is
encountered. You are responsible for adjusting the parameters
appropriately for the range of magnitudes you need to handle. WBD has
an effect only when AST has the value 2.

.IP "WBS - Wind Barb Scale Factor - Real"

WBS specifies a factor by which magnitudes passed to the wind barb
drawing routines are to be scaled. It can be used to convert vector
data given in other units into the conventional units used with wind
barbs, which is knots. For instance, if the data are in meters per second,
you could set WBS to 1.8974 to create a plot with conventional knot-based
wind barbs. Note that setting WBS does not currently have any effect on
the magnitude values written into the maximum or minimum vector legends.
WBS has an effect only when AST has the value 2.

.IP "WBT - Wind Barb Tick Size - Real"

WBT the length of the wind barb ticks as a fraction of the overall
length of a wind barb (the value of the VRL parameter). The wind barb
length is defined as the length of the wind barb shaft plus the
projection of a full wind barb tick along the axis of the
shaft. Therefore, increasing the value of WBT, for a given value of VRL
has the effect of reducing the length of the shaft itself
somewhat. You may need to increase VRL itself to compensate. WBT
also sets the hypotenuse length of the triangle defining the
pennant polygon. WBT has an effect only when AST has the value 2.

.IP "WDB - Window Bottom - Real"
When VVINIT does the call to SET, the parameter WDB is used to
determine argument number 7, the user Y coordinate at the bottom of
the window. If WDB is not equal to WDT, WDB is used. If WDB is equal
to WDT, but YC1 is not equal to YCN, then YC1 is used. Otherwise, the
value 1.0 is used. You must initialize Vectors with a call to VVINIT
after modifying this parameter. The default value of WDB is 0.0.
.IP "WDL - Window Left - Real"
When VVINIT the call to SET, the parameter WDL is used to determine
argument number 5, the user X coordinate at the left edge of the
window. If WDL is not equal to WDR, WDL is used. If WDL is equal to
WDR, but XC1 is not equal to XCM, then XC1 is used. Otherwise, the
value 1.0 is used. You must initialize Vectors with a call to VVINIT
after modifying this parameter. The default value of WDL is 0.0.
.IP "WDR - Window Right - Real"
When VVINIT does the call to SET, the parameter WDR is used to
determine argument number 6, the user X coordinate at the right edge
of the window. If WDR is not equal to WDL, WDR is used. If WDR is
equal to WDL, but XCM is not equal to XC1, then XCM is used.
Otherwise, the value of the VVINIT input parameter, M, converted to a
real, is used. You must initialize Vectors with a call to VVINIT
after modifying this parameter. The default value of WDR is 0.0.
.IP "WDT - Window Top - Real"
When VVINIT does the call to SET, the parameter WDB is used to
determine argument number 8, the user Y coordinate at the top of the
window. If WDT is not equal to WDB, WDT is used. If WDT is equal to
WDB, but YCN is not equal to YC1 then YCN is used. Otherwise, the
value of the VVINIT input parameter, N, converted to a real, is used.
You must initialize Vectors with a call to VVINIT after modifying this
parameter. The default value of WDT is 0.0.
.IP "XC1 - X Coordinate at Index 1 - Real"
The parameter XC1 specifies the X coordinate value that corresponds to
a value of 1 for the first subscript of the U, V, vector component
arrays as well as for the P scalar data array, if used. Together with
XCM, YC1, and YCN it establishes the mapping from grid coordinate
space to data coordinate space. If XC1 is equal to XCM, 1.0 will be
used. You must initialize Vectors with a call to VVINIT after
modifying this parameter. The default value of XC1 is 0.0.
.IP "XCM - X Coordinate at Index M - Real"
The parameter XCM specifies the X coordinate value that corresponds to
the value of the VVINIT input parameter, M, for the first subscript of
the U and V vector component arrays as well as for the P scalar data
array, if used.  Together with XC1, YC1, and YCN it establishes the
mapping from grid coordinate space to data coordinate space. If XC1 is
equal to XCM, the value of M, converted to a real, will be used. You
must initialize Vectors with a call to VVINIT after modifying this
parameter. The default value of XCM is 0.0.
.IP "XIN - X Axis Array Increment (Grid) - Integer"
XIN controls the step size through first dimensional subscripts of the
U,V vector component arrays and also through the P scalar data array
if it is used. For dense arrays plotted at a small scale, you
could set this parameter to a value greater than one to reduce the
crowding of the vectors and hopefully improve the intelligibility of
the plot. The grid point with subscripts (1,1) is always included in
the plot, so if XIN has a value of three, for example, only grid
points with first dimension subscripts 1, 4, 7... (and so on) will be
plotted. See also YIN. You must initialize Vectors with a call to
VVINIT after modifying this parameter. The default value of XIN is 1.
.IP "YC1 - Y Coordinate at Index 1 - Real"
The parameter YC1 specifies the Y coordinate value that corresponds to
a value of 1 for the first subscript of the U, V, vector component
arrays as well as for the P scalar data array, if used. Together with
YCN, XC1, and XCM it establishes the mapping from grid coordinate
space to data coordinate space. If YC1 is equal to YCN, 1.0 will be
used. You must initialize Vectors with a call to VVINIT after
modifying this parameter. The default value of YC1 is 0.0.
.IP "YCN - Y Coordinate at Index N - Real"
The parameter YCN specifies the Y coordinate value that corresponds to
the value of the VVINIT input parameter, N, for the second subscript
of the U and V vector component arrays as well as the P scalar data
array, if used.  Together with YC1, XC1, and XCM it establishes the
mapping from grid coordinate space to data coordinate space. If YC1 is
equal to YCN, the value of N, converted to a real, will be used. You
must initialize Vectors with a call to VVINIT after modifying this
parameter. The default value of YCN is 0.0.
.IP "YIN - Y Axis Array Increment (Grid) - Integer"
YIN controls the step size through the second dimension subscripts of
the U and V vector component arrays and also through the P scalar data
array if it is used. For dense arrays plotted at a small scale, you
could set this parameter to a value greater than one to reduce the
crowding of the vectors and hopefully improve the intelligibility of
the plot. The grid point with subscripts (1,1) is always included in
the plot, so if YIN has a value of three, for example, only grid
points with second dimension subscripts 1, 4, 7... (and so on) will
be plotted. See also XIN. You must initialize Vectors with a call to
VVINIT after modifying this parameter. The default value of YIN is 1.
.IP "ZFC - Zero Field Text Block Color - Integer"
If ZFC is greater or equal to zero, it specifies the GKS
color index to use to color the Zero Field text block.
Otherwise the Zero Field text block is colored using the
current GKS text color index. The default value of ZFC is -1.
.IP "ZFP - Zero Field Text Block Positioning Mode - Integer"
The ZFP parameter allows you to justify, using any of
the 9 standard justification modes, the Zero Field text
block unit with respect to the position established by the
parameters, ZFX and ZFY The position modes are supported as
follows:
.RS
.IP Mode 15
Justification
.IP -4 15
The lower left corner of the text block is
positioned at ZFX, ZFY.
.IP -3 15
The center of the bottom edge is
positioned at ZFX, ZFY.
.IP -2 15
The lower right corner is positioned at
ZFX, ZFY.
.IP -1 15
The center of the left edge is positioned
at ZFX, ZFY.
.IP "0 (default)" 15
The text block is centered along both axes
at ZFX, ZFY.
.IP 1 15
The center of the right edge is positioned
at ZFX, ZFY.
.IP 2 15
The top left corner is positioned at ZFX,
ZFY.
.IP 3 15
The center of the top edge is positioned
at ZFX, ZFY.
.IP 4 15
The top right corner is positioned at ZFX,
ZFY.
.RE
.IP "ZFS - Zero Field Text Block Character Size - Real"
ZFS specifies the size of the characters used in the Zero
Field graphics text block as a fraction of the viewport
width. The default value is 0.033.
.IP "ZFT - Zero Field Text String - Character* 36"
Use ZFT to modify the text of the Zero Field text block.
The Zero Field text block may appear whenever the U and V
vector component arrays contain data such that all the grid
points otherwise eligible for plotting contain zero
magnitude vectors. Currently the string length is limited
to 36 characters. Set ZFT to a single space (\' \') to
prevent the text from being displayed. The default value
for the text is \'Zero Field\'.
.IP "ZFX - Zero Field Text Block X Coordinate - Real"
ZFX establishes the X coordinate of the Zero Field graphics
text block as a fraction of the viewport width. Values less
than 0.0 or greater than 1.0 are permissible and
respectively represent regions to the left or right of the
viewport. The actual position of the block relative to ZFX
depends on the value assigned to the Zero Field Positioning
Mode parameter, ZFP. The default value is 0.5.
.IP "ZFY - Zero Field Text Block Y Coordinate - Real"
ZFY establishes the Y coordinate of the minimum vector
graphics text block as a fraction of the viewport height.
Values less than 0.0 or greater than 1.0 are permissible
and respectively represent regions below and above the
viewport. The actual position of the block relative to ZFY
depends on the value assigned to the Zero Field Positioning
Mode parameter, ZFP. The default value is 0.5.
.SH SEE ALSO
Online:
vectors,
vvectr,
vvgetc,
vvgeti,
vvgetr,
vvinit,
vvrset,
vvsetc,
vvseti,
vvsetr.
vvudmv,
vvumxy,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
