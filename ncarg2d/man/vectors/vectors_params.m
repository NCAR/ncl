'\" t
.TH Vectors_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
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
.IP "AMN - Arrow Head Minimum Size - Real"
Specifies a minimum length for the two lines representing
the point of the vector arrow head, as a fraction of the
viewport width. Normally the arrow head size is scaled
proportionally to the length of the vector. This parameter
allows the user to ensure that the arrow head will remain
recognizable even for very short vectors. The default value
is 0.01.
.IP "AMX - Arrow Head Maximum Size - Real"
Specifies a maximum length for the two lines representing
the point of the vector arrow head, as a fraction of the
viewport width. Normally the arrow head is scaled
proportionally to the length of the vector. This parameter
allows the user to ensure that the arrow heads do not
become excessively large for high magnitude vectors. The
default value is 0.2.
.IP "CLR - Array of GKS Color Indices - Integer Array"
This parameter represents an array containing the GKS color
index to use for coloring the vector when the scalar
quantity is less than or equal to the threshold value with
the same index in the TVL threshold value array. In order
to access a particular element of the CLR array, the user
must first set the value of PAI, the parameter array index
parameter, to the value of the array element\'s index. All
elements of the array are set to one initially. Note that
the Vectors utility makes no calls to set the GKS color
representation (GSCR), nor ever modifies the contents of
the CLR array; the user is therefore responsible for
creating a suitably graduated color palette and assigning
the color index values into the CLR array, prior to calling
VVECTR. Typically, assuming the desired RGB values have
been previously stored in a 2 dimensional 3 x n array
called RGB, one would loop through the calls to set up the
color representation and color index as in the following
example that sets up a fourteen color palette:
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
See the description of CTV, NLV, and TVL for details on
configuring the vector coloring scheme.
.IP "CPM - Compatibility Mode - Integer"
Controls the degree of compatibility between pre-Version
3.2 capabilities of the Vectors utility and later versions.
The nine settings provided allow the user to independently
control three behaviors:
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
When CPM is set to its default value, zero, the Vectors
utility\'s behavior varies depending on whether the utility
has been accessed through one of the pre-Version 3.2 entry
points (VELVCT, VELVEC, and EZVEC), or through the VVINIT/VVECTR
interface. Otherwise, positive values result in
invocation of the pre-Version 3.2 mapping routines (FX, FY,
MXF, and MYF) for the conversion from data to user
coordinates. Negative values cause VVMPXY and perhaps
VVUMXY to be used instead. When using the pre-Version 3.2
interface, odd values of CPM cause the data values in the
VELDAT block data subroutine to override corresponding
values initialized in the Version 3.2 VVDATA block data
subroutine, or set by the user calling VVSETx routines.
Values of CPM with absolute value greater than two cause
some of the input parameters to VELVEC and VELVCT to be
ignored. These include FLO, HI, NSET, ISPV, SPV and (for
VELVCT only) LENGTH.
.sp
Here is a table of the nine settings of CPM and their
effect on the operation of the Vectors utility:
.sp
.TS
tab(/);
l l l l.
Value/Use FX,FY,etc/Use VELDAT data/Use input params
-----/-------------/---------------/----------------
-4/no/no/no
-3/no/yes/no
-2/no/no/yes
-1/no/yes/yes
0/old - yes;new - no (*)/yes/yes
1/yes/yes/yes
2/yes/no/yes
3/yes/yes/no
4/yes/no/no
.TE
.sp
(*) Old means EZVEC, VELVEC, VELVCT entry point; new,
VVINIT/VVECTR. Only the first column affects the behavior
of the VVINIT/VVECTR interface.
.IP "CTV - Color Threshold Value Control - Integer"
In conjunction with NLV, this parameter controls vector
coloring and the setting of threshold values. The vectors
may be colored based either on the contents of the scalar
array, P, or else based on the magnitude of each vector
compared to the maximum and minimum magnitudes in the
field. A table of supported options follows:
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
.IP "DMN - NDC Minimum Vector Size - Real, Read-Only"
This parameter is a read-only parameter that has a useful
value only following a call to VVECTR (directly or through
the compatibility version of VELVCT). It may be retrieved
in order to determine the length in NDC space of the
smallest vector actually drawn (in other words, the
smallest vector within the boundary of the user coordinate
space that is greater than or equal in magnitude to the
value of the VLC parameter). It is initially set to a value
of 0.0.
.IP "DMX - NDC Maximum Vector Size - Real, Read-Only"
Unlike DMN this read-only parameter has a potentially
useful value after the call to VVINIT and before the call
to VVECTR. However, the value it reports may be different
before and after the call to VVECTR. Before the call to
VVECTR it contains the length in NDC space that would be
used to render the maximum size vector assuming the 
user-settable parameter, VRL is set to its default value of 0.0.
After the call to VVECTR it contains the NDC length used to
render the largest vector actually drawn (in other words,
the largest vector within the boundary of the user
coordinate space that is less than or equal in magnitude to
the value of the VHC parameter). See the section on the VRL
parameter for information on using the value of DMX after
the VVINIT call in order to proportionally adjust the
lengths of all the vectors in the plot. It is initially set
to a value of 0.0.
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
If set to zero, labels representing the vector magnitude
are not drawn next to each vector in the field plot.
Otherwise they are drawn. The default value is zero. The
vector labels are primarily intended as a debugging aid,
since when drawn small enough to avoid excessive overlap,
they are usually to small to be readable without
magnification on a typical vector field plot. For this
reason, as well as for efficiency, unlike the other
graphical text elements supported by the Vectors utility,
the vector labels are rendered using low quality text.
.IP "LBS - Vector Label Character Size - Real"
This parameter specifies the size of the characters used
for the vector magnitude labels as a fraction of the
viewport width. The default value is 0.007.
.IP "LWD - Vector Linewidth - Real"
LWD controls the linewidth used to draw the vectors. Note
that since the linewidth in NCAR Graphics is always
calculated relative to a unit linewidth that is dependent
on the output device, the linewidth value may need to be
adjusted depending on the output conditions to obtain a
pleasing plot. Currently linewidth applies equally to the
body of the vector and the arrowhead. Overly thick lines
may cause the arrow heads to appear smudged. The default is
0.0, specifying a device-dependent minimum linewidth.
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
If MAP has any other value, the mapping subroutine, VVMPXY,
passes its parameters on to the user-modifiable subroutine,
VVUMXY. The default version of VVUMXY simply performs an
identity transformation, without considering the value of
the TRT parameter. The effect is the same as if one were to
set both MAP and TRT to zero. Note that, while not actually
prohibited in the Vectors utility, the user is advised not
to use negative integers for user-defined mappings, since a
negative value results in an inverse transformation in the
Conpack utility. Inverse transformations are not needed in
Vectors but an inconsistent use of negative mapping codes
would likely be confusing in the long run.
.sp
For all the predefined mappings, the linear relationship
between the grid array indices and the data coordinate
system is established using the four parameters, XC1, XCM,
YC1, and YCN. The X parameters define a mapping for the
first and last indices of the first dimension of the data
arrays, and the Y parameters do the same for the second
dimension. If MAP is set to a value of one, the user needs
to be careful to ensure that the SET parameter is given a
value of zero, since the Ezmap routines call the SET
routine to define the user to NDC mapping. Otherwise, the
user may choose whether or not to issue a SET call prior to
calling VVINIT, modifying the value of SET as required.
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
.IP >= 0" 15
The value of MNC is used as the color
index for both the text and the vector
arrow
.RE
.IP "MNP - Minimum Vector Text Block Positioning Mode - Integer"
The minimum vector graphics text block consists of a 
user-definable text string centered underneath an arrow drawn
horizontally at the same size the minimum magnitude vector
in the plot is rendered. Centered directly above the arrow
is a numeric string in exponential format that represents
the magnitude of this vector. The user may position this
text block relative to the current viewport as a unit (and
only as a unit). The positioning mode parameter allows the
user to justify, using any of the 9 standard justification
modes, the text block unit with respect to the position
established by the parameters, MNX and MNY The position
modes are supported as follows:
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
.IP "MNS - Minimum Vector Text Block Character Size - Real"
MNS specifies the size of the characters used in the
minimum vector graphics text block as a fraction of the
viewport width. The default value is 0.0075.
.IP "MNT - Minimum Vector Text String - Character* 36"
Use MNT to modify the text appearing below the vector in
the minimum vector graphics text block. Currently the
string length is limited to 36 characters. Set MNT to a
single space (\' \') to remove the text block, including the
vector arrow and the numeric magnitude string, from the
plot. The default value is \'Minimum Vector\'
.IP "MNX - Minimum Vector Text Block X Coordinate - Real"
MNX establishes the X coordinate of the minimum vector
graphics text block as a fraction of the viewport width.
Values less than 0.0 or greater than 1.0 are permissible
and respectively represent regions to the left or right of
the viewport. The actual position of the block relative to
MNX depends on the value assigned to MNP. The default value
is 0.475.
.IP "MNY - Minimum Vector Text Block Y Coordinate - Real"
MNY establishes the Y coordinate of the minimum vector
graphics text block as a fraction of the viewport height.
Values less than 0.0 or greater than 1.0 are permissible
and respectively represent regions below or above the
viewport. The actual position of the block relative to MNY
depends on the value assigned to MNP. The default value
is -0.01.
.IP "MSK - Mask To Area Map Flag - Integer"
Use this parameter to control masking of vectors to an
existing area map created by routines in the Areas utility.
When MSK is greater than 0, masking is enabled and an the
area map must be set up prior to the call to VVECTR. The
area map array and, in addition, the name of a 
user-definable masked drawing routine, must be passed as input
parameters to VVECTR. Various values of the MSK parameter
have the following effects:
.RS
.IP Value 15
Effect
.IP "<= 0 (default)" 15
No masking of vectors.
.IP 1 15
The subroutine ARDRLN is called internally
to decompose the vectors into segments
contained entirely within a single area.
ARDRLN calls the user-definable masked
drawing subroutine.
.IP >1 15
Low precision masking. ARGTAI is called
internally to get the area identifiers for
the vector base position point. Then the
user-definable masked drawing subroutine
is called to draw the vector. Vectors with
nearby base points may encroach into the
intended mask area.
.RE
.IP ""
See the man page vvudmv 
for further explanation of
masked drawing of vectors
.IP "MXC - Maximum Vector Text Block Color - Integer"
MXC specifies the color of the maximum vector graphical
text output block as follows:
.RS
.IP Value 15
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
.IP "MXP - Maximum Vector Text Block Positioning Mode - Integer"
The maximum vector graphics text block consists of a user-definable
text string centered underneath a horizontal
arrow sized the same as the maximum magnitude vector in the
plot. Centered directly above the arrow is a numeric string
in exponential format that represents the magnitude of this
vector. The user may position this text block relative to
the current viewport as a unit (and only as a unit). The
positioning mode parameter allows the user to justify,
using any of the 9 standard justification modes, the text
block unit with respect to the position established by the
parameters, MXX and MXY. The position modes are supported
as follows:
.RS
.IP Mode 15
Justification
.IP -4 15
The lower left corner of the text block is
positioned at MXX, MXY.
.IP -3 15
The center of the bottom edge is
positioned at MXX, MXY.
.IP -2 15
The lower right corner is positioned at
MXX, MXY.
.IP -1 15
The center of the left edge is positioned
at MXX, MXY.
.IP 0 15
The text block is centered along both axes
at MXX, MXY.
.IP 1 15
The center of the right edge is positioned
at MXX, MXY.
.IP 2 (default) 15
The top left corner is positioned at MXX,
MXY.
.IP 3 15
The center of the top edge is positioned
at MXX, MXY.
.IP 4 15
The top right corner is positioned at MXX,
MXY.
.RE
.IP "MXS - Maximum Vector Text Block Character Size - Real"
MXS specifies the size of the characters used in the
maximum vector graphics text block as a fraction of the
viewport width. The default value is 0.0075.
.IP "MXT - Maximum Vector Text String - Character* 36"
Use MXT to modify the text appearing below the vector in
the maximum vector graphics text block. Currently the
string length is limited to 36 characters. Set MXT to a
single space (\' \') to completely remove the text block,
including the vector arrow and the numeric magnitude
string, from the plot. The default value is '\Maximum
Vector\'.
.IP "MXX - Maximum Vector Text Block X Coordinate - Real"
MXX establishes the X coordinate of the maximum vector
graphics text block as a fraction of the viewport width.
Values less than 0.0 or greater than 1.0 are permissible
and respectively represent regions below or above of the
viewport. The actual position of the block relative to MXX
depends on the value assigned to MXP. The default value
is 0.525.
.IP "MXY - Maximum Vector Text Block Y Coordinate - Real"
MXY establishes the Y coordinate of the maximum vector
graphics text block as a fraction of the viewport width.
Values less than 0.0 or greater than 1.0 are permissible
and respectively represent regions below or above the
viewport. The actual position of the block relative to MXY
depends on the value assigned to MXP. The default value
is -0.01.
.IP "NLV - Number of Colors Levels - Integer"
NLV specifies the number of color levels to use when
coloring the vectors according to data in a scalar array or
by vector magnitude. If CTV is greater than 0, the user
must set up NLV elements of the color index array CLR using
as values GKS color indices that need to be set up using
the GKS call, GSCR, prior to calling VVECTR. If CTV is less
than 0, in addition to setting up the CLR array, the user
is also responsible for setting the elements of the
threshold values array, TVL to appropriate values.
Currently NLV is constrained to a maximum value of 64. The
default value of NLV is 0, specifying that vectors are
colored according to the value of the GKS polyline color
index currently in effect, regardless of the value of CTV.
.IP "PAI - Parameter Array Index - Integer"
The value of PAI must be set before calling VVGETC, VVGETI,
VVGETR, VVSETC, VVSETI, or VVSETR to access any parameter
which is an array; it acts as a subscript to identify the
intended array element. For example, to set the 10th color
threshold array element to 7, use code like this:
.sp
.in 15
CALL VVSETI (\'PAI - PARAMETER ARRAY INDEX\',10)
.br
CALL VVSETI (\'CLR - Color Index\',7)
.in -15
.IP ""
The default value of PAI is one.
.IP "PLR - Polar Input Mode - Integer"
When PLR is greater than zero, the vector component arrays
are considered to contain the field data in polar
coordinate form: the U array is treated as containing the
vector magnitude and the V array as containing the vector
angle. Be careful not to confuse the PLR parameter with the
MAP parameter set to polar coordinate mode (2). The MAP
parameter relates to the location of the vector, not its
value. Here is a table of values for PLR:
.RS
.IP Value 15
Meaning
.IP "0 (default)" 15
U and V arrays contain data in cartesian
component form.
.IP 1 15
U array contains vector magnitudes; V
array contains vector angles in degrees.
.IP 2 15
U array contain vector magnitudes; V array
contains vector angles in radians.
.RE
.IP "PMN - Minimum Scalar Array Value - Real, Read-Only"
The user may retrieve the value specified by PMN at any
time after a call to VVINIT. It will contain a copy of the
minimum value encountered in the scalar data array. If no
scalar data array has been passed into VVINIT it will have
a value of 0.0.
.IP "PMX - Maximum Scalar Array Value - Real"
The user may retrieve the value specified by PMX at any
time after a call to VVINIT. It contains a copy of the
maximum value encountered in the scalar data array.  If no
scalar data array has been passed into VVINIT it will have
a value of 0.0.
.IP "PSV - P Array Special Value - Real"
Use PSV to indicate the special value that flags an unknown
data value in the P scalar data array. This value will not
be considered in the determination of the data set maximum
and minimum values. Also, depending on the setting of the
SPC parameter, the vector may be specially colored to flag
the unknown data point, or even eliminated from the plot.
.IP "SET - SET Call Flag - Integer"
Give SET the value 0 to inhibit the SET call otherwise
performed by VVINIT. Arguments 5-8 of a SET call done by
the user must be consistent with the ranges of the user
coordinates expected by Vectors. This is determined by the
mapping from grid to data coordinates as specified by the
values of the parameters XC1, XCM, YC1, YCN, and also by
the mapping from data to user coordinates established by
the MAP parameter. 
The default value of SET is 1.
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
.IP "SVF - Special Value Flag - Integer"
The special value flag controls special value processing
for the U and V vector component data arrays. Special
values may appear in either the U or V array or in both of
them. Five different options are available (although the
usefulness of some of the choices is debatable):
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
The U and V special values are defined by setting
parameters USV and VSV. Generally, the Vectors utility can
be expected to ignore special values when it determines the
maximum and minimum vector magnitudes, although if SVF is
set to a value of four, it is possible that a special value
occurring in only one of the arrays could be interpreted as
an actual vector component value.
.IP "TRT - Transformation Type - Integer"
As currently implemented, TRT further qualifies the mapping
transformation specified by the MAP parameters, as follows:
.RS
.IP Value 15
Effect
.IP -1 15
Direction, magnitude, and location are all
transformed. This option is not currently
supported by any of the pre-defined
coordinate system mappings.
.IP 0 15
Only location is transformed
.IP "1 (default)" 15
Direction and location are transformed
.RE
.IP ""
This parameter allows the user to distinguish between a
system that provides a mapping of location only into an
essentially cartesian space, and one in which the space
itself mapped. To understand the difference, using polar
coordinates as an example, imagine a set of wind speed
monitoring units located on a radial grid around some
central point such as an airport control tower. Each unit\'s
position is defined in terms of its distance from the tower
and its angular direction from due east. However, the data
collected by each monitoring unit is represented as
conventional eastward and northward wind components.
Assuming the towers\'s location is at a moderate latitude,
and the monitoring units are reasonably \'local\', this is an
example of mapping a radially defined location into a
nearly cartesian space (i.e. the eastward components taken
alone all point in a single direction on the plot,
outlining a series of parallel straight lines). One would
set MAP to two (for the polar transformation) and TRT to
zero to model this data on a plot generated by the Vectors
utility.
.sp
On the other hand, picture a set of wind data, again given
as eastward and northward wind components, but this time
the center of the polar map is actually the south pole. In
this case, the eastward components do not point in a single
direction; instead they outline a series of circles around
the pole. This is a space mapping transformation: one would
again set MAP to two, but TRT would be set to one to
transform both direction and location.
.sp
Changing the setting of this parameter affects the end
results only when a non-uniform non-linear mapping occurs
at some point in the transformation pipeline. For this
discussion a uniform linear transformation is defined as
one which satisfies the following equations:
.sp
.in 15
Xout = Xoffset + Sconstant * Xin
.br
Yout = Yoffset + Sconstant * Yin
.in -15
.IP ""
If the Sconstant is not the same for Y and X then the
mapping is non-uniform.
.sp
This option is currently implemented only for the 
pre-defined MAP parameter codes, 0 and 2, the identity mapping
and the polar coordinate mapping. However, it operates on a
different stage of the transformation pipeline in each
case. The polar mapping is non-linear from data to user
coordinates. The identity mapping, even though necessarily
linear over the data to user space mapping, can have a 
non-uniform mapping from user to NDC space, depending on the
values given to the input parameters of the SET call. This
will be the case whenever the LL input parameter is other
than one, or when LL equals one, but the viewport and the
user coordinate boundaries do not have the same aspect
ratio. Thus for a MAP value of 2, TRT affects the mapping
between data and user space, whereas for MAP set to 0, TRT
influences the mapping between user and NDC space.
.IP "TVL - Array of Threshold Values - Real Array"
TVL holds an array of threshold values used to determine
the color used for coloring the vectors, when either of the
coloring by scalar array or vector magnitude options is
turned on (CTV set to a non-zero value). For each vector
the TVL array is searched for the largest value less than
or equal to magnitude associated with the vector. The array
subscript of this element is used as an index into the CLR
array. The vector is colored using the color specified by
the GKS color index stored in the element of the CLR array
with the same subscript. Note that Vectors assumes that the
threshold values are monotonically increasing.
.sp
When CTV is less than 0, the user is responsible for
assigning appropriate values to the elements of TVL. To do
this, the PAI parameter must be given the value of each
threshold level index, then the threshold parameter set
call issued with the appropriate threshold value. Assuming
the desired values have previously been stored in a array
named TVALS, one could assign the threshold values for a
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
(Maximum Data Value - Minimum Data Value) / NLV
.in -15
.IP ""
where the data values are either from the scalar data array
or are the magnitudes of the vectors in the vector
component arrays. The first value is equal to the minimum
value plus the expression; the final value (indexed by the
value of NLV) is equal to the maximum value. If a value
greater than the greatest value in the TVL array is
encountered while choosing the color for a vector, it is
given the color associated with the greatest value in the
array.
.IP "USV - U Array Special Value - Real"
USV is the U vector component array special value. It is a
value outside the range of the normal data used to indicate
that there is no valid data for this grid location. When
SVF is set to 1 or 3, Vectors will not draw a vector whose
U component has the special value. It has a default value
of 1.0 E12.
.IP "VFR - Minimum Vector Fractional Length - Real"
This parameter allows the user to adjust the realized size
of the minimum magnitude vector relative to the maximum
magnitude vector in order to improve the appearance or
perhaps the information content of the plot. It is
specified as a value between 0.0 and 1.0, with 0.0
representing an unmodified linear scaling of the realized
vector length, in proportion to magnitude, and 1.0
specifying that the smallest vector be represented at 1.0
times the length of the largest vector, resulting in all
vectors, regardless of magnitude, having the same length on
the plot. A value of 0.5 means that the smallest magnitude
vector appears half as long as the largest magnitude
vector; intermediate sizes are proportionally scaled to
lengths between these extremes. Where there is a wide
variation in magnitude within the vector field, this
parameter can be used to increase the size of the smallest
vectors to a usefully visible level. Where the variation is
small, the parameter can be used to exaggerate the
differences that do exist. The default value is 0.0.
.IP "VHC - Vector High Cutoff Value - Real"
Use this parameter to prevent vectors larger than the
specified size from appearing in the output plot. The
initialization subroutine, VVINIT, calculates the magnitude
of all the vectors in the vector field, and stores the
maximum and minimum values. The user may access these
values by retrieving the read-only parameters, VMX and VMN.
Thus it is possible to remove the large vectors without any
advance knowledge of the magnitude range of the data. The
following code fragment illustrates how the largest 10% of
the vectors could be removed:
.sp
.in 15
CALL VVINIT(...
.br
CALL VVGETR(\'VMX - Vector Maximum Magnitude\', VMX)
.br
CALL VVGETR(\'VMN - Vector Minimum Magnitude\', VMN)
.br
CALL VVSETR(\'VHC - Vector High Cutoff Value\', VMX-0.1*(VMX-VMN))
.br
CALL VVECTR(...
.in -15
.IP "VLC - Vector Low Cutoff Value - Real"
Use this parameter to prevent vectors smaller than the
specified size from appearing in the output plot. The
initialization subroutine, VVINIT, calculates the magnitude
of all the vectors in the vector field, and stores the
maximum and minimum values. The user may access these
values by retrieving the read-only parameters, VMX and VMN.
Thus it is possible to remove the small vectors without
prior knowledge of the data domain. The following code
fragment illustrates how the smallest 10% of the vectors
could be removed:
.sp
.in 15
CALL VVINIT(...
.br
CALL VVGETR(\'VMX - Vector Maximum Magnitude\', VMX)
.br
CALL VVGETR(\'VMN - Vector Minimum Magnitude\', VMN)
.br
CALL VVSETR(\'VLC - Vector Low Cutoff Value\', VMN+0.1*(VMX-
VMN))
.br
CALL VVECTR(...
.sp
.in -15
.IP "VRL - Maximum Vector Realized Length - Real"
This parameter allows the user to specify the realized
length of the maximum magnitude vector as a fraction of the
viewport width. Based on this value a reference unit length
in NDC units is established, from which the length of all
vectors in the plot is derived. The actual relationship
also depends on the setting of the Minimum Vector Magnitude
Fraction parameter, VFR, but, given the default value of
VFR (0.0), the length of each vector is simply proportional
to its relative magnitude. Note that the arrow size
parameters, AMN and AMX, allow the user independent control
over the minimum and maximum size of the vector arrowheads.
.sp
The NDC reference unit length is accessible to the user,
via the read-only parameter, DMX. If VRL is less than or
equal to 0.0, VVINIT calculates a default value for DMX,
based on the size of a grid box assuming a linear mapping
from grid coordinates to NDC space. The value chosen is one
half the diagonal length of a grid box. By retrieving the
value of DMX and calling GETSET to retrieve the viewport
boundaries after the call to VVINIT, the user can make
relative adjustments to the vector length, as shown by the
following example, where the maximum vector length is set
to 1.5 times its default value:
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
When VVECTR sees that VRL is greater than 0.0, it will
calculate a new value for DMX. Do not rely on the internal
parameters used for setting the viewport, VPL, VPR, VPB and
VPT to retrieve information about viewport in lieu of using
the GETSET call. These values are ignored entirely if the
SET parameter is zero, and even if used, the viewport may
be adjusted from the specified values depending on the
setting of the Viewport Shape parameter, VPS. The default
value of VRL is 0.0.
.IP "VMN - Minimum Vector Magnitude - Real, Read-Only"
After a call to VVINIT, VMN contains the value of the
minimum vector magnitude in the U and V vector component
arrays. Later, after VVECTR is called, it is modified to
contain the magnitude of the smallest vector actually
displayed in the plot. This is the vector with the smallest
magnitude greater than or equal to the value specified by
VLC, the Vector Low Cutoff parameter, (0.0 if VLC has its
default value) that falls within the user coordinate window
boundaries. When the minimum vector text block display is
enabled, the value contained in VMN after the VVECTR call
is rendered as a graphical text item above the
representation of the minimum size vector. VMN is initially
set to a value of 0.0.
.IP "VMX - Maximum Vector Magnitude - Real, Read-Only"
After a call to VVINIT, VMX contains the value of the
maximum vector magnitude in the U and V vector component
arrays. Later, after VVECTR is called, it is modified to
contain the magnitude of the largest vector actually
displayed in the plot. This is the vector with the largest
magnitude less than or equal to the value specified by VHC,
the Vector High Cutoff parameter, (the largest floating
point value available on the machine if VHC has its default
value) that falls within the user coordinate window
boundaries. When the maximum vector text block display is
enabled, the value contained in VMX after the VVECTR call
is rendered as a graphical text item above the
representation of the maximum size vector. VMX is initially
set to a value of 0.0.
.IP "VPB - Viewport Bottom - Real"
The parameter VPB has an effect only when SET is non-zero,
specifying that Vectors should do the call to SET. It
specifies a minimum boundary value for the bottom edge of
the viewport in NDC space, and is constrained to a value
between 0.0 and 1.0. It must be less than the value of the
Viewport Top parameter, VPT. The actual value of the
viewport bottom edge used in the plot may be greater than
the value of VPB, depending on the setting of the Viewport
Shape parameter, VPS. The default value of VPB is 0.05.
.IP "VPL - Viewport Left - Real"
The parameter VPL has an effect only when SET is non-zero,
specifying that Vectors should do the call to SET. It
specifies a minimum boundary value for the left edge of the
viewport in NDC space, and is constrained to a value
between 0.0 and 1.0. It must be less than the value of the
Viewport Right parameter, VPR. The actual value of the
viewport left edge used in the plot may be greater than the
value of VPL, depending on the setting of the Viewport
Shape parameter, VPS. The default value of VPL is 0.05.
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
The parameter VPR has an effect only when SET is non-zero,
specifying that Vectors should do the call to SET. It
specifies a maximum boundary value for the right edge of
the viewport in NDC space, and is constrained to a value
between 0.0 and 1.0. It must be greater than the value of
the Viewport Left parameter, VPL. The actual value of the
viewport right edge used in the plot may be less than the
value of VPR, depending on the setting of the Viewport
Shape parameter, VPS. The default value of VPR is 0.95.
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
The viewport, whatever its final shape, is centered in, and
made as large as possible in, the area specified by the
parameters VPB, VPL, VPR, and VPT. The default value of VPS
is 25.
.IP "VPT - Viewport Top - Real"
The parameter VPT has an effect only when SET is non-zero,
specifying that Vectors should do the call to SET. It
specifies a maximum boundary value for the top edge of the
viewport in NDC space, and is constrained to a value
between 0.0 and 1.0. It must be greater than the value of
the Viewport Bottom parameter, VPB. The actual value of the
viewport top edge used in the plot may be less than the
value of VPT, depending on the setting of the Viewport
Shape parameter, VPS. The default value of VPT is 0.95.
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
.br
                        Vectors plotted: 906
.br
    Vectors rejected by mapping routine: 0
.br
        Vectors under minimum magnitude: 121
.br
         Vectors over maximum magnitude: 0
.br
              Other zero length vectors: 0
.br
                Rejected special values: 62
.br
       Minimum plotted vector magnitude: 9.94109E-02
.br
       Maximum plotted vector magnitude: 1.96367
.br
                   Minimum scalar value: -1.00000
.br
                   Maximum scalar value: 1.00000
.br
.fi
.in -15
.IP "VSV - V Array Special Value - Real"
VSV is the V vector component array special value. It is a
value outside the range of the normal data used to indicate
that there is no valid data for this grid location. When
SVF is set to 2 or 3, Vectors will not draw a vector whose
V component has the special value. It has a default value
of 1.0 E12.
.IP "WDB - Window Bottom - Real"
When VVINIT does the call to SET, the parameter WDB is used
to determine argument number 7, the user Y coordinate at
the bottom of the window. If WDB is not equal to WDT, WDB
is used. If WDB is equal to WDT, but YC1 is not equal to
YCN, then YC1 is used. Otherwise, the value 1.0 is used.
The default value of WDB is 0.0.
.IP "WDL - Window Left - Real"
When VVINIT the call to SET, the parameter WDL is used to
determine argument number 5, the user X coordinate at the
left edge of the window. If WDL is not equal to WDR, WDL is
used. If WDL is equal to WDR, but XC1 is not equal to XCM,
then XC1 is used. Otherwise, the value 1.0 is used. The
default value of WDL is 0.0.
.IP "WDR - Window Right - Real"
When VVINIT does the call to SET, the parameter WDR is used
to determine argument number 6, the user X coordinate at
the right edge of the window. If WDR is not equal to WDL,
WDR is used. If WDR is equal to WDL, but XCM is not equal
to XC1, then XCM is used. Otherwise, the value of the
VVINIT input parameter, M, converted to a real, is used.
The default value of WDR is 0.0.
.IP "WDT - Window Top - Real"
When VVINIT does the call to SET, the parameter WDB is used
to determine argument number 8, the user Y coordinate at
the top of the window. If WDT is not equal to WDB, WDT is
used. If WDT is equal to WDB, but YCN is not equal to YC1
then YCN is used. Otherwise, the value of the VVINIT input
parameter, N, converted to a real, is used. The default
value of WDT is 0.0.
.IP "XC1 - X Coordinate at Index 1 - Real"
The parameter XC1 specifies the X coordinate value that
corresponds to a value of 1 for the first subscript of the
U, V, vector component arrays as well as for the P scalar
data array, if used. Together with XCM, YC1, and YCN it
establishes the mapping from grid coordinate space to data
coordinate space. If XC1 is equal to XCM, 1 will be used.
The default value of XC1 is 0.0.
.IP "XCM - X Coordinate at Index M - Real"
The parameter XCM specifies the X coordinate value that
corresponds to the value of the VVINIT input parameter, M,
for the first subscript of the U and V vector component
arrays as well as for the P scalar data array, if used.
Together with XC1, YC1, and YCN it establishes the mapping
from grid coordinate space to data coordinate space. If XC1
is equal to XCM, the value of M, converted to a real, will
be used. The default value of XCM is 0.0.
.IP "XIN - X Axis Array Increment (Grid) - Integer"
XIN controls the step size through first dimensional
subscripts of the U,V vector component arrays and also
through the P scalar data array if it is used. For dense
arrays plotted at a small scale, the user could set this
parameter to a value greater than one to reduce the
crowding of the vectors and hopefully improve the
intelligibility of the plot. The grid point with subscripts
(1,1) is always included in the plot, so if XIN had a value
of three, for example, only grid points with first
dimension subscripts 1, 4, 7... (and so on) would be
plotted. See also YIN. The default value of XIN is 1.
.IP "YC1 - Y Coordinate at Index 1 - Real"
The parameter YC1 specifies the Y coordinate value that
corresponds to a value of 1 for the first subscript of the
U, V, vector component arrays as well as for the P scalar
data array, if used. Together with YCN, XC1, and XCM it
establishes the mapping from grid coordinate space to data
coordinate space. If YC1 is equal to YCN, 1 will be used.
The default value of YC1 is 0.0.
.IP "YCN - Y Coordinate at Index N - Real"
The parameter YCN specifies the Y coordinate value that
corresponds to the value of the VVINIT input parameter, N,
for the second subscript of the U and V vector component
arrays as well as the P scalar data array, if used.
Together with YC1, XC1, and XCM it establishes the mapping
from grid coordinate space to data coordinate space. If XC1
is equal to XCM, the value of N, converted to a real, will
be used. The default value of YCN is 0.0.
.IP "YIN - Y Axis Array Increment (Grid) - Integer"
YIN controls the step size through the second dimension
subscripts of the U and V vector component arrays and also
through the P scalar data array if it is used. For dense
arrays plotted at a small scale, the user could set this
parameter to a value greater than one to reduce the
crowding of the vectors and hopefully improve the
intelligibility of the plot. The grid point with subscripts
(1,1) is always included in the plot, so if YIN had a value
of three, for example, only grid points with second
dimension subscripts 1, 4, 7... (and so on) would be
plotted. See also XIN. The default value of YIN is 1.
.IP "ZFC - Zero Field Text Block Color - Integer"
If ZFC is greater or equal to zero, it specifies the GKS
color index to use to color the Zero Field text block.
Otherwise the Zero Field text block is colored using the
current GKS text color index. The default value of ZFC is -1.
.IP "ZFP - Zero Field Text Block Positioning Mode - Integer"
The ZPF parameter allows the user to justify, using any of
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
vvgetc,
vvgeti,
vvgetr,
vvrset,
vvsetc,
vvseti,
vvsetr.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
