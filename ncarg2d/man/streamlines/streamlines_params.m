'\" t
.TH Streamlines_params 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Streamlines_params - This document briefly describes all Streamlines
internal parameters.
.SH DESCRIPTION 
Parameter descriptions follow, in alphabetical order. Each
description begins with a line giving the three-character
mnemonic name of the parameter, the phrase for which the
mnemonic stands, and the intrinsic type of the parameter.
.IP "AGD - Arrow Placement Grid Increment - Integer"
This parameter allows you a degree of control over directional
arrowhead spacing in the field flow plot.  Streamlines allows a
maximum of one directional arrowhead for each grid box, where a grid
box is the space between adjacent integer grid coordinates in the grid
coordinate system along both dimensions. However, Streamlines actually
places an arrowhead only if both lower grid indices, modulo the value
of AGD, are equal to 0. If AGD is set to 2, for instance, Streamlines
places an arrowhead on the first streamline to enter each grid box
with even-numbered lower grid indices. 
.sp
If the transformation pipeline is everywhere linear from
grid coordinate space to NDC space, then the spacing of the
arrowheads should be more or less uniform over the field
flow plot. However, if there is a non-linearity anywhere in
the pipeline, the arrowheads will probably be more crowded
in some areas than in others. The default value of AGD is 2.
.IP "AMD - Arrow Head Minimum Distance - Real"
AMD allows you to specify, as a fraction of the viewport width, a
minimum distance between adjacent directional arrowheads along a
single streamline. If the data grid is transformed in such a way that
adjacent grid cells become very close in NDC space, as for instance in
many map projections near the poles, you can use this parameter to
help reduce the otherwise cluttered appearance of these regions of the
plot. Note that currently, whenever AMD has a positive value, the
first arrowhead that would otherwise be drawn for each streamline is
always eliminated. If AMD is less than or equal to 0.0, then no
arrowheads are eliminated. The default value of AMD is 0.0.
.IP "ARL - Arrow Head Length - Real"
ARL defines the length of each of the two lines used to create the
directional arrow head. If the parameter GBS is set to 0, ARL has
units "fraction of viewport width"; if GBS is set to 1, ARL has
the units "fraction of grid box width".  The default value of ARL is
0.012 when GBS has the value 0 and 0.33 when GBS has the value
1. Setting GBS causes ARL to be reset to its appropriate default
value.
.IP "CDS - Critical Displacement Multiplier - Real"
CDS specifies the minimum amount the streamline must grow
as a multiple of the basic differential step size each time
the stream progress is checked in order for the streamline
not to be terminated. The nominal differential step size is
specified by DFM in NDC space, and the progress is checked
each CKP iterations. Points of convergence or divergence
typically cause stream growth to diminish and the
streamline eventually to be terminated. The default value
of CDS is 2.0, meaning that any time a streamline does not
increase in length a minimum of 2.0*DFM in NDC over the
previous check, it is discontinued and a new streamline is
begun if possible.
.IP "CKP - Check Progress Iteration Count - Integer"
The parameter CKP specifies the number of iterations
through the streamline building loop between each check of
the streamline growth. If the distance between current
position of the streamline and the position saved at the
time of the previous check is less than a minimum amount,
defined as the value of CDS times the value of DFM in NDC
space, then the current streamline is terminated and a new
one begun if possible. The default value of CKP is 35.
.IP "CKX - Check Crossover Iteration Count - Integer"
CKX specifies the number of iterations through the streamline building
loop between checks for streamline crossover, that is, one streamline
growing closer than a certain distance (as specified by the parameter
SSP) to previously created streamline. A negative value of CKX causes
Streamlines to check for crossover only when a new grid box is
entered. At each crossover check, the current streamline position is
compared with a sampling of previous streamline positions retained in
an internal circular list. This list is currently fixed to a length of
750. Since up to this number of comparisons are performed at each
crossover check, the frequency with which these checks are performed
can have a noticeable impact on performance. By default, CKX has the
value -99, causing Streamlines to check for crossover only on entrance
to a new grid box.
.IP "CYK - Cyclical Data Flag - Integer"
Use this parameter to specify that the data in the vector field arrays
is cyclical: that is, it repeats with a period of M-1 (M, the input
parameter to STINIT) along the first dimensional axis. If the flag is
set on, Streamlines checks to see if the field data meet certain
criteria. If they do, an internal cyclical flag is set, causing the
normalized vector interpolation routines to consider data from the
opposite ends of the dataset when interpolating near the first
dimensional dataset boundaries. If the criteria are not met,
Streamlines sets an error flag, retrievable by the user through the
parameter ERR. Processing, however, continues without interruption,
except that Streamlines now interpolates (as it would ordinarily) near
the first dimensional end points without consideration of data at the
opposite end. The data must pass the following test in order to be deemed
cyclical: for each subscript value along the second dimensional
axis, the first element and the last element along the first
dimensional axis must be identical. A value of 0 for CYK means that
the data is to be considered non-cyclical; any other value means that
Streamlines should test for the cyclical condition.  You must
initialize Streamlines with a call to STINIT after modifying this
parameter. The default value of CYK is 0.
.IP "CPM - Compatibility Mode - Integer"
Controls the degree of compatibility between versions of the
Streamlines utility prior to NCAR Graphics 3.2 and the current
version. You can independently control three behaviors using the nine
settings provided:
.RS
.IP \(bu
use of STRMLN input parameters;
.IP \(bu
use of variables contained in the common blocks STR02 and
STR03;
.IP \(bu
use of the old coordinate mapping routines, FX
and FY.
.RE
.IP ""
Note, however, that when using the Version 3.2 entry points
STINIT and STREAM, only the third behavior option has any
meaning.
.sp
When CPM is set to 0, its default value, the Streamlines utility\'s
behavior varies depending on whether you access it 
through one of the pre-Version 3.2 entry points (STRMLN and EZSTRM),
or through the STINIT/STREAM interface. Otherwise, positive values
result in invocation of the older coordinate mapping routines (FX and
FY). Negative values cause the Version 3.2 mapping routines to be used
instead.  When using the pre-Version 3.2 interface only, odd values of
CPM cause the data values in the common blocks, STR02 and STR03, to
override corresponding values initialized in the Version 3.2 STDATA
block data subroutine, or set by the user calling STSETx routines.
Values of CPM with absolute value less than or equal to two cause the
NSET argument to STRMLN to take precedence over the SET parameter.
.sp
Here is a table of the nine settings of CPM and their
effect on the operation of the Streamlines utility:
.sp
.TS
tab(/);
l l l l.
Value/Use FX and FY/Use STR02,STR03/Use NSET
-----/-------------/---------------/--------
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
(*) Old means EZSTRM or STRMLN entry point; new, STINIT/STREAM.
Only the first column is applicable to the behavior
of the STINIT/STREAM interface. See the
strmln man page for more detailed emulation information.
.IP "DFM - Streamline Differential Magnitude"
DFM specifies the length of the differential magnitude step size used
by Streamlines. If the parameter GBS is set to 0 DFM has units
"fraction of viewport width"; if GBS is set to 1, DFM has the units
"fraction of grid box width". When the Version 3.2 mapping routines
are used, DFM directly affects processing time and the resulting plot
precision. In general, smaller values of DFM cause Streamlines to take
more, smaller steps in the construction of a streamline, resulting,
within the limits of the processor\'s floating point resolution, in
longer execution times and a more precise plot.  Process memory
requirements are not affected. If the compatibility mode parameter is
set such that the older mapping routines, FX and FY, are invoked
instead, DFM no longer has any effect on the plot, since in this case
the step size is determined by the setting of the parameter VNL as a
fraction of the grid box width. The default value of DFM is 0.02 when
GBS has the value 0 and 0.33 when GBS has the value 1. Setting GBS
causes DFM to be reset to its appropriate default value.
.IP "GBS - Grid-Based Spacing - Real"
The parameter GBS controls the interpretation of several parameters
that play a critical role in the appearance of the streamline
plot. These parameters are DFM, SSP, and ARL. When GBS has the value
0, the values of these parameters are treated as having units of
"fraction of viewport width". If GBS has the value 1, the values are
treated as having the units of "fraction of grid box width". Whenever
you set GBS, the three affected parameters are reset to default values
appropriate to the units; therefore you must set GBS prior to setting
any non-default values for DFM, SSP, or ARL. You may find that using
the grid-based spacing method causes Streamlines to adapt more
gracefully to variations in the density of the data grid. Currently, the
default value of GBS is 0; however, in the next release this may change.
.IP "LWD - Streamline Linewidth - Real"
LWD controls the linewidth used to draw the streamlines.  Note that
since the linewidth in NCAR Graphics is always calculated relative to
a unit linewidth that is dependent on the output device, you may need
to adjust the linewidth value depending on the output conditions to
obtain a pleasing plot. LWD affects the linewidth of the directional
arrowheads as well as the streamlines themselves. The arrowhead length
also increases somewhat when the linewidth is greater than the
default. However, the arrowhead length parameter still affects the
length. The default is 1.0, specifying a device-dependent minimum
linewidth.
.IP "MAP - Map Transformation Code - Integer"
MAP defines the mapping transformation between the data and user
coordinate space. For more information on coordinate mapping
transformations see the stuixy, stumxy, and stumta man pages, as well
as the description of the transformation type parameter, TRT.  Three
MAP parameter codes are reserved for pre-defined transformations, as
follows:
.RS
.IP "Value" 15
Mapping transformation
.IP "0 (default)" 15
Identity transformation between data and
user coordinates: array indices of U and V
are linearly related to user coordinates.
Note however that a non-linear
transformation is still possible from user
to NDC coordinates.
.IP "1" 15
Ezmap transformation: first dimension
indices of U and V are linearly related to
longitude; second dimension indices are
linearly related to latitude.
.IP "2" 15
Polar to rectangular transformation: first
dimension indices of U and V are linearly
related to the radius; second dimension
indices are linearly related to the angle
in degrees.
.RE
.IP ""
If MAP has any other value, Streamlines invokes the user-modifiable
subroutines, STUMXY, STUIXY, and STUMTA to perform the mapping. The
default version of these routines simply performs an identity mapping.
Note that, while the Streamlines utility does not actually prohibit
the practice, you are advised not to use negative integers for
user-defined mappings, since other utilities in the NCAR Graphics
toolkit attach a special meaning to negative mapping codes.
.sp
For all the predefined mappings, the linear relationship between the
grid array indices and the data coordinate system is established using
the four parameters, XC1, XCM, YC1, and YCN. The X parameters define a
mapping for the first and last indices of the first dimension of the
data arrays, and the Y parameters do the same for the second
dimension. If MAP is set to a value of one, you need to be careful to
ensure that the SET parameter is given a value of zero, since the
Ezmap routines require a specific user coordinate space for each
projection type, and internally call the SET routine to define the
user to NDC mapping.  Otherwise, you may choose whether or not to
issue a SET call prior to calling STINIT, modifying the value of SET
as required. See the description of the parameter, TRT, and the man
pages, stumxy, stuixy, and stumta for more information.
.IP "MSK - Mask To Area Map Flag - Integer"
Use this parameter to control masking of streamlines to an
existing area map created by routines in the Areas utility.
When MSK is greater than 0, masking is enabled and an
area map must be set up before calling STREAM. The
area map array and, in addition, the name of a user-definable
masked drawing routine, must be passed as input
parameters to STREAM. There are two states for the MSK
parameter, as follows:
.RS
.IP Value 15
Effect
.IP "<= 0 (default)" 15
No streamline masking.
.IP >0 15
The subroutine ARDRLN is called internally
to decompose the streamlines into segments
contained entirely within a single area
group. ARDRLN calls the user-definable
masked drawing subroutine.
.RE
.IP " "
See the man page, stumsl, for further information on the user-definable
masked drawing subroutine.
.IP "PLR - Polar Input Mode - Integer"
When PLR is greater than zero, the vector component arrays
are considered to contain the field data in polar
coordinate form: the U array is treated as containing the
vector magnitude and the V array as containing the vector
angle. Be careful not to confuse the PLR parameter with the
MAP parameter polar coordinate mode. The MAP parameter
relates to the location of the vector, not its value. Here
is a table of values for PLR:
.RS
.IP Value 15
Meaning
.IP "0 (default)" 15
U and V arrays contain data in cartesian
component form.
.IP "1" 15
U array contains vector magnitudes; V
array contains vector angles in degrees.
.IP "2" 15
U array contain vector magnitudes; V array
contains vector angles in radians.
.RE
.IP " "
You must initialize Streamlines with a call to STINIT after modifying
this parameter.
.IP "SET - SET Call Flag - Integer"
Give SET the value 0 to inhibit the SET call STINIT performs by
default. Arguments 5-8 of a SET call made by the user must be
consistent with the ranges of the user coordinates expected by
Streamlines. This is determined by the mapping from grid to data
coordinates as specified by the values of the parameters XC1, XCM,
YC1, YCN, and also by the mapping from data to user coordinates
established by the MAP parameter. You must initialize Streamlines
with a call to STINIT after modifying this parameter. The default
value of SET is 1.
.IP "SGD - Stream Starting Grid Increment - Integer"
This parameter gives you a degree of control over the number and
density of streamlines in the field flow plot.  The Streamlines
utility never begins a streamline in any grid box that has previously
had a streamline pass through it, where a grid box is defined as the
space between adjacent integer grid coordinates in the grid coordinate
system along both dimensions. By setting SGD to a value greater than
1, you can reduce the number of grid boxes initially eligible for
starting a streamline. A grid box is considered initially eligible for
starting a streamline only if both the lesser indices that establish
the grid box, modulo the value of SGD, equal 0. If SGD is set to 2,
for instance, every grid box with even-numbered lower grid indices
would be initially eligible for starting a streamline. As the
streamlines grow and pass through grid boxes that were initially
eligible, these boxes too are marked ineligible, further reducing the
boxes where a stream can be started.
.sp
If the transformation pipeline is everywhere linear from grid
coordinate space to NDC space, then this scheme for starting
streamlines usually produces a more or less uniform spacing of
the streamlines over the field flow plot. However, if there are
non-linear transforms anywhere in the pipeline, the streamlines will
probably be more crowded in some areas than in others. Future
enhancements to the Streamlines utility are expected to address this
issue, and also perhaps to provide options for intentional non-uniform
spacing based on flow intensity. The default value of SGD is 2.
.IP "SSP - Streamline Spacing Value - Real"
The streamline spacing parameter establishes the minimum distance a
streamline in progress is allowed to approach existing streamlines
before being terminated. If the parameter GBS is set to 0, SSP has
units "fraction of viewport width"; if GBS is set to 1, SSP has the
units "fraction of grid box width". In general, within either system of
units, larger values of SSP increase the distance between
streamlines, and have a tendency to create more, but shorter stream
lines. The spacing is only checked at intervals, so streamlines
sometimes approach closer than the specified distance. The checking
frequency is adjustable using the streamline crossover checking
parameter, CKX. The streamline starting grid increment parameter, SGD,
also affects the overall streamline density. The default value of SSP
is 0.015 when GBS has the value 0 and 0.5 when GBS has the value
1. Setting GBS causes SSP to be reset to its appropriate default
value.
.IP "SST - Streamline Statistics Output Flag - Integer"
If SST is set to one, STREAM writes a summary of its
operations to the default logical output unit, including
the number of streamlines plotted and the total
differential step count. Here is a sample of the output:
.RS 10 
.nf
.sp
STREAM Statistics
.br
.ta 36nR +2n 
	Streamlines plotted:	119
.br
	Total differential step count:	2903
.RE
.fi
.IP ""
The differential step count actually counts the number of
iterations through the main streamline construction loop,
and can be used to help gauge the trade-offs between the
increased processing time required for smaller differential
step sizes and the resulting differences in plot quality.
.IP "SVF - Special Value Flag - Integer"
The special value flag controls special value processing
for the U and V vector component data arrays. Special
values may appear in either the U or V array or in both of
them. When any of the four points surrounding the current
streamline end contain a special value, the streamline is
terminated, and a new one started, if possible. Streamlines
allows special value processing to be turned on or off, as
follows:
.RS
.IP Value 15
Effect
.IP "0 (default)" 15
Neither the U nor the V array is examined
for special values
.IP "non 0" 15
Whenever the streamline under construction
enters a new grid box, the U and V array
values at each corner of the box are
examined for special values. The
interpolation method parameter, TRP, is
overridden, causing Streamlines to use bi-linear
interpolation only.
.RE
.IP ""
The U and V special values are defined by setting parameters USV and
VSV. Streamlines only uses bi-linear interpolation when special value
processing is in effect, because the Bessel interpolation method
quadruples the requirement for good data points (from 4 to 16)
surrounding the current stream end point. You must initialize
Streamlines with a call to STINIT after modifying this parameter.
.IP "TRP - Interpolation Method - Integer"
Use TRP to control which of two interpolation methods
Streamlines should use in determining the normalized flow
components for each point in the streamline. The choices
are as follows:
.RS
.IP "Value" 15
Interpolation Method
.IP "0 (default)" 15
Use the 16-point Bessel interpolation
method where possible; otherwise, near the
data set boundaries use 12, 9 or 4 point
interpolation methods, depending on the
situation.
.IP "non 0" 15
Use 4-point bi-linear interpolation at all
points.
.RE
.IP ""
Note that Streamlines forces use of the 4-point bi-linear
interpolation method if the SVF parameter is set to turn on
special value processing.
.IP "TRT - Transformation Type - Integer"
The transformation type parameter, TRT, qualifies the
mapping transformation specified by the MAP parameters, as
follows:
.RS
.IP "Value" 15
Effect
.IP "-1" 15
Direction, magnitude, and location are all
transformed. This option is not currently
supported by any of the pre-defined
coordinate system mappings.
.IP "0" 15
Only location is transformed
.IP "1 (default)" 15
Direction and location are transformed
.RE
.IP ""
This parameter allows you to distinguish between a
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
nearly cartesian space (i.e. if the northward components
were all set to 0.0, the streamlines defined by the
eastward components would all be parallel straight lines.
One would set MAP to two (for the polar transformation) and
TRT to zero to model this data on a plot generated by the
Streamlines utility.
.sp
On the other hand, picture a set of wind data, again given
as eastward and northward wind components, but this time
the center of the polar map is actually one of the earth\'s
poles. In this case, the eastward components do not point
in a single direction; instead they outline a series of
concentric circles around the pole. This is a space mapping
transformation: one would again set MAP to two, but TRT
would be set to one to transform both direction and
location.
.sp
Changing the setting of this parameter affects the end
results only when a non-uniform non-linear mapping occurs
at some point in the transformation pipeline. For this
discussion a uniform linear transformation is defined as
one which satisfies the following equations:
.RS 10
.sp
x_out = x_offset + scale_constant * x_in
.br
y_out = y_offset + scale_constant * y_in
.RE
.IP ""
If scale_constant is not the same for both equations then
the mapping is non-uniform.
.sp
This option is currently implemented only for the pre-defined MAP
parameter codes, 0 and 2, the identity mapping and the polar
coordinate mapping. However, it operates on a different stage of the
transformation pipeline in each case. The polar mapping is non-linear
from data to user coordinates. The identity mapping, even though
necessarily linear over the data to user space mapping, can have a
non-uniform mapping from user to NDC space, depending on the values
given to the input parameters of the SET call. This will be the case
whenever the LL input parameter specifies a logarithmic scaling or the
viewport and the user coordinate boundaries do not have the same
aspect ratio. Thus for a MAP value of 2, TRT affects the mapping
between data and user space, whereas for MAP set to 0, TRT influences
the mapping between user and NDC space.
.IP "USV - U Array Special Value - Real"
USV is the U vector component array special value. It is a value
outside the range of the normal data used to indicate that there is no
valid data for this grid location. When the special value flag
parameter, SVF, is non-zero, each time a streamline enters a new cell
Streamlines will check for this special value in the U array at each
of the four corners of the grid box. Anytime the special value is
discovered, the current streamline is terminated and a new one started
if possible. The default value given to USV is 1.0 * 10**12.
.IP "VNL - Vector Normalization Value - Real"
The parameter, VNL, determines the value Streamlines uses
to normalize the vector flow field, before beginning the
streamline construction loop. When Streamlines is used with
the pre-Version 3.2 mapping routines, FX and FY, the value
of VNL determines the step size in the grid coordinate
system used to construct the streamlines, as a fraction of
the grid box size. When using FX and FY, smaller values of
VNL result in smaller steps, more processing time and,
within the limits of the processor\'s floating point
accuracy, a higher precision plot. However, if the mapping
has non-linearities, the grid size does not remain constant
over the transformation and the step size can vary greatly,
resulting in discontinuities in certain areas of the plot.
.sp
Streamline\'s new mapping routines define the streamline differential
magnitude in NDC space, ensuring a constant step size over the whole
plot, notwithstanding any non-linearity in the transformation. When
using the new mapping routines, the parameter DFM controls the step
size in NDC space, and VNL is not adjustable by the user. (See the
discussion of the compatibility mode parameter, CPM, for a discussion
of how to switch between the old and new mapping routines.) The
default value of VNL is 0.33.
.IP "VPB - Viewport Bottom - Real"
The parameter VPB has an effect only when SET is non-zero, specifying
that Streamlines should do the call to SET. It defines a minimum
boundary value for the bottom edge of the viewport in NDC space, and
is constrained to a value between 0.0 and 1.0. It must be less than
the value of the Viewport Top parameter, VPT. The actual value of the
viewport bottom edge used in the plot may be greater than the value of
VPB, depending on the setting of the Viewport Shape parameter, VPS.
You must initialize Streamlines with a call to STINIT after
modifying this parameter. The default value of VPB is 0.05.
.IP "VPL - Viewport Left - Real"
The parameter VPL has an effect only when SET is non-zero, specifying
that Streamlines should do the call to SET. It defines a minimum
boundary value for the left edge of the viewport in NDC space, and is
constrained to a value between 0.0 and 1.0. It must be less than the
value of the Viewport Right parameter, VPR. The actual value of the
viewport left edge used in the plot may be greater than the value of
VPL, depending on the setting of the Viewport Shape parameter, VPS.
You must initialize Streamlines with a call to STINIT after modifying
this parameter.  The default value of VPL is 0.05.
.IP "VPR - Viewport Right - Real"
The parameter VPR has an effect only when SET is non-zero, specifying
that Streamlines should do the call to SET. It defines a maximum
boundary value for the right edge of the viewport in NDC space, and is
constrained to a value between 0.0 and 1.0. It must be greater than
the value of the Viewport Left parameter, VPL. The actual value of the
viewport right edge used in the plot may be less than the value of
VPR, depending on the setting of the Viewport Shape parameter, VPS.
You must initialize Streamlines with a call to STINIT after modifying
this parameter.  The default value of VPR is 0.95.
.IP "VPS - Viewport Shape - Real"
The parameter VPS has an effect only when SET is non-zero,
specifying that Streamlines should do the call to SET; it
defines the desired viewport shape, as follows:
.RS
.IP Value 15
Effect
.IP "<0.0" 15
The absolute value of VPS specifies the
shape to use for the viewport, as the
ratio of the viewport width to its height,
.IP "0.0" 15
The viewport completely fills the area
defined by the boundaries specifiers, VPL,
VPR, VPB, VPT
.IP ">0.0,<1.0 (0.25,default)" 15
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
VPR, and VPT. You must initialize Streamlines with a call to STINIT
after modifying this parameter.
.IP "VPT - Viewport Top - Real"
The parameter VPT has an effect only when SET is non-zero, specifying
that Streamlines should do the call to SET. It defines a maximum
boundary value for the top edge of the viewport in NDC space, and is
constrained to a value between 0.0 and 1.0. It must be greater than
the value of the Viewport Bottom parameter, VPB. The actual value of
the viewport top edge used in the plot may be less than the value of
VPT, depending on the setting of the Viewport Shape parameter, VPS.
You must initialize Streamlines with a call to STINIT after modifying
this parameter.  The default value of VPT is 0.95.
.IP "VSV - V Array Special Value - Real"
VSV is the V vector component array special value. It is a value
outside the range of the normal data used to indicate that there is no
valid data for this grid location. When the special value flag
parameter, SVF, is non-zero, each time a streamline enters a new cell
Streamlines will check for this special value in the V array at each
of the four corners of the grid box.  Anytime the special value is
discovered, the current streamline is terminated and a new one started
if possible.  You must initialize Streamlines with a call to STINIT
after modifying this parameter.  The default value given to VSV is 1.0
* 10**12.
.IP "WDB - Window Bottom - Real"
When STINIT does the call to SET, the parameter WDB is used to
determine argument number 7, the user Y coordinate at the bottom of
the window. If WDB is not equal to WDT, WDB is used. If WDB is equal
to WDT, but YC1 is not equal to YCN, then YC1 is used. Otherwise, the
value 1.0 is used.  You must initialize Streamlines with a call to
STINIT after modifying this parameter.  The default value of WDB is
0.0.
.IP "WDL - Window Left - Real"
When STINIT the call to SET, the parameter WDL is used to determine
argument number 5, the user X coordinate at the left edge of the
window. If WDL is not equal to WDR, WDL is used. If WDL is equal to
WDR, but XC1 is not equal to XCM, then XC1 is used. Otherwise, the
value 1.0 is used. You must initialize Streamlines with a call to
STINIT after modifying this parameter.  The default value of WDL is
0.0.
.IP "WDR - Window Right - Real"
When STINIT does the call to SET, the parameter WDR is used to
determine argument number 6, the user X coordinate at the right edge
of the window. If WDR is not equal to WDL, WDR is used. If WDR is
equal to WDL, but XCM is not equal to XC1, then XCM is used.
Otherwise, the value of the STINIT input parameter, M, converted to a
real, is used.  You must initialize Streamlines with a call to STINIT
after modifying this parameter.  The default value of WDR is 0.0.
.IP "WDT - Window Top - Real"
When STINIT does the call to SET, the parameter WDB is used to
determine argument number 8, the user Y coordinate at the top of the
window. If WDT is not equal to WDB, WDT is used. If WDT is equal to
WDB, but YCN is not equal to YC1 then YCN is used. Otherwise, the
value of the STINIT input parameter, N, converted to a real, is used.
You must initialize Streamlines with a call to STINIT after modifying
this parameter.  The default value of WDT is 0.0.
.IP "XC1 - X Coordinate at Index 1 - Real"
The parameter XC1 specifies the X coordinate value that corresponds to
a value of 1 for the first subscript of the U and V, vector field
component arrays. Together with XCM, YC1, and YCN it establishes the
mapping from grid coordinate space to data coordinate space. If XC1 is
equal to XCM, 1.0 will be used. You must initialize Streamlines with a
call to STINIT after modifying this parameter.  The default value of
XC1 is 0.0.
.IP "XCM - X Coordinate at Index M - Real"
The parameter XCM specifies the X coordinate value that corresponds to
the value of the STINIT input parameter, M, for the first subscript of
the U and V vector component arrays. Together with XC1, YC1, and YCN
it establishes the mapping from grid coordinate space to data
coordinate space. If XC1 is equal to XCM, the value of M, converted to
a real, will be used. You must initialize Streamlines with a call to
STINIT after modifying this parameter.  The default value of XCM is
0.0
.IP "YC1 - Y Coordinate at Index 1 - Real"
The parameter YC1 specifies the Y coordinate value that corresponds to
a value of 1 for the first subscript of the U, V, vector component
arrays as well as for the P scalar data array, if used. Together with
YCN, XC1, and XCM it establishes the mapping from grid coordinate
space to data coordinate space. If YC1 is equal to YCN, 1.0 will be
used.  You must initialize Streamlines with a call to STINIT after
modifying this parameter.  The default value of YC1 is 0.0
.IP "YCN - Y Coordinate at Index N - Real"
The parameter YCN specifies the Y coordinate value that corresponds to
the value of the STINIT input parameter, N, for the second subscript
of the U and V vector component arrays as well as the P scalar data
array, if used.  Together with YC1, XC1, and XCM it establishes the
mapping from grid coordinate space to data coordinate space. If YC1 is
equal to YCN, the value of N, converted to a real, will be used. You
must initialize Streamlines with a call to STINIT after modifying this
parameter.  The default value of YCN is 0.0
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
stgetc,
stgeti,
stgetr,
stinit,
stream,
streamlines,
strset,
stsetc,
stseti,
stsetr,
stuixy,
stumsl,
stumta,
stumxy,
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
