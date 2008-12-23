.TH VELVCT 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VELVCT - Plots a vector field, given two 2-dimensional vector
component arrays, U and V. You may control certain characteristics of
the plot by adjusting the values given to the input arguments. Other
less frequently changed characteristics may be affected by modifying
VELDAT, the BLOCK DATA routine that initializes members of the common
blocks VEC1 and VEC2. In addition, depending on the value given to the
compatibility parameter, CPM, you may set other options by calling VVSETC,
VVSETI, or VVSETR to modify internal parameters.
.SH SYNOPSIS
CALL VELVCT (U,LU,V,LV,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
.SH STATUS
VELVCT is obsolete, and is supported only to provide compatibility
with old NCAR Graphics codes. However, the compatibility mode
parameter, CPM, offers a number of options to help ease the the
transition to the new version of the utility. When writing new code
you are encouraged not to use this entry point, since it provides less
capability than the standard Vectors interface, and may eventually
be phased out.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_velvct(float *u, int lu, float *v, int lv, int m,
.br
              int n, float flo, float hi, int nset,
.br
              int length, int ispv, float *spv)
.SH DESCRIPTION
.IP U 12
(REAL 2-dimensional array, dimensioned LU x n: n >= N, input): By
default, assumed to contain the first dimensional Cartesian
components of the vector field. However, if PLR is non-zero, it is
treated as containing the vector magnitudes.
.IP LU 12
(INTEGER, input): Actual value of the first dimension of array U.
.IP V 12
(REAL 2-dimensional array, dimensioned LV x n: n >= N, input): By
default, assumed to contain the second dimensional Cartesian
components of the vector field. However, if PLR is non-zero, it is
treated as containing the vector angles.
.IP LV 12
LV (INTEGER, input): Actual value of the first dimension of array V.
.IP M 12
(INTEGER, input): Number of contiguous elements along the first
dimensional axis containing data to be processed in each of the
arrays, U and V.
.IP N 12
(INTEGER, input): Number of contiguous elements along the second
dimensional axis containing data to be processed in each of the
arrays, U and V.
.IP FLO 12
(REAL, input): Minimum vector magnitude allowed to be displayed in the
plot.
.IP HI 12
(REAL, input): Maximum vector magnitude allowed to be displayed in the
plot. If set to 0.0 there is no upper limit imposed.
.IP NSET 12
(INTEGER, input): Flag that controls how and when the SET call is
invoked. If NSET is 0, VELVCT makes a SET call to establish a standard
viewport and window boundaries coincident with the array coordinate
boundaries. PERIM is called to draw a border. If NSET is greater than
zero, VELVCT does not call SET or PERIM. If NSET is less than zero,
VELVCT calls SET to establish window boundaries coincident with the
array grid coordinate boundaries but does not modify the viewport or
call PERIM. Unlike the VVINIT/VVECTR interface, when VELVCT does a SET
call, it always restores the original coordinate system state before
returning.
.IP LENGTH 12
(INTEGER, input): The length in plotter address units (PAUs) used to
render the vector with the greatest magnitude that is eligible for
plotting. A vector is eligible for plotting if it is less than or
equal to the value of HI, unless HI is 0.0 in which case all vectors
are eligible. If LENGTH is set to 0.0, the length selected is one half
the diagonal length of a grid cell assuming a linear mapping of the
grid coordinate space into the viewport.
.IP ISPV 12
(INTEGER, input): Flag to control the special value feature. 0 means
that the feature is not in use. 1 means that if the value of
U(I,J)=SPV(1) the vector will not be plotted.  2 means that if the
value of V(I,J)=SPV(2) the vector will not be plotted. 3 means that
if either U(I,J)=SPV(1) or V(I,J)=SPV(2) then the vector will not be
plotted. 4 means that if U(I,J)=SPV(1) and V(I,J)=SPV(2), the vector
will not be plotted.
.IP SPV 12
(REAL array, dimensioned 2, input): An array of length 2 which gives
the value in the U array and the value in the V array which denote
special values. This argument is ignored if ISPV=0. The default values
are 1.0E12.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions with the following exceptions:
.sp
.IP lu 12
The second dimension of u in the calling program.
.IP lv 12
The second dimension of v in the calling program.
.IP m 12
Number of contiguous elements along the
second dimensional axis containing data to be processed in
each of the arrays, u and v.
.IP n 12
Number of contiguous elements along the
first dimensional axis containing data to be processed in
each of the arrays, u and v.
.SH USAGE
Beginning with version 3.2 of NCAR Graphics, the VELVCT entry point
has been recoded as a front end to the VVINIT/VVECTR interface to
Vectors. The compatibility mode parameter, CPM, controls the
degree to which a call to the Version 3.2 VELVCT emulates the older
call. Appropriate settings of CPM can separately address each of the
following three questions regarding the level of emulation:
.IP \(bu
Should FX, FY, MXF, and MYF rather than the Version 3.2 mapping
routines perform the mapping to user coordinates?
.IP \(bu
Should the input arguments FLO, HI, NSET, LENGTH, ISPV, SPV override
the current values of the corresponding Vectors' internal parameters?
.IP \(bu
Should the values contained in the common blocks VEC1 and VEC2
override the current values of corresponding Vectors' internal
parameters?
.PP
Given the default value of CPM, all these questions are answered in
the affirmative, and a call to VELVCT gives a reasonably faithful
emulation of the older version's behavior.
.sp
However, even in this case, it is possible to use the parameter
setting routines to control the behavior of features that have no
counterpart in the older version of VELVCT, as long as the feature is
accessible without calling the new interface. For instance, you could
control the vector linewidth by setting the LWD parameter, although
you could not draw vectors masked to an area map because doing so
requires direct invocation of VVECTR
.sp
The following two tables show how the VELVCT input arguments and
VEC1/VEC2 common block members map into internal parameters currently
supported by Vectors. (Consult the vectors_params man page for
descriptions of the internal parameters.)
.RS
.IP "Input Argument" 22
Internal Parameter
.IP "FLO" 22
VLC (VLC is set to -FLO if FLO is positive, 0.0 otherwise)
.IP "HI" 22
VHC (VHC is set to -HI if HI is positive, 0.0 otherwise)
.IP "NSET" 22
SET (NSET = 0 is approximately equivalent to SET = 1)
.IP "LENGTH" 22
VRL (LENGTH in Plotter Address Units is converted to VRL as a fraction
of the viewport width)
.IP "ISPV" 22
SVF
.IP "SPV(1)" 22
USV
.IP "SPV(2)" 22
VSV
.RE
.sp
.RS
.IP "Common Block Member" 22
Internal Parameter
.IP "EXT" 22
VPS
.IP "ICTRFG" 22
VPO
.IP "ILAB" 22
LBL
.IP "IOFFD" 22
DPF
.IP "RMN" 22
AMN (RMN in metacode coordinates is converted to AMN as a fraction of the
viewport width)
.IP "RMX" 22
AMX (RMX in metacode coordinates is converted to AMX as a fraction of the
viewport width)
.IP "SIZE" 22
LBS (Character size in metacode coordinates is converted to LBS as a fraction 
of viewport width)
.IP "INCX" 22
XIN
.IP "INCY" 22
YIN
.IP "-" 22
MNT (When common blocks VEC1 and VEC2 override parameter settings, MNT
is always set to " ", indicating that the minimum vector text block
is not to be displayed.)
.IP "IOFFM" 22
MXT (If IOFFM is non-zero, MXT is set to " ", indicating that the
maximum text block is not to be displayed. Otherwise MXT is set to the
string "MAXIMUM VECTOR")
.IP "IOFFM" 22
MXX (If IOFFM is 0, MXX is set to a computed value that, as a fraction
of the viewport width, specifies a point 0.05 in NDC left of the right
hand edge of the plotter frame.)
.IP "IOFFM" 22
MXY (If IOFFM is 0, MXY is set to a computed value that, as a fraction
of the viewport height, specifies a point 0.005 in NDC up from the
bottom edge of the plotter frame.)
.IP "IOFFM" 22
MXP (If IOFFM is 0, MXP is set to -2, indicating that the text block
should be lower-right justified.
.SH
Note that the emulation of the maximum vector text block differs from
the older implementation in that it now uses high-quality text.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:
stex02,
tvelvc.
.SH ACCESS
To use VELVCT or c_velvct, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
vectors,
vectors_params,
fx,
vvectr,
vvgetc,
vvgeti,
vvgetr,
vvinit,
vvrset,
vvsetc,
vvseti,
vvsetr,
vvudmv,
vvumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
