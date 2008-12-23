.TH STRMLN 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STRMLN - Plots a streamline representation of field flow data, given
two 2-dimensional vector component arrays, U and V. You may control
certain characteristics of the plot by adjusting the values given to
the input arguments. In addition, depending on the value given to the
compatibility parameter, CPM, you may set other options by calling
STSETI or STSETR to modify internal parameters.
.SH STATUS
STRMLN is obsolete, and is supported only to provide compatibility
with old NCAR Graphics codes. However, the compatibility mode
parameter, CPM, offers a number of options to help ease the the
transition to the new version of the utility. When writing new code
you are encouraged not to use this entry point, since it provides less
capability than the standard Streamlines interface, and may eventually
be phased out.
.SH SYNOPSIS
CALL STRMLN (U,V,WORK,IMAX,IPTSX,JPTSY,NSET,IER)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_strmln (float *u, float *v, float *work, int imax,
.br
               int iptsx, int jptsy, int nset, int *ier)
.SH DESCRIPTION 
.IP U 12
(REAL 2-dimensional array, dimensioned IMAX x n: n >=
JPTSY, input): By default, assumed to contain the first
dimensional Cartesian components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector magnitudes.
.IP V 12
(REAL 2-dimensional array, dimensioned IMAX x n: n >=
JPTSY, input): By default, assumed to contain the second
dimensional Cartesian components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector angles.
.IP WORK 12
(REAL array, dimensioned n: n>= 2*IPTSX*JPTSY, working
space): User provided work array used to store the
normalized vector component values, and also to keep track
of the grid boxes eligible for starting a streamline or
placement of a directional arrow.
.IP IMAX 12
(INTEGER, input): Actual value of the first dimension
of arrays U and V.
.IP IPTSX 12
(INTEGER, input): Number of contiguous elements along the first
dimensional axis containing data to be processed in each of the
arrays, U and V.
.IP JPTSY 12
(INTEGER, input): Number of contiguous elements along
the second dimensional axis containing data to be processed
in each of the arrays, U and V.
.IP NSET 12
(INTEGER, input) Flag that controls how and when the SET call is
invoked. If NSET is 0, STRMLN makes a SET call to establish a standard
viewport and window boundaries coincident with the array coordinate
boundaries. PERIM is called to draw a border. If NSET is greater than
zero, STRMLN does not call SET or PERIM. If NSET is less than zero,
STRMLN calls SET to establish window boundaries coincident with the
array grid coordinate boundaries but does not modify the viewport or
call PERIM. Unlike the STINIT/STREAM interface, when STRMLN does a SET
call, it always restores the original coordinate system state before
returning.
.IP IER 12
(INTEGER, output) If no error involving the ICYC common block variable
(or, depending on the compatibility mode, the CYK internal parameter)
is detected IER contains the value 0 on exit from STRMLN. If, when
STRMLN is invoked, ICYC (or CYK) is erroneously set to indicate that
the data is cyclic, STRMLN still processes the data using non-cyclic
interpolation formulas, but returns the value of -1 in IER.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions, with the following exceptions:
.sp
.IP imax 12
The second dimension of u and v in the calling program
.IP iptsx 12
Number of contiguous elements along the second
dimensional axis containing data to be processed in each of the
arrays, u and v.
.IP jptsy 12
Number of contiguous elements along the first
dimensional axis containing data to be processed in each of the
arrays, u and v.
.SH USAGE
Beginning with version 3.2 of NCAR Graphics, the STRMLN entry point
has been recoded as a front end to the STINIT/STREAM interface to
Streamlines. The compatibility mode parameter, CPM, controls the
degree to which a call to the Version 3.2 STRMLN emulates the older
call. Appropriate settings of CPM can separately answer each of the
following three questions regarding the level of emulation:
.IP \(bu
Should FX and FY rather than the Version 3.2 mapping routines perform
the mapping to user coordinates?
.IP \(bu
Should the value of the input argument, NSET, override the current
value of the SET parameter?
.IP \(bu
Should the values
contained in the common blocks STR02 and STR03 override the current
values of corresponding Streamlines' internal parameters?
.PP
Given the default value of CPM, all these questions are answered in
the affirmative, and a call to STRMLN gives a reasonably faithful
emulation of the older version's behavior.
.sp
However, even in this case, it is possible to use the parameter
setting routines to control the behavior of features that have no
counterpart in the older version of STRMLN, as long as the feature is
accessible without calling the new interface. For instance, you could
control the streamline linewidth by setting the LWD parameter, but on
the other hand you could not draw streamlines masked to an area map
because doing so requires direct invocation of STREAM with the
proper input arguments. The following two tables show how the STRMLN
input arguments and STR02/STR03 common block members map into internal
parameters currently supported by Streamlines:
.RS
.IP "Input Argument" 22
Internal Parameter
.IP "NSET" 22
SET (NSET = 0 is approximately equivalent to SET = 1)
.RE
.sp
.RS
.IP "Common Block Member" 22
Internal Parameter
.IP "INITA" 22
SGD
.IP "INITB" 22
AGD
.IP "ITERP" 22
CKP
.IP "ITERC" 22
CKX
.IP "IGFLG" 22
TRP
.IP "ICYC" 22
CYK
.IP "IMSG" 22
SVF
.IP "UVMSG" 22
USV
.IP "UVMSG" 22
VSV
.IP "DISPL" 22
VNL
.IP "AROWL" 22
ARL (AROWL as fraction of grid box size is converted to ARL as fraction of 
viewport width)
.IP "CSTOP" 22
SSP (CSTOP as fraction of grid box size is converted to SSP as fraction of 
viewport width)
.IP "DISPL" 22
DFM (DISPL as fraction of grid box size is converted to DFM as fraction of 
viewport width)
.IP "DISPC/DISPL" 22
CDS (The critical displacement multiplier CDS is calculated as the ratio of 
DISPC to DISPL)
.RE
.PP 
See the streamlines_params man page for a description of the internal
parameters.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
stex02,
tstrml.
.SH ACCESS
To use STRMLN or c_strmln, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
fx,
stgetc,
stgeti,
stgetr,
stinit,
stream,
streamlines,
streamlines_params,
strset,
stsetc,
stseti,
stsetr,
stuixy,
stumsl,
stumta,
stumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
