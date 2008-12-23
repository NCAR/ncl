.TH SET 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SET - defines the mapping between fractional and user coordinates: sets
the values of the SPPS internal parameters 'LS' (axis linear/log scaling)
and 'MI' (axis mirror imaging); defines GKS normalization transformation 1.
.SH SYNOPSIS
CALL SET(VL,VR,VB,VT,WL,WR,WB,WT,LF)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_set(float vl, float vr, float vb, float vt, float wl, float wr, float wb, float wt, int lf)
.SH DESCRIPTION 
.IP VL 12
(an input expression of type REAL) defines the left margin of the
viewport in fractional coordinates.
.IP VR 12
(an input expression of type REAL) defines the right margin of the
viewport in fractional coordinates.
.IP VB 12
(an input expression of type REAL) defines the bottom margin of the
viewport in fractional coordinates.
.IP VT 12
(an input expression of type REAL) defines the top margin of the
viewport in fractional coordinates.
.IP WL 12
(an input expression of type REAL) defines the left margin of the
window in user coordinates.
.IP WR 12
(an input expression of type REAL) defines the right margin of the
window in user coordinates.
.IP WB 12
(an input expression of type REAL) defines the bottom margin of the
window in user coordinates.
.IP WT 12
(an input expression of type REAL) defines the top margin of the
window in user coordinates.
.IP LF 12
(an input expression of type INTEGER) defines the linear/log nature of
the mapping, as follows:
.RS
.IP 1 3
linear X, linear Y
.IP 2 3
linear X, log Y
.IP 3 3
log X, linear Y
.IP 4 3
log X, log Y
.RE
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
Note that in setting the limits of the viewport and the window, the
left margin can have a user coordinate value that is larger than the
user coordinate value at the right
margin.  This means the X axis scale will run from a maximum
value at the left to a minimum value at the right.  Likewise the
bottom value can exceed the top value meaning that the Y axis can
run from a maximum at the bottom to a minimum at the top.  This
reversal of the axes is referred to as "mirror imaging".
It represents a substantial functional
enhancement over what is provided in GKS.  That is why NCAR Graphics
SET calls using fractional and user coordinates are in general preferred
over GKS calls to GSVP and GSWN using normalized device coordinates
and world coordinates.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
fcoord1, fcoord2, splogy, sprevx.
.sp
sprevx shows the use of routine SET for an X axis scaling reversal,
splogy shows the use of routine SET for logarithmic scaling of the Y axis,
fcoord1 shows the mapping from GKS world coordinates to GKS normalized
device coordinates, and
fcoord2 shows the mapping from NCAR Graphics user coordinates to GKS
normalized device coordinates.
.SH ACCESS
To use SET or c_set, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
gsvp, gswn, getset, getusv, setusv, spps, spps_params, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics;
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
