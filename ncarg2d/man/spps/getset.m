.TH GETSET 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GETSET - returns a set of values which, if used as arguments in a call to
SET, will cause normalization transformation 1, axis linear/log scaling
(internal parameter 'LS'), and axis mirror imaging (internal parameter 'MI')
to be defined in such a way as to duplicate the combined effects of the
current normalization transformation, axis scaling, and axis mirror imaging.
.SH SYNOPSIS
CALL GETSET (VL,VR,VB,VT,WL,WR,WB,WT,LS)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_getset(float *vl, float *vr, float *vb, float *vt, float *wl, float *wr, float *wb, float *wt, int *lf)
.SH DESCRIPTION 
.IP VL 12
(an output variable of type REAL) defines the left margin of the
viewport in fractional coordinates.
.IP VR 12
(an output variable of type REAL) defines the right margin of the
viewport in fractional coordinates.
.IP VB 12
(an output variable of type REAL) defines the bottom margin of the
viewport in fractional coordinates.
.IP VT 12
(an output variable of type REAL) defines the top margin of the
viewport in fractional coordinates.
.IP WL 12
(an output variable of type REAL) defines the left margin of the
window in user coordinates.
.IP WR 12
(an output variable of type REAL) defines the right margin of the
window in user coordinates.
.IP WB 12
(an output variable of type REAL) defines the bottom margin of the
window in user coordinates.
.IP WT 12
(an output variable of type REAL) defines the top margin of the
window in user coordinates.
.IP LS 12
(an output variable of type INTEGER) defines the linear/log nature of
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
arex01, mpex07, mpex09, cpexcc.
.SH ACCESS
To use GETSET or c_getset, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gqnt, set, getusv, setusv, spps, spps_params, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
