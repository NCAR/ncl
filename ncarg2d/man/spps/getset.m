.TH GETSET 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GETSET - returns the values of the parameters used in the previous call
to routine SET.
.SH SYNOPSIS
CALL GETSET (VL,VR,VB,VT,WL,WR,WB,WT,LF)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_getset(float *vl, float *vr, float *vb, float *vt, float *wl, float *wr, float *wb, float *wt, int *lf)
.SH DESCRIPTION 
.IP VL 12
(an output parameter of type REAL) that defines the left margin of the
viewport in fractional coordinates.
.IP VR 12
(an output parameter of type REAL) that defines the right margin of the
viewport in fractional coordinates.
.IP VB 12
(an output parameter of type REAL) that defines the bottom margin of the
viewport in fractional coordinates.
.IP VT 12
(an output parameter of type REAL) that defines the top margin of the
viewport in fractional coordinates.
.IP WL 12
(an output parameter of type REAL) that defines the left margin of the
window in user coordinates.
.IP WR 12
(an output parameter of type REAL) that defines the right margin of the
window in user coordinates.
.IP WB 12
(an output parameter of type REAL) that defines the bottom margin of the
window in user coordinates.
.IP WT 12
(an output parameter of type REAL) that defines the top margin of the
window in user coordinates.
.IP LF 12
(an output parameter of type INTEGER) that defines the linear/log nature of
the mapping, where:
.nf

LF = 1, is linear X, linear Y
LF = 2, is linear X, log Y
LF = 3, is log X,    linear Y
LF = 4, is log X,    log Y
.fi
.SH C-BINDING DESCRIPTION                               ,,
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
Note that in setting the limits of the viewport and the window, the
left margin can have a coordinate value that is larger than the right
margin value.  This means the X axis scale will run from a maximum
value at the left to a minimum value at the right.  Likewise the
bottom value can exceed the top value meaning that the Y axis can
run from a maximum at the bottom to a minimum at the top.  This
is called axis reversal.  It represents a substantial functional
enhancement over what is provided in GKS.  That is why NCAR Graphics
SET calls using fractional and user coordinates are in general preferred
over GKS calls to GSVP and GSWN using normalized device coordinates
and world coordinates.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
arex01, mpex07, mpex09, cpexcc.
.SH ACCESS
To use GETSET, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_getset, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gqnt, set, getusv, setusv, spps, spps_params, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
