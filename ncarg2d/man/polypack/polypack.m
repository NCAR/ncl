.TH Polypack 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Polypack - a set of routines allowing polygons to be manipulated in various
ways. Each of the six principal routines is given a "clip" polygon, a "subject"
polygon, and a workspace; it operates on the two polygons in some way to create
either a derivative polygon or a set of trapezoids representing the interior
of that derivative polygon and then passes these on to a user-specified
processing routine.
.SH SYNOPSIS
.sp
PPDIPO - generates and returns the boundary of the "difference" polygon, which
consists of all points that are inside the subject polygon but not inside the
clip polygon.
.sp
PPINPO - generates and returns the boundary of the "intersection" polygon,
which consists of all points that are inside both the clip polygon and the
subject polygon.
.sp
PPUNPO - generates and returns the boundary of the "union" polygon, which
consists of all points that are inside either or both of the clip polygon
and the subject polygon.
.sp
PPDITR - generates and returns a set of trapezoids representing the
interior of the "difference" polygon, which consists of all points that
are inside the subject polygon but not inside the clip polygon.
.sp
PPINTR - generates and returns a set of trapezoids representing the
interior of the "intersection" polygon, which consists of all points
that are inside both the clip polygon and the subject polygon.
.sp
PPUNTR - generates and returns a set of trapezoids representing the
interior of the "union" polygon, which consists of all points
that are inside either or both of the clip polygon and the subject polygon.
.sp
PPPPAP - can be called to preprocess a polygon in such a way as to remove
certain peculiarities that can cause minor cosmetic errors in the output
from the routines that return trapezoids.
.sp
PPPLCL - clips a polyline against a clipping rectangle. It is intended that,
eventually, there should be a better routine that will clip a polyline
against an arbitrary polygon, but that routine has not yet been written.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_ppdipo
.br
c_ppditr
.br
c_ppinpo
.br
c_ppintr
.br
c_ppplcl
.br
c_ppppap
.br
c_ppunpo
.br
c_ppuntr
.SH ACCESS
To use Polypack routines, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
None.  All error conditions in Polypack routines are reported by means
of an error-flag argument.
.SH SEE ALSO
Online:
ppdipo, ppditr, ppinpo, ppintr, ppplcl, ppppap, ppunpo, ppuntr,
ncarg_cbind.
.sp
Hardcopy:
None.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
