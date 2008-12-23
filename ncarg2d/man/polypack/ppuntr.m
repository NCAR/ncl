.TH PPUNTR 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PPUNTR - generates and returns a set of trapezoids representing the
interior of the "union" polygon, which consists of all points
that are inside either or both of the clip polygon and the subject polygon.
.SH SYNOPSIS
CALL PPUNTR (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,RWRK,IWRK,NWRK,URPT,IERR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
.nf
void c_ppuntr(
     float *xccp,
     float *yccp,
     int nccp,
     float *xcsp,
     float *ycsp,
     int ncsp,
     float *rwrk,
     int *iwrk,
     int nwrk,
     int (*urpt_)(float *xcbl,
                  float *xcbr,
                  float *ycob,
                  float *dxle,
                  float *dxre,
                  float *ycot),
     int *ierr)
.fi
.SH DESCRIPTION 
.IP XCCP 12
(an input array of type REAL) is the X coordinate array for the clip polygon.
.IP YCCP 12
(an input array of type REAL) is the Y coordinate array for the clip polygon.
.IP NCCP 12
(an input expression of type INTEGER) is the number of points defining the
clip polygon.
.IP XCSP 12
(an input array of type REAL) is the X coordinate array for the subject polygon.
.IP YCSP 12
(an input array of type REAL) is the Y coordinate array for the subject polygon.
.IP NCSP 12
(an input expression of type INTEGER) is the number of points defining the
subject polygon.
.IP RWRK 12
(a scratch array, dimensioned NWRK, of type REAL) is a real workspace
array.  Because of the way in which they are used, RWRK and IWRK may be
EQUIVALENCEd (and, to save space, they should be).
.IP IWRK 12
(a scratch array, dimensioned NWRK, of type INTEGER) is an integer workspace
array.  Because of the way in which they are used, RWRK and IWRK may be
EQUIVALENCEd (and, to save space, they should be).
.IP NWRK 12
(an input expression of type INTEGER) is the length of the workspace array(s).
The routines that produce trapezoids use a lot less workspace than the ones
that produce polygons.  The amount of space used at a particular time during
the execution of one of these routines is roughly ten or eleven times the
number of intersections of a horizontal "scan line" with the edge segments
making up the input polygons, plus about three times the number of points
above the current scan line that are "local minima".  (A "local minimum" is
a point on one of the polygons that is connected to two other points on the
polygon having larger Y coordinates than its own).  I have not yet developed
a rule of thumb for setting the value of NWRK.  Ultimately, I would like to
at least put in an internal parameter that will tell one how much space was
actually used on a given call, but I have not yet done so.
.IP URPT 12
is the name of a user-provided routine to process the trapezoids.  This name
must appear in an EXTERNAL statement in the routine that calls PPUNTR and
the routine itself must have the following form:
.sp
.nf
     SUBROUTINE URPP (XCBL,XCBR,YCOB,DXLE,DXRE,YCOT)
       ...(code to process a trapezoid)...
       RETURN
     END
.fi
.sp
The bottom and top of the trapezoid are horizontal (parallel to the X axis);
the arguments XCBL and XCBR define the X coordinates of its bottom left and
bottom right corners, YCOB is the Y coordinate of its bottom edge, DXLE and
DXRE are the inverses (dx/dy) of the slopes of its left and right edges, and
YCOT is the Y coordinate of its top edge. The corners of the trapezoid are
therefore as follows: (XCBL,YCOB), (XCBR,YCOB), (XCBL+DXLE*(YCOT-YCOB),YCOT),
and (XCBR+DXRE*(YCOT-YCOB),YCOT).
.IP IERR 12
(an output variable of type INTEGER) is returned with the value zero
if no errors occurred in the execution of PPUNTR or with a
small positive value if an error did occur.  The value 1 indicates that a
degenerate clip polygon was detected, the value 2 that a degenerate subject
polygon was detected, and the value 3 that the workspace provided was too
small; values greater than 3 should be reported to the author, as they
probably indicate some problem with the algorithm.  If IERR is returned
non-zero, one can be sure that there have been no calls to URPT only if
IERR = 1 or 2; otherwise, some trapezoids probably have been generated
and delivered to URPT.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The FORTRAN statement
.sp
.nf
CALL PPUNTR (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,RWRK,IWRK,NWRK,URPT,IERR)
.fi
.sp
causes the formation of a union polygon (of the subject and clip polygons)
and the delivery of trapezoids representing the interior of that polygon,
one at a time, to the user-specified trapezoid-processing routine URPT.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: ppex01, tppack, c_ppex01.
.SH ACCESS
To use PPUNTR or c_ppuntr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
polypack, ppdipo, ppditr, ppinpo, ppintr, ppplcl, ppppap, ppunpo,
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
