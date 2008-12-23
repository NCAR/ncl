.TH PPUNPO 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PPUNPO - generates and returns the boundary of the "union" polygon, which
consists of all points that are inside either or both of the clip polygon
and the subject polygon.
.SH SYNOPSIS
CALL PPUNPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,RWRK,IWRK,NWRK,URPP,IERR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
.nf
void c_ppunpo(
     float *xccp,
     float *yccp,
     int nccp,
     float *xcsp,
     float *ycsp,
     int ncsp,
     float *rwrk,
     int *iwrk,
     int nwrk,
     int (*urpp_)( float *xcra, float *ycra, int *ncra),
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
It is a bit difficult to describe how much space might be required. At the
moment, I would recommend using NWRK equal to about ten times the total of
the number of points in the input polygons and the number of intersection
points. This situation will change with time; at the very least, I would
like to put in an internal parameter that will tell one how much space was
actually used on a given call, but I have not yet done so.
.IP URPP 12
is the name of a user-provided routine to process the polygon-boundary pieces.
This name must appear in an EXTERNAL statement in the routine that calls
PPUNPO and the routine itself must have the following form:
.sp
.nf
     SUBROUTINE URPP (XCRA,YCRA,NCRA)
       DIMENSION XCRA(NCRA),YCRA(NCRA)
       ...(code to process a polygon boundary piece)...
       RETURN
     END
.fi
.sp
Each of the arguments XCRA and YCRA is a real array, dimensioned NCRA; the
former holds the X coordinates, and the latter the Y coordinates, of a piece
of the polygon boundary. It will be the case that XCRA(NCRA)=XCRA(1) and
YCRA(NCRA)=YCRA(1).
.IP IERR 12
(an output variable of type INTEGER) is returned with the value zero if no
errors occurred in the execution of PPUNPO or with a small positive value
if an error did occur.  The value 1 indicates that a degenerate clip polygon
was detected, the value 2 that a degenerate subject polygon was detected,
and the value 3 that the workspace provided was too small; values greater
than 3 should be reported to the author, as they indicate some problem with
the algorithm. Currently, if IERR is returned non-zero, one can be sure that
no calls to URPP were executed; in the future, this could change, but, in
that case, there will be an internal parameter allowing one to request the
current behavior.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The FORTRAN statement
.sp
.nf
CALL PPUNPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,RWRK,IWRK,NWRK,URPP,IERR)
.fi
.sp
causes the formation of a union polygon (of the subject and clip polygons)
and the delivery of that polygon's boundary, piece by piece, to the
user-specified polygon-processing routine URPP.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: ppex01, tppack, c_ppex01.
.SH ACCESS
To use PPUNPO or c_ppunpo, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
polypack, ppdipo, ppditr, ppinpo, ppintr, ppplcl, ppppap, ppuntr,
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
