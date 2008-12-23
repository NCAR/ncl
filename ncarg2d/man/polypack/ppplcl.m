.TH PPPLCL 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PPPLCL - clips a polyline against a clipping rectangle. It is intended that,
eventually, there should be a better routine that will clip a polyline
against an arbitrary polygon, but that routine has not yet been written.
.SH SYNOPSIS
CALL PPPLCL (XMIN,XMAX,YMIN,YMAX,XCPL,YCPL,NCPL,RWRK,LRWK,URPF,IERR) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
.nf
void c_ppplcl(float xmin,
              float xmax,
              float ymin,
              float ymax,
              float *xcpl,
              float *ycpl,
              int ncpl,
              float *rwrk,
              int lwrk,
              int (*urpf_)(float *xcra,
                           float *ycra,
                           int *ncra),
              int *ierr)
.fi
.SH DESCRIPTION 
.IP XMIN 12
(an input expression of type REAL) is the X coordinate value defining
the position of the left edge of the clipping rectangle.
.IP XMAX 12
(an input expression of type REAL) is the X coordinate value defining
the position of the right edge of the clipping rectangle.
.IP YMIN 12
(an input expression of type REAL) is the Y coordinate value defining
the position of the bottom of the clipping rectangle.
.IP YMAX 12
(an input expression of type REAL) is the Y coordinate value defining
the position of the top of the clipping rectangle.
.IP XCPL 12
(an input array of type REAL) is the X coordinate array for the polyline
that is to be clipped.
.IP YCPL 12
(an input array of type REAL) is the Y coordinate array for the polyline
that is to be clipped.
.IP NCPL 12
(an input expression of type INTEGER) is the number of points defining the
input polyline (the number of meaningful elements in each of the arrays XCPL
and YCPL).
.IP RWRK 12
(a scratch array of type REAL) is a workspace array for PPPLCL to use.  It
is divided into halves; one half is used for X coordinates and the other
half for Y coordinates.  This array doesn't have to be too big unless it
is important not to break the polyline except at the edges of the clipping
rectangle.
.IP LRWK 12
(an input expression of type INTEGER) is the length of the array RWRK.
.IP URPF 12
is the name of a user-provided routine to process the fragments of the polyline
resulting from the clipping process.  This name must appear in an EXTERNAL
statement in the routine that calls PPPLCL and the routine itself must have
the following form:
.sp
.nf
     SUBROUTINE URPF (XCRA,YCRA,NCRA)
       DIMENSION XCRA(NCRA),YCRA(NCRA)
       ...(code to process a polyline)...
       RETURN
     END
.fi
.sp
Each of the arguments XCRA and YCRA is a real array, dimensioned NCRA; the
former holds the X coordinates, and the latter the Y coordinates, of a
polyline fragment.
.IP IERR 12
(an output variable of type INTEGER) is returned with the value zero if no
errors occurred in the execution of PPPLCL or with a small positive value
if an error did occur. The value 1 indicates that NCPL is less than or equal
to zero and the value 2 indicates that LRWK is less than 4.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The FORTRAN statement
.sp
.nf
CALL PPPLCL (XMIN,XMAX,YMIN,YMAX,XCPL,YCPL,NCPL,RWRK,LRWK,URPF,IERR) 
.fi
.sp
clips a polyline against a rectangle and delivers the portions of the
polyline that fall within the rectangle, one by one, to the user-specified
polyline-fragment-processing routine URPF.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: ppex01, tppack, c_ppex01.
.SH ACCESS
To use PPPLCL or c_ppplcl, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
polypack, ppdipo, ppditr, ppinpo, ppintr, ppppap, ppunpo, ppuntr,
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
