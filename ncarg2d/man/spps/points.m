.TH POINTS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
POINTS - draws a series of markers at the locations (PX(I),PY(I),I=1,NP).
The markers can also be connected by drawing lines between them.
.SH SYNOPSIS
CALL POINTS (PX,PY,NP,IC,IL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_points (float *px, float *py, int np, int ic, int il)
.SH DESCRIPTION 
.IP PX 12
(an input array of type REAL) defining the X user coordinates
where a series of NP markers are to be drawn.
.IP PY 12
(an input array of type REAL) defining the Y user coordinates
where a series of NP markers are to be drawn.
.IP NP 12
(an input parameter of type INTEGER) the value of which is the number
of markers to be drawn.
.IP IC 12
(an input parameter of type INTEGER) which describes the type of marker
to be drawn based upon the following options:
.sp
IC < 0, draw a GKS polymarker of type -IC where
.sp
.nf
    IC = -1 is a dot (.),       -2 is a plus (+),
	    -3 is an asterisk (*), -4 is a circle (o), and
	    -5 is a cross (x).
.fi
.sp
IC specifies the GKS polymarkers type.  The polymarker size and
color can also be set by calling the appropriate
GKS routines, GSMKSC, GSCR, and GSPMCI, before calling POINTS.
.sp
IC = 0, is a dot (.).
.sp
IC > 0, yields the FORTRAN77 character CHAR(IC).
.sp
In this case, the GKS routine GTX is used to draw the specified
character CHAR(IC) at each of the NP points.  Each character is
drawn with the horizontal and vertical text alignment centered
at the specified point with a call to GKS routine GSTXAL.  The
horizontal and vertical text alignment parameters are then restored
to their previous values.  Other text attributes such as character
orientation, character height, and color, can be set by calling
GKS text attribute setting routines before the call to routine POINTS.
.IP IL 12
(a parameter of type INTEGER) which determines if the markers are
to be connected with line segments.
.sp
IL = 0, do not connect the markers with line segments.
.sp
IL .NE. 0, connect the markers with line segments.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
If routine SET has not been called, or if the last call to SET specified
linear mappings in X and Y, and the value of IC specifies a GKS polymarker,
then the GKS routine GPM is called to draw the polymarkers.  If IL is
additionally non-zero, GKS routine GPL is called to draw the line segments.
In all other cases, NCAR Graphics routines are used to draw the more
complicated set of graphical objects.
.sp
Since a call to POINTS can result in subsequent calls to GKS routines
GPM or GPL, various polymarker and polyline attributes may need to
be set before routine POINTS is called.  Polymarker attributes which
can be set are listed above in the DESCRIPTION Section.  Polyline
attributes which can be set include polyline type, GSLN, polyline
line width, GSLWSC, and color, GSCR and GSPLCI.
.sp
A call to routine POINTS causes the PLOTIF buffer to be flushed.
Refer to the man page for routine PLOTIF.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
agex11, cmptra.                                                               |
.SH ACCESS
To use POINTS, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_points, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gpl, gsln, gslwsc, gpm, gsmk, gsmksc, gtx, gstxal, gstxp, gstxfp,
gschh, gschsp, gschup, gschxp, gscr, gstxci, gsplci, gspmci,
spps, spps_params, plotif, ngdots, point, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
