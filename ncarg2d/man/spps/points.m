.TH POINTS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
POINTS - draws a marker at each of a series of specified positions in the
user coordinate system.  The markers can also be connected by drawing lines
between them.
.SH SYNOPSIS
CALL POINTS (PX,PY,NP,IC,IL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_points (float *px, float *py, int np, int ic, int il)
.SH DESCRIPTION 
.IP PX 12
(an input array of type REAL) defines the X user coordinates
where a series of NP markers are to be drawn.
.IP PY 12
(an input array of type REAL) defines the Y user coordinates
where a series of NP markers are to be drawn.
.IP NP 12
(an input expression of type INTEGER) specifies the number
of markers to be drawn.
.IP IC 12
(an input expression of type INTEGER) describes the type of marker
to be drawn, as follows:
.IP " " 12
IC < 0 - draw a GKS polymarker of type -IC, as follows:
.RS
.IP "  -1" 6
requests a dot (.),
.IP "  -2" 6
requests a plus (+),
.IP "  -3" 6
requests an asterisk (*),
.IP "  -4" 6
requests a circle (o), and
.IP "  -5" 6
requests a cross (x).
.RE
.IP " " 12
The polymarker size and color can be set by calling the
GKS routines GSMKSC, GSCR, and GSPMCI before calling POINTS.
.IP " " 12
IC = 0 - draw a dot (.).  Effect is the same as IC = -1.
.IP " " 12
IC > 0 - draw the FORTRAN77 character CHAR(IC).
.IP " " 12
In this case, the GKS routine GTX is used to draw the specified
character CHAR(IC) at each of the NP points.  Each character is
drawn with the horizontal and vertical text alignment centered
at the specified point with a call to the GKS routine GSTXAL.  The
horizontal and vertical text alignment parameters are then restored
to their previous values.  Other text attributes such as character
orientation, character height, and color, can be set by calling
GKS text attribute setting routines before the call to the routine POINTS.
.IP IL 12
(a input expression of type INTEGER) determines if the markers are
to be connected with line segments.
.IP " " 12
IL = 0 - do not connect the markers with line segments.
.IP " " 12
IL .NE. 0 - connect the markers with line segments.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
If the routine SET has not been called, or if the last call to SET specified
linear mappings in X and Y, and the value of IC specifies a GKS polymarker,
then the GKS routine GPM is called to draw the polymarkers.  If, in addition,
IL is non-zero, the GKS routine GPL is called to draw the line segments.  In
all other cases, NCAR Graphics routines are used to draw the more complicated
set of graphical objects.
.sp
Since a call to POINTS can result in subsequent calls to the GKS routines
GPM or GPL, various polymarker and polyline attributes may need to
be set before the routine POINTS is called.  Polymarker attributes which
can be set are listed above in the DESCRIPTION Section.  Polyline
attributes which can be set include polyline type (call GSLN), polyline
line width (call GSLWSC), and color (call GSCR and GSPLCI).
.sp
A call to the routine POINTS causes the SPPS polyline buffer to be flushed.
Refer to the man page for the routine PLOTIF.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
agex11, cmptra.
.SH ACCESS
To use POINTS or c_points, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
