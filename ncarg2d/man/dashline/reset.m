.TH RESET 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
RESET -
The "super" version of RESET zeroes the internal integer array used to detect
crowded lines; other versions do nothing.
.SH SYNOPSIS
CALL RESET
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_reset()
.SH USAGE
The "super" version of the Dashline utility attempts to cull crowded lines
from the output picture.  It does this using an internal integer array,
ISCREN, in which each bit used represents a single pixel in a square array
of 1024x1024 pixels of the picture.  Initially, all the bits must be set
to zero by calling the routine RESET.  When a line segment is about to be
drawn, an appropriate set of bits in ISCREN (representing the pixels through
which the line segment passes) is examined; if any bit in that set is a 1,
the line segment is not drawn. Then, the bits representing the pixels in the
immediate vicinity of the line segment are set to 1, so as to prevent any
subsequent line segment from being drawn through the area.
.sp
As each label is drawn, the bits in ISCREN representing the area occupied
by the label are set to 1, as well, which prevents subsequent line segments
from being drawn through the label.
.sp
This culling process has the following implications for the user of
the "super" version of Dashline:
.IP \(bu 3
The routine RESET should be called at the beginning of each new picture.
If this is not done, each of a consecutive series of pictures will have
progressively more gaps where line segments have been culled.
.IP \(bu 3
More important lines should be drawn first, followed by less important
lines.  For example, the "super" version of the routine CONREC draws
labeled contour lines first, followed by the contour lines halfway in
between the labeled lines, followed by the rest of the lines.
.IP \(bu
Because each line segment is either drawn in its entirety or not at all,
long straight line segments are broken into shorter pieces.  The internal
parameter MLLINE determines how small the resulting pieces will be; you
may wish to reduce its value so that smaller pieces will be used.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
tdashp, 
fdlsmth.
.SH ACCESS
To use RESET or c_reset, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashline, dashline_params, curved, dashdb, dashdc, frstd,
lastd, lined, vectd, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
