.TH POINT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
POINT - draws a point at a specified position in the user coordinate system.
.SH SYNOPSIS
CALL POINT (PX,PY)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_point (float px, float py)
.SH DESCRIPTION 
.IP PX 12
(an input expression of type REAL) defines the X user coordinate
where the dot is to be drawn.
.IP PY 12
(an input expression of type REAL) defines the Y user coordinate
where the dot is to be drawn.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
If one desires a filled dot of arbitrary size see the routine NGDOTS.
.sp
The GKS routine GPM can also be used to draw a dot in world coordinates;
however, NCAR Graphics user coordinates allow for mirror imaging of axes
and logarithmic axes which GKS does not.  See the SCD User Doc
"NCAR Graphics Fundamentals, UNIX Version" for a description
of these coordinate systems.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
tareas.
.SH ACCESS
To use POINT, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_point, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gpm, gsmk, gsmksc, gscr, gspmci,
spps, spps_params, plotif, ngdots, points, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
