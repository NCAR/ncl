.\"
.\"	$Id: gpl.m,v 1.1 1993-03-11 16:22:21 haley Exp $
.\"
.TH GPL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GPL (Polyline) - This output primitive draws
line segments connecting a sequence of user-specified coordinate pairs.
.SH SYNOPSIS
CALL GPL (N, X, Y)
.SH DESCRIPTION
.IP N 12
(Integer, Input) - The number of points in the line to 
be drawn. N must be larger than one.
.IP "X (N)" 12
(Real Array, Input) - The X coordinates (specified in world
coordinates) of the N points to be connected by line segments.  
.IP "Y (N)" 12
(Real Array, Input) - The Y coordinates (specified in world coordinates)
of the N points to be connected by line segments.
.SH USAGE
Note that the coordinate pairs must be in world coordinates and not
user coordinates.  Among other things, this means that the log scaling
and mirror-imaging features available via the SET call and the SPPS
functions for drawing lines are not applicable here.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gsln, gslwsc, gscr, gsplci, gqln, gqlwsc, gqplci, dashline, set
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics";
"The Use of X/Y Coordinates in NCAR Graphics" SCD User Document"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
