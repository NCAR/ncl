.\"
.\"	$Id: gsmksc.m,v 1.1 1993-03-11 16:24:09 haley Exp $
.\"
.TH GSMKSC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSMKSC (Set marker size scale factor) - sets the polymarker 
size scale factor -- the relative size of the marker.
.SH SYNOPSIS
CALL GSMKSC (MSZSF)
.SH DESCRIPTION
.IP MSZSF 12
(Real, Input) - A scale factor to control the size of the marker.  
It must be  greater than or equal to 0.  
Since MSZSF = 1.0 by default, setting MSZSF = 2.0 doubles polymarker size.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gpm, gsmk, gscr, gspmci, gqmk, gqmksc, gqpmci, point, points
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
