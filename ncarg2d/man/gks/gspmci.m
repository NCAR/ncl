.\"
.\"	$Id: gspmci.m,v 1.2 1993-04-02 16:50:38 haley Exp $
.\"
.TH GSPMCI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSPMCI - (Set polymarker color index) - sets the polymarker color index.
.SH SYNOPSIS
CALL GSPMCI (COLI)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_marker_colr_ind(Gint marker_colr_ind);
.SH DESCRIPTION
.IP COLI 12
(Integer, Input) - A color index.
.SH USAGE
All polymarkers drawn with calls to the GPM output primitive
will be drawn with the color associated with index COLI
until GSPMCI is called again and a new index is assigned.
.sp
For all GKS output primitives, color is assigned using a color
index. The color indices run from 0 to 255, where 0 is the background
color index and 1 is the foreground color index.  Color values
are associated with indices by calls to the GKS routine GSCR.
If a color index is used that has no user-assigned color value
set in a GSCR call, then a device-dependent color value will
be assigned to that index.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gpm, gsmk, gsmksc, gscr, gqmk, gqmksc, gqpmci, 
point, points, gset_marker_colr_ind
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
