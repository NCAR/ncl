.\"
.\"	$Id: gsfaci.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GSFACI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSFACI (Set fill area color index) - sets the fill area color index.
.SH SYNOPSIS
CALL GSFACI (COLI)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_fill_colr_ind(Gint fill_colr_ind);
.SH DESCRIPTION
.IP COLI 12
(Integer, Input) - A color index.
.SH USAGE
All filled areas drawn with calls to the GFA output primitive
will be drawn with the color associated with index COLI
until GSFACI is called again and a new index is assigned.
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
gfa, gsfais, gsfasi, gscr, gqfais, gqfasi, 
areas, gset_fill_colr_ind
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
