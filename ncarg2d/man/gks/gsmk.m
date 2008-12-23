.\"
.\"	$Id: gsmk.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GSMK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSMK (Set marker type) - sets the type of polymarker to be used in
subsequent GPM calls.
.SH SYNOPSIS
CALL GSMK (MTYPE)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_marker_type(Gint marker_type);
.SH DESCRIPTION
.IP MTYPE 12
(Integer, Input) - Selects the type of marker to be drawn. 
Options are:
.RS
.IP "< 0"  
implementation dependent (not 
used in NCAR GKS-0A)
.IP "  1" 
 . (dot)
.IP "  2" 
 + (plus)
.IP "  3" 
 * (asterisk) This is the default
.IP "  4" 
 o (circle)
.IP "  5" 
 X (cross)
.IP "\(>= 6"
 reserved for registration or future standardization
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online:
gpm, gsmksc, gscr, gspmci, gqmk, gqmksc, gqpmci, 
point, points, ngdots, gset_marker_type
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
