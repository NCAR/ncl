.\"
.\"	$Id: gsln.m,v 1.3 1993-05-03 17:28:20 haley Exp $
.\"
.TH GSLN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSLN (Set line type) - sets the line type to solid or various dashed patterns.
.SH SYNOPSIS
CALL GSLN (LTYPE)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_linetype(Gint linetype);
.SH DESCRIPTION
.IP LTYPE 12
(Integer, Input) - The type of polyline to be drawn.  
Options are:
.RS
.IP "< 0" 
Implementation dependent (not 
used in NCAR GKS-0A)
.IP "1" 
Solid line (default)
.IP "2" 
Dashed line
.IP "3" 
Dotted line
.IP "4" 
Dashed dotted line
.IP "\(>= 5" 
Reserved for registration or 
future standardization (not 
used in NCAR GKS-0A)
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gpl, gslwsc, gscr, gsplci, gqln, gqlwsc, gqplci, 
dashline, gset_linetype
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
