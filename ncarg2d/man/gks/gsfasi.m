.\"
.\"	$Id: gsfasi.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GSFASI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSFASI (Set fill are style index) - sets the fill area style index.
.SH SYNOPSIS
CALL GSFASI (STYLI)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_fill_style_ind(Gint fill_style_ind);
.SH DESCRIPTION
.IP STYLI 12
(Integer, Input) - 
Specifies workstation-dependent options for hatch or pattern fills. 
Pattern fill options are not supported in NCAR Graphics. Hatch fill 
options are:
.RS
.IP 1 
Horizontal lines (default)
.IP 2 
Vertical lines
.IP 3 
Lines of positive slope
.IP 4 
Lines of negative slope
.IP 5 
Horizontal and vertical lines
.IP 6 
Lines of positive and negative 
slope
.RS
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gfa, gsfais, gscr, gsfaci, gqfais, gqfasi, 
areas, gset_fill_style_ind
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
