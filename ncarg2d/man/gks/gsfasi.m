.\"
.\"	$Id: gsfasi.m,v 1.1 1993-03-11 16:23:58 haley Exp $
.\"
.TH GSFASI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSFASI (Set fill are style index) - sets the fill area style index.
.SH SYNOPSIS
CALL GSFASI (STYLI)
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
areas
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

