.\"
.\"	$Id: gsfais.m,v 1.1 1993-03-11 16:23:56 haley Exp $
.\"
.TH GSFAIS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSFAIS (Set fill area interior style) - sets the fill style
of polygons drawn with GFA.
.SH SYNOPSIS
CALL GSFAIS (INTS)
.SH DESCRIPTION
.IP INTS 12
(Integer, Input) - Gives the style of fill to be used 
in subsequent calls to the GFA output primitive. Options are:
.RS
.IP 0 
Hollow fill (the default)
.IP 1 
Solid fill
.IP 2 
Pattern fill (not implemented in NCAR GKS).
.IP 3 
Hatch fill
.SH USAGE
For fill style "Hatch", GSFASI should be used to specify
the type of hatch pattern used (horizontal lines, vertical lines,
etc.)
.sp
Note: If you call GFA and generate an output graphic 
only to find that you have a closed curve with no 
fill, you probably have allowed the fill choice 
to default to hollow fill (no fill).
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gfa, gsfasi, gscr, gsfaci, gqfais, gqfasi, 
areas
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
