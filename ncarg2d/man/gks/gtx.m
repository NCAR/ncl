.\"
.\"	$Id: gtx.m,v 1.1 1993-03-11 16:24:35 haley Exp $
.\"
.TH GTX 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GTX (Text) - The basic output primitive for drawing text.  A number 
of attribute setting routines are available for selecting the size, font, 
precision, orientation, color, character spacing, etc.
.SH SYNOPSIS
CALL GTX (X, Y, STRING)
.SH DESCRIPTION
.IP "X" 12
(Real, Input) - The X world coordinate of the text 
alignment of the first 
character in STRING.
.IP "Y" 12
(Real, Input) - The Y world coordinate of the text
alignment of the first 
character in STRING.
.IP STRING 12
(Character variable, Input) - The characters to be drawn.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gstxp, gstxal, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, gqchxp, plotchar
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
