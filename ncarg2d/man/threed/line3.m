.\"
.\"	$Id: line3.m,v 1.1 1993-03-11 16:34:35 haley Exp $
.\"
.TH LINE3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
LINE3 - draws a line between two points in 3-space.
.SH SYNOPSIS
CALL LINE3 (UA,VA,WA,UB,VB,WB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_line3 (float ua, float va, float wa, float ub, float vb, float wb)
.SH DESCRIPTION 
.IP "UA, VA, WA" 12
coordinates for the first point in 3-space.
.IP "UB, VB, WB" 12
coordinates for the second point in 3-space.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use LINE3 load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_line3 load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.sp
.SH SEE ALSO
Online:
curve3 fence3 frst3 line3 perim3 point3 set3 threed
tick43 vect3 ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
