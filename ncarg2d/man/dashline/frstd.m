.\"
.\"	$Id: frstd.m,v 1.1 1993-04-06 19:38:47 haley Exp $
.\"
.TH FRSTD 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
FRSTD - put pen down at a point in user coordinates
.SH SYNOPSIS
CALL FRSTD (X, Y)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_frstd (float x, float y)
.SH DESCRIPTION 
.IP X 12
X position of point in user coordinates
.IP Y 12
Y position of point in user coordinates
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use FRSTD load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_frstd load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
curved, dashline, dashdb, dashdc, frstd, lined, vectd, ncarg_cbind
.sp
Hardcopy:
"NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

