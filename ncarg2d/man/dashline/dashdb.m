.\"
.\"	$Id: dashdb.m,v 1.1 1993-04-06 19:38:32 haley Exp $
.\"
.TH DASHDB 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
DASHDB - Chooses a dash pattern without labels.
.SH SYNOPSIS
CALL DASHDB (IPAT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dashdb (int *ipat)
.SH DESCRIPTION 
IPAT is a 16-bit dash pattern (1=solid, 0=blank); e.g.,
1111000011110000 will give dashes of medium length.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the Fortran 
argument description.
.SH ACCESS
To use DASHDB, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_dashdb, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
curved, dashline, dashdb, dashdc, frstd, lined, vectd, ncarg_cbind
.sp
Hardcopy:
"NCAR Graphics User's Guide," Version 2.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
