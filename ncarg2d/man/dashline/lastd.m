.\"
.\"	$Id: lastd.m,v 1.1 1993-04-06 19:38:49 haley Exp $
.\"
.TH LASTD 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
LASTD - finishes drawing a line with DASHLINE if necessary.
.SH SYNOPSIS
CALL LASTD 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_lastd()
.SH USAGE
When using FRSTD and VECTD, LASTD may be called.  If the dashed
line package was leaving space for characters and the line ended
before there was enough space for the characters, the space left
will be filled in if LASTD is called.
.SH ACCESS
To use LASTD load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_lastd load 
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

