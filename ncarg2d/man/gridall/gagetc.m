.\"
.\"	$Id: gagetc.m,v 1.1 1993-03-11 16:26:23 haley Exp $
.\"
.TH GAGETC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GAGETC - @Text
.SH SYNOPSIS
CALL GAGETC (PNAM,CVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gagetc (char *pnam, char *cval, int len)
.SH DESCRIPTION 
.IP PNAM 12
@Text
.IP CVAL 12
@Text
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions with the following exception:
.sp
.IP len 12
The size of cval as dimensioned in the calling program.
.SH ACCESS
To use GAGETC, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_gagetc, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gacolr, gagetc, gageti, gagetr, gasetc, gaseti, gasetr, grid, gridal,
gridl, halfax, labmod, perim, periml, tick4, ticks, ncarg_cbind
.sp
Hardcopy:  
"NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
