.\"
.\"	$Id: gaseti.m,v 1.1 1993-03-11 16:26:34 haley Exp $
.\"
.TH GASETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GASETI - @Text
.SH SYNOPSIS
CALL GASETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gaseti (char *pnam, int ival)
.SH DESCRIPTION 
.IP PNAM 12
@Text
.IP IVAL 12
@Text
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use GASETI, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_gaseti, load the 
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
