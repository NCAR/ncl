.\"
.\"	$Id: periml.m,v 1.1 1993-03-11 16:26:50 haley Exp $
.\"
.TH PERIML 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
PERIML - @Text
.SH SYNOPSIS
CALL PERIML (MJRX,MNRX,MJRY,MNRY)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_periml (int mjrx, int mnrx, int mjry, int mnry)
.SH DESCRIPTION 
.IP MJRX 12
@Text
.IP MNRX 12
@Text
.IP MJRY 12
@Text
.IP MNRY 12
@Text
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use PERIML, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_periml, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gacolr, gagetc, gageti, gagetr, gasetc, gaseti, gasetr, grid, gridal,
gridl, halfax, labmod, perim, periml, tick4, ticks, ncarg_cbind
.sp
Hardcopy:  
Tutorial: "A Step-by-Step Guide to Contouring and Mapping"; 
"NCAR Graphics User's Guide, Version 2.00";
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
