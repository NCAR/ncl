.\"
.\"	$Id: tick4.m,v 1.1 1993-03-11 16:26:52 haley Exp $
.\"
.TH TICK4 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
TICK4 - controls tick mark length and direction.
.SH SYNOPSIS
CALL TICK4 (LMJX, LMNX, LMJY, LMNY)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tick4 (int lmjx, int lmnx, int lmjy, int lmny)
.SH DESCRIPTION 
.IP LMJX 12
the lengths, in plotter address units of major ticks on
the X axes.  The default value is 12.
.IP LMNX 12
the lengths, in plotter address units of minor ticks on
the X axes.  The default value is 8.
.IP LMJY 12
the lengths, in plotter address units of major ticks on
the Y axes.  The default value is 12.
.IP LMNY 12
the lengths, in plotter address units of minor ticks on
the Y axes.  The default value is 8.
By default tick marks point inward.  Negative values of LMJX,
LMNX, LMJY, and LMNY can  be used to create outward pointing
tick marks.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use TICK4 load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_tick4 load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gacolr, gagetc, gageti, gagetr, gasetc, gaseti, gasetr, grid, gridal,
gridl, halfax, labmod, perim, periml, tick4, ticks, ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

