.\"
.\"	$Id: gacolr.m,v 1.1 1993-03-11 16:26:19 haley Exp $
.\"
.TH GACOLR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GACOLR - To set the color of various parts of the background.
.SH SYNOPSIS
CALL GACOLR (KAXS,KLBL,KMJT,KMNT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gacolr (int kaxs, int klbl, int kmjt, int kmnt)
.SH DESCRIPTION 
.IP KAXS 12
the color index of the axes
.sp
.IP KLBL 12
the color index of the labels
.sp
.IP KMJT 12
the color index of the major ticks and grid lines
.sp
.IP KMNT 12
the color index of the minor ticks and gridlines
.sp
Values less than or equal to zero imply that no call is to be
done to set the color before drawing items of the associated
type. The default value of each of the four parameters is
zero.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use GACOLR load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_gacolr load 
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

