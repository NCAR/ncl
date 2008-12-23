.TH TICK4 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TICK4 - 
Allows for program control of tick mark length and
direction.
.SH SYNOPSIS
CALL TICK4 (LMJX, LMNX, LMJY, LMNY)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tick4 (int lmjx, int lmnx, int lmjy, int lmny)
.SH DESCRIPTION 
All arguments are in plotter address units (PAUs). By
default, one PAU is 1/1023rd of the width of the plotter
frame.
.sp
Note: A call to the SPPS routine SETI will change the
definition of a PAU. Such calls are no longer recommended,
but the possibility still exists.
.IP "LMJX and LMNX" 12 
(input expressions of type INTEGER) are the
lengths, in plotter address units, of major and minor ticks
on the X axis. The default values are 12 and 8.
.IP "LMJY and LMNY" 12
(input expressions of type INTEGER) are the
lengths, in plotter address units, of major and minor ticks
on the Y axis. The default values are 12 and 8.
.IP "" 0
By default, tick marks point inward. Negative values of
LMJX, LMNX, LMJY, and LMNY may be used to create outward-pointing
tick marks.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Gridall parameters.  For a complete list of parameters available
in this utility, see the gridall_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
tgrida.
.SH ACCESS
To use TICK4 or c_tick4, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gridall,
gridall_params,
gacolr,
gagetc,
gageti,
gagetr,
gasetc,
gaseti,
gasetr,
grid,
gridal,
gridl,
halfax,
labmod,
perim,
periml,
ticks,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
