.TH GASETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GASETR - Gives a real value to an internal parameter of Gridall.
.SH SYNOPSIS
CALL GASETR (PNAM,RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gasetr (char *pnam, float rval)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) is a string
three or more characters in length, the first three
characters of which constitute the name of the internal
parameter whose value is to be set.
.IP RVAL 12
(an input expression of type REAL) 
is the new value to be given to the internal parameter
specified by PNAM.
If the internal parameter is of type INTEGER, the value INT(RVAL) will be
given to it.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Gridall parameters.  For a complete list of parameters available
in this utility, see the gridall_params man page.
.SH ACCESS
To use GASETR or c_gasetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the gridall man page for a description of all Gridall error
messages and/or informational messages.
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
grid,
gridal,
gridl,
halfax,
labmod,
perim,
periml,
tick4,
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
