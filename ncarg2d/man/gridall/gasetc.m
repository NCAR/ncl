.TH GASETC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GASETC - 
Gives a new value to a specified internal parameter
of type CHARACTER.
.SH SYNOPSIS
CALL GASETC (PNAM,CVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gasetc (char *pnam, char *cval)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) is a string
three or more characters in length, the first three
characters of which constitute the name of the internal
parameter whose value is to be set.
.IP CVAL 
(an input expression of type CHARACTER) 
is the new value to be given to the internal parameter
specified by PNAM.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Gridall parameters.  For a complete list of parameters available
in this utility, see the gridall_params man page.
.SH ACCESS
To use GASETC or c_gasetc, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
gaseti,
gasetr,
grid,
gridal,
gridl,
halfax,
labmod,
perim,
periml,
tick4,
ticks
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
