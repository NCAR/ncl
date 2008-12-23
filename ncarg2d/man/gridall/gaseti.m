.TH GASETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GASETI - Gives an integer value to an internal parameter of Gridall.
.SH SYNOPSIS
CALL GASETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gaseti (char *pnam, int ival)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) is a string
three or more characters in length, the first three
characters of which constitute the name of the internal
parameter whose value is to be set.
.IP IVAL 12
(an input expression of type INTEGER) 
is the new value to be given to the internal parameter
specified by PNAM.
If the internal parameter is of type REAL, the value REAL(IVAL) will be
given to it.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Gridall parameters.  For a complete list of parameters available
in this utility, see the gridall_params man page.
.SH ACCESS
To use GASETI or c_gaseti, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
