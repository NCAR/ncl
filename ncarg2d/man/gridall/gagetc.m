.TH GAGETC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GAGETC - Retrieves the value of a specified internal parameter
of type CHARACTER.
.SH SYNOPSIS
CALL GAGETC (PNAM,CVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gagetc (char *pnam, char *cval, int len)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) is a string
three or more characters in length, the first three
characters of which constitute the name of the internal
parameter whose value is to be retrieved.
.IP CVAL 12
(an output variable of type CHARACTER)
is the name of a variable into which the value of the
internal parameter specified by PNAM is to be retrieved.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exception:
.sp
.IP len 12
The size of cval as dimensioned in the calling program.
.SH USAGE
This routine allows you to retrieve the current value of
Gridall parameters.  For a complete list of parameters available
in this utility, see the gridall_params man page.
.SH ACCESS
To use GAGETC or c_gagetc, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the gridall man page for a description of all Gridall error
messages and/or informational messages.
.SH SEE ALSO
Online:
gridall,
gridall_params,
gacolr,
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
