.TH LBSETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
LBSETR - Gives a real value to an internal parameter of Labelbar.
.SH SYNOPSIS
CALL LBSETR (PNAM, RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_lbsetr (char *pnam, float rval)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) is a string three or
more characters in length, beginning with one of the six 
character strings 'CBL', 'CFL', 'CLB', 'WBL', 'WFL', or 'WLB'.
.IP RVAL 12
(an input expression of type REAL) specifies the desired
value of the parameter.
If the internal parameter is of type INTEGER, the value INT(RVAL) will be
given to it.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of Labelbar parameters. For a complete list of parameters available in this utility, see the labelbar_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: cpex05, elblba, and tlblba.
.SH ACCESS
To use LBSETR or c_lbsetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the labelbar man page for a description of all Labelbar error
messages and/or informational messages.
.SH SEE ALSO
Online:
labelbar, labelbar_params, lbfill, lbgeti, lbgetr, lblbar,
lbseti, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
