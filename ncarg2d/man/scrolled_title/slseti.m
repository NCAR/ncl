.TH SLSETI 3NCARG "July 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SLSETI - 
Provides a new integer value for an internal parameter of Scrolled_title
of type INTEGER or REAL.
See the scrolled_title_params man page
for a complete list of all the
Scrolled_title internal parameters.
.SH SYNOPSIS
CALL SLSETI (PNAM, IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_slseti (char *pnam, int ival)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the
name of the parameter to be set. Only the first three
characters of the string are examined.
.IP IVAL 12
(an input expression of type INTEGER) is the desired
value of the internal parameter. If the internal parameter is
of type INTEGER, it will be given the value IVAL.   If the internal
parameter is of type REAL, it will be given the value REAL(IVAL).
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Scrolled_title parameters.  For a complete list of parameters available
in this utility, see the scrolled_title_params man page.
.SH ACCESS
To use SLSETI or c_slseti, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.
.SH MESSAGES
See the scrolled_title man page for a description of all Scrolled_title error
messages and/or informational messages.
.SH SEE ALSO
Online:
ftitle,
scrolled_title,
scrolled_title_params,
slgeti,
slgetr,
slogap,
slrset,
slsetr,
stitle,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
