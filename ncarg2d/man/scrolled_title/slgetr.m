.TH SLGETR 3NCARG "July 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SLGETR - 
Gets the current real value of an internal parameter of Scrolled_title
of type INTEGER or REAL.
See the
scrolled_title_params man page for a complete list of all
the Scrolled_title internal parameters.
.SH SYNOPSIS
CALL SLGETR (PNAM, RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_slgetr (char *pnam, float *rval)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the
name of the parameter whose value is to be retrieved. Only
the first three characters of the string are examined.
.IP RVAL 12 
(an output variable of type REAL) is the value of the
internal parameter. If the internal parameter is of type
INTEGER and has the value "i", RVAL = REAL(i).  If the
internal parameter is of type REAL and has the value "r",
RVAL = r.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
Scrolled_title parameters.  For a complete list of parameters available
in this utility, see the scrolled_title_params man page.
.SH ACCESS
To use SLGETR or c_slgetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
slogap,
slrset,
slseti,
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
