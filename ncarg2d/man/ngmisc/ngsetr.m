.TH NGSETR 3NCARG "April 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGSETR - 
Provides a new real value for an internal parameter of Ngmisc
of type REAL.  See the ngmisc_params man page for a complete list of all the
Ngmisc internal parameters.
.SH SYNOPSIS
CALL NGSETR (PNAM, RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngsetr (char *pnam, float rval)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the
name of the parameter to be set. Only the first two
characters of the string are examined.
.IP RVAL 12
(an input expression of type REAL) is the desired
value of the internal parameter.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Ngmisc parameters.  For a complete list of parameters available
in this utility, see the ngmisc_params man page.
.SH ACCESS
To use NGSETR, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, ncarg_c, and ncarg_loc, preferably in that order.  To use c_ngsetr, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ngmisc_params,
nggetr,
ngsetc,
nggetc,
ngseti,
nggeti,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
