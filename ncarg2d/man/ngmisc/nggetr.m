.TH NGGETR 3NCARG "April 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGGETR - 
Gets the current real value of an internal parameter of Ngmisc
of type REAL.
See the
ngmisc_params man page for a complete list of all
the Ngmisc internal parameters.
.SH SYNOPSIS
CALL NGGETR (PNAM, RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_nggetr (char *pnam, float *rval)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the
name of the parameter whose value is to be retrieved. Only
the first two characters of the string are examined.
.IP RVAL 12 
(an output variable of type REAL) is the value of the
internal parameter. 
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
Ngmisc parameters.  For a complete list of parameters available
in this utility, see the ngmisc_params man page.
.SH ACCESS
To use NGGETR, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.  To use c_nggetr, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
ngmisc_params,
ngsetr,
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
Copyright (C) 1987-1996
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
