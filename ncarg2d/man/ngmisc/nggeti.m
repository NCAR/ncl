.TH NGGETI 3NCARG "April 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGGETI - 
Gets the current integer value of an internal parameter of Ngmisc 
of type INTEGER.
See the
ngmisc_params man page for a complete list of all
the Ngmisc internal parameters.
.SH SYNOPSIS
CALL NGGETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_nggeti (char *pnam, int *ival)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the
name of the parameter whose value is to be retrieved. Only
the first two characters of the string are examined.
.IP IVAL 12
(an output variable of type INTEGER) is the current
value of the internal parameter. 
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
Ngmisc parameters.  For a complete list of parameters available
in this utility, see the ngmisc_params man page.
.SH ACCESS
To use NGGETI, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.  To use c_nggeti, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
ngmisc_params,
ngseti,
nggetc,
ngsetc,
nggetr,
ngsetr,
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
