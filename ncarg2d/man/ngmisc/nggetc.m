.TH NGGETC 3NCARG "April 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGGETC - Used to retrieve the current value of a specified internal parameter.
.SH SYNOPSIS
CALL NGGETC (CNP,CVP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_nggetc (char *cnp, char *cvp, int len)
.SH DESCRIPTION 
.IP CNP 12
(an input expression of type CHARACTER) is the name of an internal parameter
of Ngmisc.  Only the first two characters of the string are examined.
.IP CVP 12
(an output variable of type CHARACTER) is the name of a variable into which
the value of the parameter specified by CNP is to be retrieved.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exception:
.IP len 12
The size of cvp as dimensioned in the calling program.
.SH USAGE
This routine allows you to retrieve the current value of Ngmisc
parameters. For a complete list of parameters available in this
utility, see the ngmisc_params man page.
.SH ACCESS
To use NGGETC, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  To use c_nggetc, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online: 
ngmisc_params,
ngsetc,
ngseti,
nggeti,
ngsetr,
nggetr,
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
