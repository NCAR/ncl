.TH NGSETC 3NCARG "April 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGSETC - Used to set the character value of a specified internal parameter.
.SH SYNOPSIS
CALL NGSETC (CNP, CVP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngsetc (char *cnp, char *cvp)
.SH DESCRIPTION 
.IP CNP 12
(an input expression of type CHARACTER) is the name of an internal parameter
of Ngmisc.  Only the first two characters will be examined.
.IP CVP 12
(an input expression of type CHARACTER) is the desired new
value of the parameter specified by CNP. This value will be 
used until the next call resetting it.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the 
FORTRAN argument descriptions.
.SH USAGE
This routine allows you to set the current value of Ngmisc
parameters. For a complete list of parameters available in
this utility, see the ngmisc_params man page.
.SH ACCESS
To use NGSETC, load the NCAR Graphics libraries ncarg, 
ncarg_gks, and ncarg_c, preferably in that order.  To use
c_ngsetc, load the NCAR Graphics libraries ncargC, ncarg_gksC,
ncarg, ncarg_gks, and ncarg_c, preferably in that order.
.SH SEE ALSO
Online: 
ngmisc_params,
nggetc,
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

