.TH STGETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STGETR - 
Gets the current value of an internal
parameter of type REAL.
.SH SYNOPSIS
CALL STGETR (CNM,RVL) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_stgetr(char *whch, float *rval)
.SH DESCRIPTION 
.IP CNM 12
(CHARACTER, input) is the name of a parameter whose
real value is to be retrieved. Only the first three
characters of CNM are examined. The three characters must
either be entirely upper or entirely lower case; mixed case
is not recognized. It is recommended that the rest of the
character string be used to improve the readability of the
code. For example, instead of just \'ARL\', use \'ARL -
Arrowhead Length\'.
.IP RVL 12
(REAL, output) is a variable in which the value of the
parameter specified by CNM is to be returned.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of Streamlines
parameters. For a complete list of parameters available in this utility,
see the streamlines_params man page.
.SH ACCESS
To use STGETR, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_stgetr, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH MESSAGES
See the streamlines man page for a description of all Streamlines error
messages and/or informational messages.
.SH SEE ALSO
Online:
streamlines,
streamlines_params,
ezstrm,
fx,
fy,
stgeti,
stinit,
stream,
strmln,
strset,
stseti,
stsetr,
stuixy,
stumsl,
stumta,
stumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
