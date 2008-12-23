'\" t
.TH ISGETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ISGETR - Gets the current values of internal parameters
of type REAL.
.SH SYNOPSIS
CALL ISGETR (CNP,RVP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_isgetr (char *cnp, float *rvp)
.SH DESCRIPTION 
.IP CNP 12
(an input expression of type CHARACTER) is the name of
an internal parameter. Only the first two characters of the
name are meaningful, but a longer character string may be
used to more clearly document what internal parameter is
being retrieved.
.IP RVP
(an output variable of type REAL)
is the name of a variable into which
the value of the parameter specified by CNP is to be
retrieved.
If the internal parameter is inherently an integer
value "i", "REAL(i)" will be returned as the value of RVP.
.SH USAGE
This routine allows you to retrieve the current value of
Isosurface parameters.  For a complete list of parameters available
in this utility, see the isosurface_params man page.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use ISGETR or c_isgetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the isosurface man page for a description of all Isosurface error
messages and/or informational messages.
.SH SEE ALSO
Online:
isosurface, isosurface_params, ezisos, 
isgeti, isosrf, isseti, issetr, pwrzi, 
ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
