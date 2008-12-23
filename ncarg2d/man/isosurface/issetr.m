'\" t
.TH ISSETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ISSETR - Resets the current values of internal parameters
of type REAL.
.SH SYNOPSIS
CALL ISSETR (CNP,RVP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_issetr (char *cnp, float rvp)
.SH DESCRIPTION 
.IP CNP 12
(an input expression of type CHARACTER) is the name of
an internal parameter. Only the first two characters of the
name are meaningful, but a longer character string may be
used to more clearly document what internal parameter is
being set.
.IP RVP 12
(an input expression of type REAL)
is the desired new value of the
parameter specified by CNP. This value will be used until
the next call resetting it.
If the internal parameter is inherently of type INTEGER, the
value "INT(RVP)" will be given to it.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Isosurface parameters.  For a complete list of parameters available
in this utility, see the isosurface_params man page.
.SH ACCESS
To use ISSETR or c_issetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the isosurface man page for a description of all Isosurface error
messages and/or informational messages.
.SH SEE ALSO
Online:
isosurface, isosurface_params, ezisos, 
isgeti, isgetr, isosrf, isseti, pwrzi, 
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
