.TH DPGETR 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPGETR - Retrieves the real value of an internal parameter of type REAL or
INTEGER.
.SH SYNOPSIS
CALL DPGETR (PNAM,RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpgetr (char *pnam, float *rval)
.SH DESCRIPTION 
.IP PNAM 12
(an input expression of type CHARACTER) specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.  Other characters may be used to document
the use of the parameter being
retrieved; for example, instead of just \'LS1\', one can use
\'LS1 - LABEL SPACING PARAMETER 1\'.
.IP RVAL 12
(an output variable of type REAL) is the name of the variable
into which the value of the internal parameter specified by PNAM
is to be retrieved.
If the internal parameter is a value "r" of type REAL, the value returned
is "r".
If the internal parameter is a value "i" of type INTEGER, the value returned
is "REAL(i)".
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
Dashpack parameters.  For a complete list of parameters available
in this utility, see the dashpack_params man page.
.SH ACCESS
To use DPGETR or c_dpgetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the dashpack man page for a description of all Dashpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
dashpack,
dashpack_params,
dpcurv,
dpdraw,
dpfrst,
dpgetc,
dpgeti,
dplast,
dpline,
dpsetc,
dpseti,
dpsetr,
dpsmth,
dpvect,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
