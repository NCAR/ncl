.TH DPGETI 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPGETI - Retrieves the integral value of an internal parameter of type
INTEGER or REAL.
.SH SYNOPSIS
CALL DPGETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpgeti (char *pnam, int *ival)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.  Other characters
may be used to document the use of the parameter being
retrieved; for example, instead of just \'DPL\', one can use
\'DPL - DASH PATTERN LENGTH\'.
.IP IVAL 12
(an output variable of type INTEGER) is the name of the variable
into which the value of the internal parameter specified by PNAM
is to be retrieved.
If the internal parameter is a value "i" of type INTEGER, the value returned
is "i".
If the internal parameter is a value "r" of type REAL, the value returned is
"INT(r)".
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
Dashpack parameters.  For a complete list of parameters available
in this utility, see the dashpack_params man page.
.SH ACCESS
To use DPGETI or c_dpgeti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashpack,
dashpack_params,
dpcurv,
dpdraw,
dpfrst,
dpgetc,
dpgetr,
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
None.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
