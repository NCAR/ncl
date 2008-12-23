.TH DPSETR 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPSETR - Sets the value of an internal parameter of type REAL or INTEGER.
.SH SYNOPSIS
CALL DPSETR (PNAM,RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpsetr (char *pnam, float rval)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the name of the
parameter to be set. The name must appear as the first three
characters of the string.  Other characters
may be used to document the use of the parameter being
set; for example, instead of just \'LS1\', one can use
\'LS1 - LABEL SPACING PARAMETER 1\'.
.IP RVAL 12
(an input expression of type REAL)
is the value to be assigned to the
internal parameter specified by PNAM.
If the internal parameter is of type REAL, the value given to it is RVAL.
If the internal parameter is of type INTEGER, the value given to it is
INT(RVAL).
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Dashpack parameters.  For a complete list of parameters available
in this utility, see the dashpack_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tdshpk.
.SH ACCESS
To use DPSETR or c_dpsetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
dpgetr,
dplast,
dpline,
dpsetc,
dpseti,
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
