.TH DPSETI 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPSETI - Sets the value of an internal parameter of type INTEGER or REAL.
.SH SYNOPSIS
CALL DPSETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpseti (char *pnam, int ival)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the name of the
parameter to be set. The name must appear as the first three
characters of the string.  Other characters
may be used to document the use of the parameter being
set; for example, instead of just \'DPL\', one can use
\'DPL - DASH PATTERN LENGTH\'.
.IP IVAL 12
(an input expression of type INTEGER)
is the value to be assigned to the
internal parameter specified by PNAM.
If the internal parameter is of type INTEGER, the value given to it is IVAL.
If the internal parameter is of type REAL, the value given to it is
REAL(IVAL).
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
To use DPSETI or c_dpseti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
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
