.TH DPGETC 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPGETC -
Retrieves the current character value of an internal parameter.
.SH SYNOPSIS
CALL DPGETC (PNAM,CVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpgetc (char *pnam, char *cval, int len)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.  Other characters
may be used to document the use of the parameter being
retrieved; for example, instead of just \'CRB\', one can use
\'CRB - CHARACTER REPRESENTING BREAK\'.
.IP CVAL 12
(an output variable of type CHARACTER) is the name of the variable
into which the value of the internal parameter specified by PNAM
is to be retrieved.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exception:
.IP len 12
The size of cval as dimensioned in the calling program.
.SH USAGE
This routine allows you to retrieve the current value of
Dashpack parameters.  For a complete list of parameters available
in this utility, see the dashpack_params man page.
.SH ACCESS
To use DPGETC or c_dpgetc, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
dpgeti,
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
