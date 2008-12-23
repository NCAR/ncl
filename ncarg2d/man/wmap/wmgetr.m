.TH WMGETR 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
WMGETR - Retrieves the real value of an internal parameter of type REAL.
.SH SYNOPSIS
CALL WMGETR (PNAM,RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_wmgetr (char *pnam, float *rval)
.SH DESCRIPTION 
.IP PNAM 12
(an input expression of type CHARACTER) specifies the name of the
parameter to get. The name must appear as the first three
characters of the string.
.IP RVAL 12
(an output variable of type REAL) is the name of the variable
into which the value of the internal parameter specified by PNAM
is to be retrieved.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
Wmap parameters.  For a complete list of parameters available
in this utility, see the wmap_params man page.
.SH ACCESS
To use WMGETR or c_wmgetr, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmlabs, wmsetc, wmseti, wmsetr, wmap_params
.sp
Hardcopy: 
WMAP - A Package for Producing Daily Weather Maps and Plotting Station 
Model Data
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
