.TH VVRSET 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VVRSET - 
Resets all parameters to their initial default values.
.SH SYNOPSIS
CALL VVRSET 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vvrset( void )
.SH DESCRIPTION 
None.
.SH USAGE
This routine resets all Vectors parameters to their default values.
For a complete list of the parameters affected and their default
values, see the vectors_params man page.
.SH ACCESS
To use VVRSET or c_vvrset, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
vectors,
vectors_params,
vvectr,
vvgetc,
vvgeti,
vvgetr,
vvinit,
vvsetc,
vvseti,
vvsetr,
vvudmv,
vvumxy,
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
