.TH VVRSET 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
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
This routine allows you to set the current value of
Vectors parameters.  For a complete list of parameters available
in this utility, see the vectors_params man page.
.SH ACCESS
To use VVRSET, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_vvrset, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
vectors,
vectors_params,
ezvec,
fx,
fy,
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
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
