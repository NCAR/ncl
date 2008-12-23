.TH STRSET 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STRSET - 
Resets all parameters to their initial default values.
.SH SYNOPSIS
CALL STRSET 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_strset(void)
.SH DESCRIPTION 
None.
.SH C-BINDING DESCRIPTION
None.
.SH USAGE
This routine resets all Streamlines parameters to their default
values.  For a complete list of the parameters affected and their
default values, see the vectors_params man page.
.SH ACCESS
To use STRSET or c_strset, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
stgetc,
stgeti,
stgetr,
stinit,
stream,
streamlines,
streamlines_params,
stsetc,
stseti,
stsetr,
stuixy,
stumsl,
stumta,
stumxy,
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
