.TH STRSET 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
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
This routine allows you to set the current value of Streamlines
parameters. For a complete list of parameters available in this utility,
see the streamlines_params man page.
.SH ACCESS
To use STRSET, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_strset, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
streamlines,
streamlines_params,
ezstrm,
fx,
fy,
stgeti,
stgetr,
stinit,
stream,
strmln,
stseti,
stsetr,
stuixy,
stumsl,
stumta,
stumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
