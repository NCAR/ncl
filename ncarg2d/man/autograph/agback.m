.TH AGBACK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGBACK - 
Draws the background specified by the current values of the
control parameters - the primary parameters with default
values or with values supplied by the user, and the
secondary parameters with values computed by AGSTUP.
.SH SYNOPSIS
CALL AGBACK
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_agback()
.SH DESCRIPTION
None.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
agex13,
cbex01.
.SH ACCESS 
To use AGBACK, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.  To use, c_agback load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.  To get smoother curves, 
drawn using spline interpolation, also load libdashsmth.o.  Or,
you can use the ncargf77 command to compile your program and load 
the above libraries, then, to get smoother curves, use the 
-dashsmth option.
.SH SEE ALSO
Online:
autograph,
agbnch,
agchax,
agchcu,
agchil,
agchnl,
agcurv,
agdshn,
aggetc,
aggetf,
aggeti,
aggetp,
aggetr,
agpwrt,
agrstr,
agsave,
agsetc,
agsetf,
agseti,
agsetp,
agsetr,
agstup,
agutol,
anotat,
displa,
ezmxy,
ezmy,
ezxy,
ezy
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
