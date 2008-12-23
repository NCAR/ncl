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
To use AGBACK or c_agback, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  To get smoother curves, 
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
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
