.TH AGDSHN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGDSHN - 
Provides an easy way to generate the names of parameters in
the group 'DASH/PATTERN.', for use in calls to AGSETC and
AGGETC.
.SH SYNOPSIS
AGDSHN (IDSH)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
char *c_agdshn (int idsh)
.SH DESCRIPTION
.IP IDSH 12
(an input expression of type INTEGER) is between 1 and
26, inclusive.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
This routine generates parameter names for use in calls
to other routines.
For a complete list of parameters available
in this utility, see the autograph_params man page.
.SH ACCESS 
To use AGDSHN or c_agdshn, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.    To get smoother curves, 
drawn using spline interpolation, also load libdashsmth.o.  Or,
you can use the ncargf77 command to compile your program and load 
the above libraries, then, to get smoother curves, use the 
-dashsmth option.
.SH SEE ALSO
Online:
autograph,
autograph_params,
agback,
agbnch,
agchax,
agchcu,
agchil,
agchnl,
agcurv,
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
