.TH AGBNCH 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGBNCH - 
Provides an easy way to convert binary dash patterns into
character dash patterns.
.SH SYNOPSIS
AGBNCH (IDSH)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
char *c_agbnch(int idsh)
.SH DESCRIPTION 
.IP IDSH 12
(an input expression of type INTEGER) is between 0 and
65535, inclusive.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH ACCESS 
To use AGBNCH, load the NCAR Graphics libraries ncarg, ncarg_gks, 
ncarg_c, and ncarg_loc, preferably in that order.  To use c_agbnch, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
ncarg_c, and ncarg_loc, preferably in that order.  To get smoother curves, 
drawn using spline interpolation, also load libdashsmth.o.  Or,
you can use the ncargf77 command to compile your program and load 
the above libraries, then, to get smoother curves, use the 
-dashsmth option.
.SH SEE ALSO
Online:
autograph,
agback,
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
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
