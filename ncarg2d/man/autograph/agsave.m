.TH AGSAVE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGSAVE - 
Saves the current state of Autograph for later restoration
by AGRSTR.
.SH SYNOPSIS
CALL AGSAVE (IFNO)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_agsave (int ifno)
.SH DESCRIPTION
.IP IFNO 12
(an input expression of type INTEGER) is the number of
a unit to which a single unformatted record is to be
written. It is the user's responsibility to position this
unit. AGSAVE does not rewind it, either before or after
writing the record.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
This routine saves internal parameter values to a file.
For a complete list of parameters available
in this utility, see the autograph_params man page.
.SH ACCESS 
To use AGSAVE or c_agsave, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.    To get smoother curves, 
drawn using spline interpolation, also load libdashsmth.o.  Or,
you can use the ncargf77 command to compile your program and load 
the above libraries, then, to get smoother curves, use the 
-dashsmth option.
.SH MESSAGES
See the autograph man page for a description of all Autograph error
messages and/or informational messages.
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
agdshn,
aggetc,
aggetf,
aggeti,
aggetp,
aggetr,
agpwrt,
agrstr,
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
