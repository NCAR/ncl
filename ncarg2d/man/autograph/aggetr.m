.TH AGGETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGGETR - 
Allows a user program to retrieve the real value of a
single parameter.
.SH SYNOPSIS
CALL AGGETR (TPGN,FUSR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_aggetr (char *tpgn, float *fusr)
.SH DESCRIPTION
.IP TPGN 12
(an input expression of type CHARACTER) is a parameter
identifier, as described in the AGGETP man page. If a group of
more than one parameter is specified, only the first
element of that group will be retrieved by the call.
.IP FUSR 12
(an output variable of type REAL) receives the real
value of the parameter specified by TPGN.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
Autograph parameters.  For a complete list of parameters available
in this utility, see the autograph_params man page.
.SH ACCESS 
To use AGGETR or c_aggetr, load the NCAR Graphics libraries ncarg, 
ncarg_gks, and ncarg_c, preferably in that order.    
To get smoother curves, drawn using spline interpolation, also 
load libdashsmth.o.  Or, you can use the ncargf77 command to 
compile your program and load the above libraries, then, to 
get smoother curves, use the -dashsmth option.
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
