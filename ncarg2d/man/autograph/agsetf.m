.TH AGSETF 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGSETF - 
Allows a user program to store a real number as the value
of a single parameter.
.SH SYNOPSIS
CALL AGSETF (TPGN,FUSR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_agsetf (char *tpgn, float fusr)
.SH DESCRIPTION
.IP TPGN 12
(an input expression of type CHARACTER) is an
parameter identifier, as described in AGSETP man page. If a
group of more than one parameter is specified, only the
first element of that group will be affected by the call.
.IP FUSR 12
(an input expression of type REAL) is the value to be
given to the parameter specified by TPGN.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Autograph parameters.  For a complete list of parameters available
in this utility, see the autograph_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples:
agex05,
agex07,
agex08,
agex10,
agex13,
fagaxmax,
fagcudsh,
fagovrvw.
.SH ACCESS 
To use AGSETF or c_agsetf, load the NCAR Graphics libraries ncarg, 
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
aggetr,
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
agstup,
agutol,
anotat,
displa,
ezmxy,
ezmy,
ezxy,
ezy
.SH COPYRIGHT
Copyright (C) 1987-2007
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
