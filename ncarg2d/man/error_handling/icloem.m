.TH ICLOEM 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ICLOEM - Called to get the length of the non-blank portion of an error message.
.SH SYNOPSIS
LNGTH = ICLOEM(MESSG)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_icloem(char *messg)
.SH DESCRIPTION 
The value of the expression "ICLOEM(MESSG)" is the actual length of the
non-blank portion of the input character variable MESSG.  Thus,
"MESSG(1:ICLOEM(MESSG))" is the non-blank part of MESSG.  (It is assumed
that the non-blank part of MESSG is left-justified in the character string,
with blank fill to the right, as is the case for all NCAR Graphics error
messages in calls to SETER.)
.sp
The argument of ICLOEM is as follows:
.sp
.IP "MESSG" 12
(an input variable or constant of type CHARACTER) - Most likely an error
message returned by a reference to the function SEMESS.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tseter,
arex02.
.SH ACCESS
To use ICLOEM or c_icloem, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
entsr, eprin, errof, error_handling, fdum, icfell, nerro, retsr,
semess, seter, ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2000
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
