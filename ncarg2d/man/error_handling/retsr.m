.TH RETSR 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
RETSR - Called by a user to return to a saved state of the recovery mode in
NCAR Graphics.
.SH SYNOPSIS
CALL RETSR(IROLD)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_entsr(int irold)
.SH DESCRIPTION 
The FORTRAN statement "CALL RETSR(IROLD)" is normally used to return
the internal error flag of SETER that says whether or not recovery mode
is in effect to the state that it was in before some past call to ENTSR.
.sp
If recovery mode is turned off by a call to RETSR at a time when the
internal error flag is non-zero, this is treated as a fatal error; the
error message is printed, the dump routine FDUM is called, and a STOP
is executed.
.sp
The argument of RETSR is as follows:
.sp
.IP "IROLD" 12
(an input expression of type INTEGER) - Specifies a desired value for the
internal flag that indicates whether recovery mode is in effect or not.
The value 1 turns recovery mode on and the value 2 turns it off.  Normally,
one uses a value of IROLD that was previously returned by a call to ENTSR.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tseter,
arex02.
.SH ACCESS
To use RETSR or c_retsr, load the NCAR Graphics libraries ncarg, 
ncarg_gks, and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
entsr, eprin, errof, error_handling, fdum, icfell, icloem, nerro, semess,
seter, ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
