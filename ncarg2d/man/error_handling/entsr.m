.TH ENTSR 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ENTSR - Called by a user to set recovery mode in NCAR Graphics.
.SH SYNOPSIS
CALL ENTSR(IROLD,IRNEW)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_entsr(int *irold, int irnew)
.SH DESCRIPTION 
The FORTRAN statement "CALL ENTSR(IROLD,IRNEW)" is normally used to enter
recovery mode and save the previous value of the internal error-recovery
flag, but it can also be used to exit from recovery mode and save the
previous value of the flag, or to just get the value of the flag, without
changing it.
.sp
If recovery mode is turned off by a call to ENTSR at a time when the
internal error flag is non-zero, this is treated as a fatal error; the
error message is printed, the dump routine FDUM is called, and a STOP
is executed.
.sp
The arguments of ENTSR are as follows:
.sp
.IP "IROLD" 12
(an output variable of type INTEGER) - Receives the old value of the internal
flag that indicates whether recovery mode is in effect or not.  In the former
case, the returned value will be a 1; in the latter case, it will be a 2.
Normally, the value returned is saved for a later call to RETSR.
.sp
.IP "IRNEW" 12
(an input expression of type INTEGER) - Specifies what is to be done to the
internal flag that indicates whether recovery mode is in effect or not.  If
IRNEW is a 1 or a 2, it becomes the new value of the internal recovery-mode
flag: the value 1 turns recovery mode on and the value 2 turns it off.  Other
non-zero values are illegal, but, if IRNEW is zero, the state of the internal
recovery-mode flag is not changed.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tseter,
arex02.
.SH ACCESS
To use ENTSR or c_entsr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
eprin, errof, error_handling, fdum, icfell, icloem, nerro, retsr, semess, seter,
ncarg_cbind
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
