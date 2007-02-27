.TH FDUM 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
FDUM - A routine called when fatal errors occur.
.SH SYNOPSIS
CALL FDUM
.SH DESCRIPTION 
This routine is not intended to be called by the user.  It is called by SETER
when a fatal error is being processed and a STOP is about to be executed.  The
default version of FDUM does nothing.  A user-supplied replacement version may
take some other (system-dependent or program-dependent) action which would be
helpful in diagnosing what may have caused the error to occur.
.sp
FDUM has no arguments.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tseter,
arex02.
.SH ACCESS
To use FDUM, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
entsr, eprin, errof, error_handling, icfell, icloem, nerro, retsr, semess,
seter, ncarg_cbind
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
