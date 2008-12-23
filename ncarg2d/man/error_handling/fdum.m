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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
