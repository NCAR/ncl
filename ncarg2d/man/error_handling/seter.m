.TH SETER 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SETER - Called by NCAR Graphics routines when errors occur.
.SH SYNOPSIS
CALL SETER(MESSG,NERRF,ILEVL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_seter(char *messg, int nerrf, int ilevl)
.SH DESCRIPTION 
The FORTRAN statement "CALL SETER (MESSG,NERRF,ILEVL) is used to tell the
error-handling package that an error has occurred. Exactly what happens as
a result of such a call depends not only on the values of the arguments, but
on the settings of two internal variables: the error flag, which says whether
or not there was a prior error, and the recovery-mode flag, which says whether
or not recovery mode is in effect.
.IP " -" 4
If the internal error flag is 0 (no prior error) and the internal recovery-mode
flag is 2 (recovery mode not set, which is the default), SETER prints the error
message MESSG, calls FDUM if and only if ILEVL = 2, and then STOPs.
.IP " -" 4
If the internal error flag is 0 (no prior error) and the internal recovery-mode
flag is 1 (recovery mode set), then what happens depends on the value of ILEVL:
if ILEVL = 2 (fatal error), SETER prints the error message MESSG, calls FDUM,
and STOPs, but, if ILEVL = 1 (recoverable error), SETER resets the internal
error flag equal to NERRF, remembers the error message MESSG, and RETURNs. In
the latter case, the user is expected to detect the fact that an error has
occurred, take the necessary remedial action, and call ERROF to turn the
internal error flag off.
.IP " -" 4
If the internal error flag is non-zero (prior error), then the value of the
internal recovery-mode flag will be 1 (recovery mode set); in this case, it
doesn't make any difference what the value of ILEVL is: SETER prints both the
remembered error message from the prior error and the new error message from
MESSG, calls FDUM, and STOPs.
.PP
The arguments of SETER are as follows:
.sp
.IP "MESSG" 12
(an input variable or constant of type CHARACTER) is an error message
describing the error that has occurred. This should be of the form
"XXXXXX - TEXT DESCRIBING THE ERROR", where "XXXXXX" is the name of
the routine in which the error occurred, and it should not be more
than 113 characters long.
.IP "NERRF" 12
(an input expression of type INTEGER) is a non-zero error number in the
range from -999 to +9999.
.IP "ILEVL" 12
(an input expression of type INTEGER) is the "error level" - either a 1
or a 2, implying that the error described by MESSG and NERRF is recoverable
or fatal, respectively.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tseter,
arex02.
.SH ACCESS
To use SETER or c_seter, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
entsr, eprin, errof, error_handling, fdum, icfell, icloem, nerro, retsr, semess,
ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
