.TH Error_handling 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Error_handling - A set of routines for error handling in NCAR Graphics.
.SH SYNOPSIS
ENTSR - Enters recovery mode.
.br
EPRIN - Prints the current error message.
.br
ERROF - Turns off the internal error flag.
.br
FDUM - A dump routine - the default version just RETURNS.
.br
ICFELL - Checks for an outstanding error condition.
.br
ICLOEM - Computes the real length of its character-string argument (ignoring blanks on the end)
.br
NERRO - Gets the current value of the internal error flag.
.br
RETSR - Restores a previous value of the internal error flag.
.br
SEMESS - Gets a specified portion of the current error message.
.br
SETER - Called by NCAR Graphics routines to report error conditions.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_entsr
.br
c_eprin
.br
c_errof
.br
c_icfell
.br
c_icloem
.br
c_nerro
.br
c_retsr
.br
c_semess
.br
c_seter
.sp
.SH "USING SETER IN NCAR GRAPHICS"
There are specific conventions for the use of SETER within NCAR Graphics, as
follows:
.IP " -" 4
All detectable errors shall be recoverable, in the sense described above.
(That is, in every call to SETER, the final argument shall be a 1, rather
than a 2.) This is by request of the folks doing NCAR Interactive, who
rightly consider STOPs in the utilities undesirable. The idea is to let the
user decide what is to be done about the various error conditions.
.IP " -" 4
Whenever an NCAR Graphics routine calls a lower-level routine that might
detect an error and call SETER, it should subsequently use ICFELL to check
the error state; if a recoverable error has occurred, it should first do
required clean-up chores, if possible, and then pass control back to the
routine that called it. In all such uses of ICFELL, the first argument
should be the name of the routine referencing ICFELL and the second
argument should be a new number for the error, reflecting the position
of the reference to the lower-level routine in the upper-level routine.
.IP " -" 4
Any NCAR Graphics routine that can be called by a user and that can
potentially yield a call to SETER must immediately check the error state
and, if that error state is non-zero, return control without doing anything
else. This is most conveniently done using a reference to ICFELL; see the
second example in the "Usage" section of the description of ICFELL. All
such references should have a first argument of the
form 'XXXXXX - UNCLEARED PRIOR ERROR', where "XXXXXX" is the name of
the routine in which the reference occurs, and a second argument equal
to "1".
.IP " -" 4
It is recommended that, within a given utility routine, the error numbers
in references to SETER and ICFELL should start at 1 and increment by 1.
These numbers generally have no intrinsic meaning in and of themselves:
they are merely intended to allow a consultant to find the reference
that generated a given error.
.IP " -" 4
NCAR Graphics routines are not required to turn recovery mode on before
calling a lower-level routine that might call SETER (which was the
convention in the PORT library, as described in the PORT document).
Instead, the assumption is that it is the responsibility of the user of
NCAR Graphics to set recovery mode if he/she desires to do recovery. Since,
by default, recovery mode is turned off, all NCAR Graphics calls to SETER
will be treated as fatal: the error message will be printed and execution
will be terminated. Once the user turns recovery mode on, however, no NCAR
Graphics error will be treated in this way except for one that the user
fails to recover from.
.PP
Note: These conventions are being adopted as of December 2, 1993, and
represent a goal for the future. The current situation is somewhat muddled:
In some utilities, all SETER calls are fatal ones. In other utilities, some
SETER calls are fatal and some are not. In other utilities, no SETER calls
are fatal. In general, errors at a lower level are not detected and passed
back up the call chain. Users have complained (and rightly so) that error
recovery is, in general, not possible; observance of these conventions
should help to fix the situation.
.PP
Further note: As of March 30, 1994, the situation has improved markedly.
CONPACK and all utilities referenced by it have been updated to follow the
guidelines given above and work is proceeding on other utilities.
.PP
There is one sticky area in which questions remains to be answered: Sometimes,
when an error condition occurs during execution of a utility routine that has
changed the internal state of GKS or SPPS, it has not been possible to restore
the state of those packages to exactly what it was before that routine was
entered. In some cases, better bookkeeping would allow restoration to be
done; in other cases, though, restoration would involve calling a routine
that could generate a call to SETER, which would cause a STOP. In the latter
cases, I have not yet worked out a good solution. For the moment, therefore,
the situation is this: if you call an NCAR Graphics utility with recovery
mode turned on and, when you get control back, you find that an error has
occurred, you must be prepared to deal with the possibility that at least
the following things might been changed: 1) the current SET call; 2) the
current polyline color index; 3) the current polymarker color index; 4)
the current text color index; 5) the current fill area color index; 6) the
current dash pattern.
.SP
.SH ACCESS
To use the Error_handling C or Fortran routines, load the NCAR
Graphics libraries ncarg, ncarg_gks, and ncarg_c, preferably in that
order.
.sp
.SH SEE ALSO
Online:
entsr, eprin, errof, fdum, icfell, icloem, nerro, retsr, semess, seter,
ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
