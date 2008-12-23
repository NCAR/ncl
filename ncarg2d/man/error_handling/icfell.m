.TH ICFELL 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ICFELL - Called to check for existing error conditions and (perhaps) to update
the current error message and error flag.
.SH SYNOPSIS
NERR = ICFELL(MESSG,NERRF)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_icfell(char *messg, int nerrf)
.SH DESCRIPTION 
ICFELL (which stands for "I Check For Errors on Lower Level") is used to
check for the occurrence of a recoverable error in a lower-level routine and
(perhaps) to update the current internal error message and error flag.
.sp
The old value of the error flag is returned as the value of the function
ICFELL. If that value is zero, nothing else has been done. If the value of
ICFELL is non-zero, the following actions have been taken:
.IP " -" 4
If MESSG is blank, the current error message has not been changed.
.IP " -" 4
If MESSG is non-blank and its length is 6 or less, it should be the name of
the routine referencing ICFELL; the current error message has been altered by
prepending first a slash and then MESSG. (Note that, if an error occurs several
levels deep, the effect of using ICFELL in this manner at each level is,
effectively, to generate traceback information in the error message.)
.IP " -" 4
If MESSG is non-blank and its length is 7 or greater, its value has become
the new value of the current error message and the previous error message
has been printed. This is intended for use at the beginning of an NCAR
Graphics routine to check for an outstanding error that the user has not
recovered from and to ensure that the message for the outstanding error
gets printed.
.IP " -" 4
If the expression NERRF has the value zero, the current error flag has not
been changed.
.IP " -" 4
If the expression NERRF has a non-zero value, the value of the current error
flag has been made equal to that value.
.PP
An example: Assume that the routine "A" calls the routine "B" and that "B"
detects an error and calls SETER with error number "32" and error message
"B - ERROR HAS OCCURRED". If recovery mode is not in effect, SETER prints
the error message and STOPs; if recovery mode is in effect, control returns
from "SETER" to "B" and thence to "A". At that point, the statement
"IF (ICFELL('A',13).NE.0) RETURN" detects the fact that an error has occurred
in "B" and results in a return from "A" to whatever routine called it. It also
changes the current error message to read 'A/B - ERROR HAS OCCURRED' and
changes the error number from "32" to "13".
.PP
Another example: Assume that the NCAR Graphics routine "A" is called when
recovery mode is set and that it detects an error, calls SETER, and RETURNs
to the user. If the user neglects to check the error state and calls the
routine "B" next, the statement "IF (ICFELL('B - UNCLEARED PRIOR ERROR',1).NE.0) RETURN"
ensures that the error message from routine "A" will be printed, that it will
be replaced by an error message referring to the routine "B", and that "B"
won't do anything else.
.sp
The arguments of ICFELL are as follows:
.sp
.IP "MESSG" 12
(an input variable or constant of type CHARACTER) - A character string saying
what is to be done to the current error message (when an error has occurred).
If MESSG is blank, no change is to be made in the current error message. If
the length of MESSG is six characters or fewer, a slash is prepended to the
current error message and then MESSG is prepended to that. If the length of
MESSG is seven characters or more, MESSG is a complete new error message to
be substituted for the current one; it should be of the form
"XXXXXX - TEXT DESCRIBING THE ERROR", where "XXXXXX" is the name of the
routine in which the error occurred, and it should not be more than 113
characters long.
.IP "NERRF" 12
(an input expression of type INTEGER) - A value in the range from -999 to
+9999. If this value is zero, no change is made in the internal error flag;
otherwise, it is the desired new value of that flag (when an error has
occurred).
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tseter,
arex02.
.SH ACCESS
To use ICFELL or c_icfell, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
entsr, eprin, errof, error_handling, fdum, icloem, nerro, retsr,
semess, seter, ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
