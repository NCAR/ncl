.TH SEMESS 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SEMESS - Called by a user to get a specified portion of the current error
message.
.SH SYNOPSIS
MESSG = SEMESS(ITRIM)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
char *c_semess(int itrim)
.SH DESCRIPTION 
In any routine that references
SEMESS, it must be declared to be of type CHARACTER*113.
.sp
If the internal error flag of SETER is non-zero, the value of the character
expression "SEMESS(ITRIM)" is the remembered error message associated with
that error flag, trimmed as indicated by the value of the input quantity
ITRIM.  Otherwise, it is a string of blanks.
.sp
When ITRIM = 0 or less, no trimming is done; the whole error message is
returned.  When ITRIM = 1, prepended routine names and the associated slashes
are trimmed off.  When ITRIM = 2 or greater, all leading routine names are
trimmed off; just the error description is returned.
.sp
Example: Assume that the
current error message is the string "CPCLAM/AREDAM - AREA-MAP ARRAY OVERFLOW";
then, SEMESS(0) is that entire string, while SEMESS(1) = "AREDAM - AREA-MAP
ARRAY OVERFLOW", and SEMESS(2) = "AREA-MAP ARRAY OVERFLOW".
.sp
The argument of SEMESS is as follows:
.sp
.IP "ITRIM" 12
(an input expression of type INTEGER) - Says what part of the error message
is to be returned, as detailed above.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tseter,
arex02.
.SH ACCESS
To use SEMESS or c_semess, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
entsr, eprin, errof, error_handling, fdum, icfell, icloem, nerro, retsr,
seter, ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
