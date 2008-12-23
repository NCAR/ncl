.TH NERRO 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NERRO - Referenced by a user to obtain the current value of the internal error
flag of SETER.
.SH SYNOPSIS
NERR=NERRO(NERRF)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_nerro(int *nerrf)
.SH DESCRIPTION 
The FORTRAN expression "NERRO(NERRF)" has the value of the internal error
flag of SETER.  If its value is non-zero, this indicates that a prior
recoverable error occurred and has not yet been cleared.  The argument
NERRF is given the same value as the function reference; this is useful
in some situations.
.sp
The argument of NERRO is as follows:
.sp
.IP "NERRF" 12
(an output variable of type INTEGER) - Receives the same value that is
returned as the value of the function itself.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tseter,
arex02.
.SH ACCESS
To use nerro or c_nerro, load the NCAR Graphics libraries  ncarg, 
ncarg_gks, and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
entsr, eprin, errof, error_handling, fdum, icfell, icloem, retsr, semess,
seter, ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
