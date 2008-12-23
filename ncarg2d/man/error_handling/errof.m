.TH ERROF 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ERROF - Turns off the internal error flag of SETER.
.SH SYNOPSIS
CALL ERROF
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_errof()
.SH DESCRIPTION 
The FORTRAN statement "CALL ERROF" turns off (zeroes) SETER's internal error
flag.
.sp
ERROF has no arguments.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tseter,
arex02.
.SH ACCESS
To use ERROF or c_errof, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
entsr, eprin, error_handling, fdum, icfell, icloem, nerro, retsr, semess, seter,
ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
