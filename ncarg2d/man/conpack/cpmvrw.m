.TH CPMVRW 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPMVRW - Moves the contents of an old real workspace to a new one.
.SH SYNOPSIS
CALL CPMVRW (RWKO, RWKN, LWKN)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpback (float *iwko, float *iwkn, int *lwkn)
.SH DESCRIPTION 
The arguments define the old and new real workspaces.
.IP IWKO 12
(an input array of type REAL, dimensioned as specified in the last call
to CPRECT, CPSPS1, CPSPS2, or CPMVIW) is the old real workspace array.
.IP IWKN 12
(an output array of type REAL, dimensioned LWKN) is the new real
workspace array.
.IP LWKN 12
(an input expression of type INTEGER) specifies the length of RWKN.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
When, in the execution of a CONPACK routine, the amount of space left in
the real workspace array is found to be insufficient, if the internal
parameter 'WSO' has the value 3, the error-handling routine SETER is
called with an appropriate error message.  If, in addition, the user has
turned recovery mode on, execution continues and, eventually, control is
returned to the user.  At that point, the user should detect the fact
that an error has occurred.  If he/she chooses to try to recover from
the error, CPMVRW may be of use: it may be called to move everything
from the current real workspace to a new (and presumably bigger) one,
after which it may be possible to resume execution.
.SH EXAMPLES
None, at present.
.SH ACCESS
To use CPMVRW or c_cpmvrw, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online: 
conpack, 
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
