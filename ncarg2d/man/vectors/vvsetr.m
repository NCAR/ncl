.TH VVSETR 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VVSETR - 
Sets the value of a Vectors parameter of
type REAL.
.SH SYNOPSIS
CALL VVSETR (CNM,RVL) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vvsetr(char *cnm, float rvl)
.SH DESCRIPTION 
.IP CNM 12
(CHARACTER, input) is the name of a parameter to be
given a real value. Only the first three characters of CNM
are examined. The three characters may either be entirely
upper or entirely lower case; mixed case is not recognized.
It is recommended that the rest of the character string be
used to improve the readability of the code. For example,
instead of \'VFR\', use \'VFR - Minimum Vector Fractional
Length\'.
.IP RVL 12
(REAL, input) is an expression, the value of which is
to be given to the parameter specified by CNM.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
This routine allows you to set the current value of Vectors parameters
of type REAL. For a complete list of parameters available in this
utility, see the vectors_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples:
bnchmk,
fcover,
ffex00,
ffex02,
ffex05,
stex02,
stex03,
vvex01,
vvex02.
.SH ACCESS
To use VVSETR or c_vvsetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the vectors man page for a description of all Vectors error
messages and/or informational messages.
.SH SEE ALSO
Online:
vectors,
vectors_params,
vvectr,
vvgetc,
vvgeti,
vvgetr,
vvinit,
vvrset,
vvsetc,
vvseti,
vvudmv,
vvumxy,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
