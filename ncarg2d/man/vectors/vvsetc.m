.TH VVSETC 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VVSETC - 
Sets the value of a Vectors parameter of
type CHARACTER.
.SH SYNOPSIS
CALL VVSETC (CNM,CVL) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vvsetc (char *cnm, char *cvl)
.SH DESCRIPTION 
.IP CNM 12
(CHARACTER, input) is the name of a parameter to be
given a character value. Only the first three characters of
CNM are examined. The three characters may either be
entirely upper or entirely lower case; mixed case is not
recognized. It is recommended that the rest of the
character string be used to improve the readability of the
code. For example, instead of \'ZFT\', use \'ZFT - Zero Field
Text String\'.
.IP CVL 12
(CHARACTER, input) is an expression, the value of which
is to be given to the parameter specified by CNM.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
This routine allows you to set the current value of Vectors parameters
of type CHARACTER.  For a complete list of parameters available in
this utility, see the vectors_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples:
fcover,
ffex00.
.SH ACCESS
To use VVSETC or c_vvsetc, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
vvseti,
vvsetr,
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
