.TH STGETC 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STGETC - Gets the current value of a Streamlines parameter of type
CHARACTER.
.SH SYNOPSIS
CALL STGETC (CNM,CVL) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_stgetc(char *cnm, char *cvl, int len)
.SH DESCRIPTION 
.IP CNM 12
(CHARACTER, input) is the name of a parameter whose character value is
to be retrieved. Only the first three characters of CNM are examined.
The three characters may either be entirely upper or entirely lower
case; mixed case is not recognized. It is recommended that the rest of
the character string be used to improve the readability of the code.
For example, instead of just \'ZFT\', use \'ZFT - Zero Field Text
String\'.
.IP CVL 12
(CHARACTER, output) is a variable in which the value of the parameter
specified by CNM is to be returned.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions with the following exception:
.sp
.IP len 12
The size of cvl as dimensioned in the calling program.
.SH USAGE
This routine allows you to retrieve the current value of Streamlines
parameters of type CHARACTER. For a complete list of parameters
available in this utility, see the streamlines_params man page.
.SH ACCESS
To use STGETC or c_stgetc, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the streamlines man page for a description of all Streamlines error
messages and/or informational messages.
.SH SEE ALSO
Online:
stgeti,
stgetr,
stinit,
stream,
streamlines,
streamlines_params,
strset,
stsetc,
stseti,
stsetr,
stuixy,
stumsl,
stumta,
stumxy,
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
