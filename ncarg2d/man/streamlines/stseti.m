.TH STSETI 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STSETI - 
Sets the value of an internal parameter of
type INTEGER.
.SH SYNOPSIS
CALL STSETI (CNM,IVL) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_stseti(char *cnm, int ivl)
.SH DESCRIPTION 
.IP CNM 12
(CHARACTER, input) is the name of a parameter to be
given an integer value. Only the first three characters of
CNM are examined. The three characters must either be
entirely upper or entirely lower case; mixed case is not
recognized. It is recommended that the rest of the
character string be used to improve the readability of the
code. For example, instead of \'CKP\', use \'CKP - Check
Progress Iteration Count\'.
.IP IVL 12
(INTEGER, input) is an expression, the value of which
is to be given to the parameter specified by CNM.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
This routine allows you to set the current value of Streamlines
parameters of type INTEGER. For a complete list of parameters
available in this utility, see the streamlines_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples:
ffex00,
ffex03,
ffex04,
stex01,
stex02,
stex03.
.SH ACCESS
To use STSETI or c_stseti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the streamlines man page for a description of all Streamlines error
messages and/or informational messages.
.SH SEE ALSO
Online:
stgetc,
stgeti,
stgetr,
stinit,
stream,
streamlines,
streamlines_params,
strset,
stsetc,
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
