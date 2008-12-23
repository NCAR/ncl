.TH ARSETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ARSETI - Provides a new integer value for an Areas parameter.
.SH SYNOPSIS
CALL ARSETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_arseti (char *pnam, int ival)
.SH DESCRIPTION 
.IP "PNAM" 12
(an input constant or variable of type CHARACTER) -
The name of the parameter that you want to set. The character string
can be of any length, but only the first two characters
of it will be examined.
.IP "IVAL" 12
(an input expression of type INTEGER) - 
The integer value you select for the parameter.  If the parameter is of type
INTEGER, it will be given the value IVAL; if is is of type REAL, it will be
given the value REAL(IVAL).
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of Areas 
parameters. For a complete list of parameters available in this 
utility, see the areas_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cardb1,
arex01.
.SH ACCESS
To use ARSETI or c_arseti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH MESSAGES
See the areas man page for a description of all Areas error
messages and/or informational messages.
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, ardrln, aredam, argeti, argetr, argtai,
arinam, armvam, arpram, arscam, arsetr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
