.TH ARGETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ARGETI - Retrieves the integer value of an Areas parameter.
.SH SYNOPSIS
CALL ARGETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_argeti (char *pnam, int *ival)
.SH DESCRIPTION 
.IP "PNAM" 12
(an input constant or variable of type CHARACTER) -
The name of the parameter that you want to retrieve.  The character string
can be of any length, but only the first two characters
of it will be examined.
.IP "IVAL" 12
(an output variable of type INTEGER) -
An integer variable to receive the desired parameter value.  If the internal
parameter is of type INTEGER and has the value "i", IVAL will be given the
value "i"; if the internal parameter is of type REAL and has the value
"r", IVAL will be given the value "INT(r)".
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of Areas 
parameters. For a complete list of parameters available in this 
utility, see the areas_params man page.
.SH ACCESS
To use ARGETI or c_argeti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH MESSAGES
See the areas man page for a description of all Areas error
messages and/or informational messages.
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, ardrln, aredam, argetr, argtai, arinam,
armvam, arpram, arscam, arseti, arsetr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
