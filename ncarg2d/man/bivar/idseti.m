.TH IDSETI 3NCARG "November 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
IDSETI - Provides a new integer value for a Bivar parameter.
.SH SYNOPSIS
CALL IDSETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_idseti (char *pnam, int ival)
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
This routine allows you to set the current value of Bivar
parameters. For a complete list of parameters available in this 
utility, see the bivar_params man page.
.SH ACCESS
To use IDSETI or c_idseti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH MESSAGES
See the bivar man page for a description of all Bivar error
messages and/or informational messages.
.SH SEE ALSO
Online:
bivar, bivar_params, idbvip, idsfft, idpltr, idgeti, idgetr, idsetr,
ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
