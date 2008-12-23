.TH IDSETR 3NCARG "November 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
IDSETR - Provides a new real value for a Bivar parameter.
.SH SYNOPSIS
CALL IDSETR (PNAM,RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_idsetr (char *pnam, float rval)
.SH DESCRIPTION 
.IP "PNAM" 12
(an input constant or variable of type CHARACTER) -
The name of the parameter that you want to set. The character string
can be of any length, but only the first two characters
of it will be examined.
.IP "RVAL" 12
(an input expression of type REAL) -
The real value you select for the parameter.  If the parameter is of type
REAL, it will be given the value RVAL; if is is of type INTEGER, it will be
given the value INT(RVAL).
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of Bivar
parameters. For a complete list of parameters available in this 
utility, see the bivar_params man page.
.SH ACCESS
To use IDSETR or c_idsetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH MESSAGES
See the bivar man page for a description of all Bivar error
messages and/or informational messages.
.SH SEE ALSO
Online:
bivar, bivar_params, idbvip, idsfft, idpltr, idgeti, idgetr, idseti,
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
