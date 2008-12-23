'\" t
.TH SFGETC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SFGETC - Used to retrieve the current value of a specified internal parameter.
.SH SYNOPSIS
CALL SFGETC (CNP,CVP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_sfgetc (char *cnp, char *cvp, int len)
.SH DESCRIPTION 
.IP CNP 12
(an input expression of type CHARACTER) is the name of an internal parameter
of SOFTFILL.  Currently, the only legal value is 'CH'
(or any longer string beginning with 'CH'), which stands for "CHaracter".
.IP CVP 12
(an output variable of type CHARACTER) is the name of a variable into which
the value of the parameter specified by CNP is to be retrieved.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exception:
.IP len 12
Size of cvp as dimensioned in the calling program.
.SH USAGE
This routine allows you to retrieve the current value of Softfill
parameters. For a complete list of parameters available in this
utility, see the softfill_params man page.
.SH ACCESS
To use SFGETC or c_sfgetc, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the softfill man page for a description of all Softfill error
messages and/or informational messages.
.SH SEE ALSO
Online: 
softfill, softfill_params, sfgeti, sfgetp, sfgetr, sfseti, sfsetp, sfsetr,
sfsgfa, sfwrld, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
