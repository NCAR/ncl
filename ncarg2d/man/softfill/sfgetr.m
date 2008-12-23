'\" t
.TH SFGETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SFGETR - Used to retrieve the current real value of a
specified parameter.
.SH SYNOPSIS
CALL SFGETR (CNP, RVP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_sfgetr (char *cnp, float *rvp)
.SH DESCRIPTION 
.IP CNP 12
(an input expression of type CHARACTER) is one of the 
character strings 'AN', 'CH', 'DO', 'SP', or 'TY' (or a 
longer string beginning with one of these strings), 
meaning "ANgle", "CHaracter", "DOts", "SPacing", and "TYpe", 
respectively.
.IP RVP 12
(an output variable of type REAL) is the name of a variable 
into which the value of the parameter specified by CNP is to 
be retrieved.
If the internal parameter is inherently an integer
value "i", "REAL(i)" will be returned as the value of RVP.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of 
Softfill parameters. For a complete list of parameters
available in this utility, see the softfill_params man page.
.SH ACCESS
To use SFGETR or c_sfgetr, load the NCAR Graphics libraries ncarg, 
ncarg_gks, and ncarg_c, preferably in that order.  
.SH MESSAGES
See the softfill man page for a description of all Softfill 
error messages and/or informational messages.
.SH SEE ALSO
Online: 
softfill, softfill_params, sfgetc, sfgeti, sfgetp, sfsetc, sfseti,
sfsetp, sfsetr, sfsgfa, sfwrld, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
