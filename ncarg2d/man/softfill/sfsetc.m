'\" t
.TH SFSETC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SFSETC - Used to set the character value of a specified internal parameter.
.SH SYNOPSIS
CALL SFSETC (CNP, CVP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_sfsetc (char *cnp, char *cvp)
.SH DESCRIPTION 
.IP CNP 12
(an input expression of type CHARACTER) is the name of an internal parameter
of SOFTFILL.  Currently, the only legal value is 'CH'
(or any longer string beginning with 'CH'), which stands for "CHaracter".
.IP CVP 12
(an input expression of type CHARACTER) is the desired new
value of the parameter specified by CNP. This value will be 
used until the next call resetting it.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the 
FORTRAN argument descriptions.
.SH USAGE
This routine allows you to set the current value of Softfill
parameters. For a complete list of parameters available in
this utility, see the softfill_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples:
sfex01,
tsoftf.
.SH ACCESS
To use SFSETC, load the NCAR Graphics libraries ncarg, 
ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.  To use
c_sfsetc, load the NCAR Graphics libraries ncargC, ncarg_gksC,
ncarg, ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the softfill man page for a description of all Softfill
error messages and/or informational messages.
.SH SEE ALSO
Online: 
softfill, softfill_params, sfgetc, sfgeti, sfgetp, sfgetr,
sfseti, sfsetp, sfsetr, sfsgfa, sfwrld, ncarg_cbind
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

