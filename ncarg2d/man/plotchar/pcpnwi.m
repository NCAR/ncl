.TH PCPNWI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PCPNWI - 
Is a character function.
.SH SYNOPSIS
CALL PCPNWI (PNAM,IPAI)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_PCPNWI (char *pnam, int *ipai)
.SH DESCRIPTION 
.IP PNAM 12
(character string) 
specifies the first two characters of
which are the name of an internal parameter array of
Plotchar (currently, \'BC\' or \'CC\').
.IP IPAI 12
is an integer expression specifying the index of the
element of the internal parameter array of which one wants
the name.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
This routine generates parameter names for use in calls to
other routines. 
For a complete list of parameters available
in this utility, see the plotchar_params man page.
.SH ACCESS
To use PCPNWI, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_pcpnwi, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH MESSAGES
See the plotchar page for a description of all Plotchar error 
messages and/or informational messages.
.SH SEE ALSO
Online:
plotchar,
plotchar_params,
pcdlsc,
pcgetc,
pcgeti,
pcgetr,
pchiqu,
pcloqu,
pcmequ,
pcmpxy,
pcsetc,
pcseti,
pcsetr,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
