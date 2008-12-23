.TH PCPNWI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PCPNWI - A function of type CHARACTER*16.
.SH SYNOPSIS
.nf
CHARACTER*16 PCPNWI,ENAM
ENAM = PCPNWI (PNAM,IPAI)
.fi
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pcpnwi (char *pnam, int *ipai)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) is a character string,
the first two characters of
which are the name of an internal parameter array of
Plotchar (currently, \'BC\' or \'CC\').
.IP IPAI 12
(an input expression of type INTEGER)
is an integer expression specifying the index of that
element of the internal parameter array of which one wants
the name.
.PP
Given the above two arguments, the value of PCPNWI(PNAM,IPAI) is that
character string which serves as the name of element IPAI of the internal
parameter array identified by PNAM.
.sp
For example, if IPAI has the value 3, then the value of
"PCPNWI('CC',IPAI)" is the character string 'CC(3)' (padded with blanks
to the right) and the call "CALL PCSETI (PCPNWI('CC',IPAI),62)" would
give the value 62 to the third element of the parameter array 'CC'.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
This routine generates parameter names for use in calls to
other routines. 
For a complete list of parameters available
in this utility, see the plotchar_params man page.
.SH ACCESS
To use PCPNWI or c_pcpnwi, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
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
pcrset,
pcsetc,
pcseti,
pcsetr,
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
