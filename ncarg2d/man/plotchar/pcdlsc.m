.TH PCDLSC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PCDLSC - 
Defines the default list of "special colors" used by
PCHIQU in drawing certain characters from the filled fonts.
Calling PCDLSC with the argument IFCI will define color
indices IFCI, IFCI+1, IFCI+2,
\&..., IFCI+15 and it will set all elements of the
internal parameter array \'CC\' corresponding to indices 1
through 16. At the moment, although such a call does define a
set of sixteen colors (ranging from blue to red) and set
the elements of \'CC\', it\'s a bit pointless, since there are
no characters for which the special colors are used. In the
future, there will be a few such (like the state highway
symbol, which is normally drawn in a particular pair of
colors); at that time, the routine will be of more use.
.SH SYNOPSIS
CALL PCDLSC (IFCI)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pcdlsc (int ifci)
.SH DESCRIPTION 
.IP IFCI 12
(an input expression of type INTEGER) specifies the first of a set
of sixteen color indices that may be defined by PCDLSC.
Make sure that these color indices are not already used.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
This routine allows you to set the current value of Plotchar
parameters. For a complete list of parameters available in this utility,
see the plotchar_params man page.
.SH ACCESS
To use PCDLSC or c_pcdlsc, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the plotchar man page for a description of all Plotchar error
messages and/or informational messages.
.SH SEE ALSO
Online:
plotchar,
plotchar_params,
pcgetc,
pcgeti,
pcgetr,
pchiqu,
pcloqu,
pcmequ,
pcmpxy,
pcpnwi,
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
