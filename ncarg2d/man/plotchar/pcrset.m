.TH PCRSET 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PCRSET - Resets all parameters to their initial default values.
.SH SYNOPSIS
CALL PCRSET
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pcrset()
.SH DESCRIPTION 
PCRSET has no arguments.
.SH USAGE
Calling this routine restores all
Plotchar parameters to their initial default values.
For a complete list of parameters available
in this utility, see the plotchar_params man page.
.SH EXAMPLES
Currently, there are no examples for this routine.
.SH ACCESS
To use PCRSET or c_pcrset, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the plotchar man page for a description of all Plotchar error
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
pcpnwi,
pcsetc,
pcseti,
pcsetr,
ncarg_cbind.
.sp
Hardcopy:
None.
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
