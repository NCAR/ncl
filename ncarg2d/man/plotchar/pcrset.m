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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
