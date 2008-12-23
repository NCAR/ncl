.TH CPRSET 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPRSET - Resets all parameters to their initial default
values.
.SH SYNOPSIS
CALL CPRSET 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cprset()
.SH DESCRIPTION
CPRSET has no arguments.
.SH USAGE
Calling this routine restores all
Conpack parameters to their initial default values.  
For a complete list of parameters available
in this utility, see the conpack_params man page.
.SH ACCESS
To use CPRSET or c_cprset, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
conpack,
conpack_params,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cpscae, cpsetc,
cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
