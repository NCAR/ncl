.TH CPSETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPSETR - 
Sets the value of an internal parameter of type REAL or INTEGER.
.SH SYNOPSIS
CALL CPSETR (PNAM, RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpsetr (char *pnam, float rval)
.SH DESCRIPTION 
.IP PNAM 12
(CHARACTER, input) is the name of a parameter to be 
given a real value. Only the first three characters of PNAM 
are examined. It is recommended that the rest of the 
character string be used to improve the readability of the 
code. For example, instead of 'CIS', use 'CIS - CONTOUR 
INTERVAL SPECIFIER'.
.IP RVAL 12
(REAL, input) is an expression, the value of which is 
to be given to the parameter specified by PNAM.
If the internal parameter is of type REAL, the value given to it is RVAL.
If the internal parameter is of type INTEGER, the value given to it is
INT(RVAL).
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Conpack parameters.  For a complete list of parameters available
in this utility, see the conpack_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
caredg,
ccpcfx,
ccpcica,
ccpcir,
ccpcis,
ccpcit,
ccpcll,
ccpclu,
ccpga,
ccphand,
ccphcf,
ccphl,
ccphlt,
ccpila,
ccpils,
ccpilt,
ccplbam,
ccpline,
ccpllc,
ccplll,
ccpllp,
ccpllt,
ccpllw,
ccpmap,
ccpmovi,
ccpmpxy,
ccpnsd,
ccppc1,
ccppc2,
ccppc3,
ccppc4,
ccppkcl,
ccppole,
ccprc,
ccpscam,
ccpset,
ccpsps2,
ccpspv,
ccpt2d,
ccpvp,
colcon,
cpex01,
cpex02,
cpex03,
cpex04,
cpex05,
cpex06,
cpex07,
cpex08,
cpex09,
cbex01,
vvex01,
tconpa,
fcover,
ffex03,
ffex05,
fsfsgfa.
.SH ACCESS
To use CPSETR or c_cpsetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
conpack_params,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsprs, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
