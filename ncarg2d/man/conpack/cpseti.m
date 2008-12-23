.TH CPSETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPSETI - 
Sets the value of an internal parameter of type INTEGER or REAL.
.SH SYNOPSIS
CALL CPSETI (PNAM, IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpseti (char *pnam, int ival)
.SH DESCRIPTION 
.IP PNAM 12
(CHARACTER, input) is the name of a parameter to be 
given an integer value. Only the first three characters of 
PNAM are examined. It is recommended that the rest of the 
character string be used to improve the readability of the 
code. For example, instead of 'ILA', use 'ILA - 
INFORMATIONAL LABEL ANGLE'.
.IP IVAL 12
(INTEGER, input) is an expression, the value of which 
is to be given to the parameter specified by PNAM.
If the internal parameter is of type INTEGER, the value given to it is IVAL.
If the internal parameter is of type REAL, the value given to it is
REAL(IVAL).
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
ccpcfx,
ccpcica,
ccpcir,
ccpcis,
ccpcit,
ccpclc,
ccpcld,
ccpcldm,
ccpcll,
ccpclu,
ccpfil,
ccphand,
ccphcf,
ccphl,
ccphlt,
ccpila,
ccpklb,
ccplbdr,
ccpline,
ccpllb,
ccpllc,
ccplll,
ccpllo,
ccpllp,
ccpllt,
ccpllw,
ccpmap,
ccpmovi,
ccpmpxy,
ccpncls,
ccpnet,
ccpnof,
ccpnsd,
ccppc,
ccppc1,
ccppc2,
ccppc3,
ccppc4,
ccppkcl,
ccppole,
ccprc,
ccppole,
ccpset,
ccpspv,
ccpt2d,
ccpvp,
ccpvs,
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
To use CPSETI or c_cpseti, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
cpsetc, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
