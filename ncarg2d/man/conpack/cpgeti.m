.TH CPGETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPGETI - Retrieves the integral value of an internal parameter of type
INTEGER or REAL.
.SH SYNOPSIS
CALL CPGETI (PNAM, IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpgeti (char *pnam, int *ival)
.SH DESCRIPTION 
.IP PNAM 12
(CHARACTER, input) is the name of a parameter whose 
integer value is to be retrieved. Only the first three 
characters of PNAM are examined. It is recommended that the 
rest of the character string be used to improve the 
readability of the code. For example, instead of just 
\&'LLP', use 'LLP - LINE LABEL POSITIONING'.
.IP IVAL 12
(INTEGER, output) is a variable in which the value of 
the parameter specified by PNAM is to be returned.
If the internal parameter is a value "i" of type INTEGER, the value returned
is "i".
If the internal parameter is a value "r" of type REAL, the value returned is
"INT(r)".
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
Conpack parameters.  For a complete list of parameters available
in this utility, see the conpack_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpcff,
ccpclc,
ccpcld,
ccpcldm,
ccpcll,
ccpklb,
ccplbam,
ccplbdr,
ccpllb,
ccpllc,
ccplll,
ccpllo,
ccpllp,
ccpllt,
ccpllw,
ccppc,
ccppc1,
ccppc2,
ccppc3,
ccppc4,
ccppkcl,
ccprc,
ccprwc,
ccprwu,
ccpscam,
colcon,
cpex01,
cpex02,
cpex03,
cpex06,
cpexcc,
vvex01,
fcover,
ffex03,
ffex05.
.SH ACCESS
To use CPGETI or c_cpgeti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
conpack_params,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
