.TH CPGETC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPGETC - Retrieves the current value of an internal
parameter of type CHARACTER.
.SH SYNOPSIS
CALL CPGETC (PNAM, CVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpgetc (char *pnam, char *cval, int len)
.SH DESCRIPTION 
.IP PNAM 12
(CHARACTER, input) is the name of a parameter whose 
character value is to be retrieved. Only the first three 
characters of PNAM are examined. It is recommended that the 
rest of the character string be used to improve the 
readability of the code. For example, instead of just 
\&'LLT', use 'LLT - LINE LABEL TEXT'.
.IP CVAL 12
(CHARACTER, output) is a variable in which the value 
of the parameter specified by PNAM is to be returned.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exception:
.IP len 12
The size of cval as dimensioned in the calling program.
.SH USAGE
This routine allows you to retrieve the current value of
Conpack parameters.  For a complete list of parameters available
in this utility, see the conpack_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
cpex07.
.SH ACCESS
To use CPGETC or c_cpgetc, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
conpack_params,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgeti, cpgetr, cplbam,
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
