.TH PCGETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PCGETR - Retrieves the real value of an internal parameter of type REAL or
INTEGER.
.SH SYNOPSIS
CALL PCGETR (PNAM,RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pcgetr (char *pnam, float *rval)
.SH DESCRIPTION 
.IP PNAM 12
(an input expression of type CHARACTER) specifies the name of the
parameter to get. The name must appear as the first two
characters of the string. If the internal parameter is one
of the two (\'BC\' and \'CC\') that are arrays, the index of
the desired array element may appear, enclosed in
parentheses, in columns 3 and following. Other characters
may be used to document the use of the parameter being
retrieved; for example, instead of just \'MA\', one can use
\'MA - MAPPING FLAG\' and, instead of \'CC(10)\', one can use
\'CC(10) - SPECIAL COLOR 10\'.
.IP RVAL 12
(an output variable of type REAL) is the name of the variable
into which the value of the internal parameter specified by PNAM
is to be retrieved.
If the internal parameter is a value "r" of type REAL, the value returned
is "r".
If the internal parameter is a value "i" of type INTEGER, the value returned
is "REAL(i)".
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
Plotchar parameters.  For a complete list of parameters available
in this utility, see the plotchar_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cpexcc,
epltch,
vvexcc,
fpchiqu.
.SH ACCESS
To use PCGETR or c_pcgetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
