.TH PCGETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PCGETI - Retrieves the integral value of an internal parameter of type
INTEGER or REAL.
.SH SYNOPSIS
CALL PCGETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pcgeti (char *pnam, int *ival)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the name of the
parameter to get. The name must appear as the first two
characters of the string. If the internal parameter is one
of the two (\'BC\' and \'CC\') that are arrays, the index of
the desired array element may appear, enclosed in
parentheses, in columns 3 and following. Other characters
may be used to document the use of the parameter being
retrieved; for example, instead of just \'MA\', one can use
\'MA - MAPPING FLAG\' and, instead of \'CC(10)\', one can use
\'CC(10) - SPECIAL COLOR 10\'.
.IP IVAL 12
(an output variable of type INTEGER) is the name of the variable
into which the value of the internal parameter specified by PNAM
is to be retrieved.
If the internal parameter is a value "i" of type INTEGER, the value returned
is "i".
If the internal parameter is a value "r" of type REAL, the value returned is
"INT(r)".
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
vvexcc.
.SH ACCESS
To use PCGETI or c_pcgeti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
plotchar,
plotchar_params,
pcdlsc,
pcgetc,
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
