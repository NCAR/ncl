.TH PCSETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PCSETI - Sets the value of an internal parameter of type INTEGER or REAL.
.SH SYNOPSIS
CALL PCSETI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pcseti (char *pnam, int ival)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the name of the
parameter to be set. The name must appear as the first two
characters of the string. If the internal parameter is one
of the two (\'BC\' and \'CC\') that are arrays, the index of
the desired array element may appear, enclosed in
parentheses, in columns 3 and following. Other characters
may be used to document the use of the parameter being
retrieved; for example, instead of just \'MA\', one can use
\'MA - MAPPING FLAG\' and, instead of \'CC(10)\', one can use
\'CC(10) - SPECIAL COLOR 10\'.
.IP IVAL 12
(an input expression of type INTEGER)
is the value to be assigned to the
internal parameter specified by PNAM.
If the internal parameter is of type INTEGER, the value given to it is IVAL.
If the internal parameter is of type REAL, the value given to it is
REAL(IVAL).
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Plotchar parameters.  For a complete list of parameters available
in this utility, see the plotchar_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cpex01,
cpex09,
cpexcc,
epltch,
srex01,
vvexcc.
fcell0,
fcoord1,
fcoord2,
fgkgpm,
fgpm01,
fngwsym,
fpcfonts,
fpchiqu.
.SH ACCESS
To use PCSETI, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_pcseti, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
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
pcsetr,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
