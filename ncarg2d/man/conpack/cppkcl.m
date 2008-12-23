'\" t
.TH CPPKCL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPPKCL - Picks a set of contour levels.
.SH SYNOPSIS
CALL CPPKCL (ZDAT, RWRK, IWRK) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cppkcl (float *zdat, float *rwrk, int *iwrk) 
.SH DESCRIPTION 
All three arguments are arrays used in the last call to 
CPRECT, CPSPS1, or CPSPS2, the contents of which must not 
have been changed since that call.
.IP ZDAT 12
(REAL array, dimensioned as specified in the last call 
to CPRECT, CPSPS1, or CPSPS2, input) is the data array.
.IP RWRK 12
(REAL array, dimensioned as specified in the last call 
to CPRECT, CPSPS1, or CPSPS2, input/output) is the real 
workspace array.
.IP IWRK 12
(INTEGER array, dimensioned as specified in the last 
call to CPRECT, CPSPS1, or CPSPS2, input/output) is the 
integer workspace array.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
Normally, CPPKCL is called by CONPACK when the contour levels are
needed; it may be called directly by the user when it is desired
to modify the resulting parameter arrays specifying contour levels
and associated quantities in some way.  CPPKCL must be called after
initialization has been done (by a call to one of the initialization
routines CPRECT, CPSPS1, or CPSPS2); this is because CPPKCL depends
on values set during initialization.
.sp
If the contour-level-selection parameter 'CLS' is zero (indicating
that the user intends to pick contour levels), or if the
constant-field-flag 'CFF' is non-zero (indicating that, during
initialization, the data were found to be essentially constant),
CPPKCL does nothing.  Otherwise, CPPKCL picks a set of contour
levels.  The way in which it picks these levels is determined by
the value of the internal parameter 'CLS'; see the description of
this parameter in the conpack_params man page.
.sp
When CPPKCL picks contour levels, it sets the value of the internal
parameter 'NCL' equal to the number of contour levels picked.  It
then sets the values of elements 1 through 'NCL' of the parameter
array 'CLV' (contour level values) to the levels chosen.  The Ith
elements of the associated parameter arrays are set as follows:
.TS
tab (/);
l l.
.sp
CLU = 1 or 3/Contour level use flag 
AIA = I+1/Area identifier above the level
AIB = I/Area identifier below the level
LLT = ' ' (single blank)/Contour line label text
CLD = '$$$$$$$$$$$$$'/Contour line dash pattern
CLC = -1/Contour line color
LLC = -1/Contour line label color
CLL = 0./Contour line line width
.TE
.sp
Thus, after CPPKCL is called, the situation for each 
contour level is as follows:
.IP \(bu
Contour lines at the level are to be drawn by calls to CPCLDM or CPCLDR.
.IP \(bu
The lines will be labeled if 'CLU'=3, but not if 'CLU'=1.
.IP \(bu
If CPCLAM is called, lines at the level will be added to the area map;
the area identifiers for areas "above" the level and for areas "below"
the level are as specified by 'AIA' and 'AIB'.
.IP \(bu
The dash pattern for the level is solid, and neither 
the color of the line, the color of the labels on it, nor its 
width are to be set. 
.IP \(bu
The text of the label associated with the level is, 
as yet, unspecified.
.PP
By resetting elements in these parameter arrays, the user can change the
situation in various ways.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
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
ccpscam,
cpex01,
cpex02,
cpex03,
cpex06,
vvex01,
fcover,
ffex03,
ffex05.
.SH ACCESS
To use CPPKCL or c_cppkcl, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppklb, cprect, cprset, cpscae, cpsetc,
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
