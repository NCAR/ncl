.TH CPLBAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPLBAM - 
Adds label boxes (for the
informational label, high and low labels, and contour-line
labels) to an area map. The ultimate object of this will
usually be to prevent contour lines drawn by CPCLDM from
passing through labels or to prevent fill of the label
boxes as contour bands are filled.
.SH SYNOPSIS
CALL CPLBAM (ZDAT, RWRK, IWRK, IAMA)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cplbam (float *zdat, float *rwrk, int *iwrk, 
.br
int *iama)
.SH DESCRIPTION 
The first three arguments are arrays used in the last call 
to CPRECT, CPSPS1, or CPSPS2, the contents of which must 
not have been changed since that call.
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
.IP IAMA 12
(INTEGER array, dimensioned as specified in a call to 
ARINAM) is the array holding the area map to which the 
label boxes are to be added.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The routine CPLBAM, which adds label boxes to an area map, may
be called at any time after the initialization call to CPRECT,
CPSPS1, or CPSPS2.  If, during the last call to CPRECT, CPSPS1,
or CPSPS2, the data being contoured were found to be
essentially constant, then the box for the constant field
label is added to the area map.  Otherwise, boxes for the
informational label, the high and low labels, and/or the
contour line labels are added, as follows:
.IP \(bu 3
A box for the informational label is added only if the
parameter 'ILT', which specifies the text of that label,
is non-blank.
.IP \(bu 3
Boxes for the high labels are added only if the parameter
\&'HIT', which specifies the text of those labels, is
non-blank.
.IP \(bu 3
Boxes for the low labels are added only if the parameter
\&'LOT', which specifies the text of those labels, is
non-blank.
.IP \(bu 3
Boxes for the contour line labels are added only if
the parameter 'LLP', which specifies how those labels are to
be positioned, has an absolute value of 2 or 3, and if, for
some I between 1 and 'NCL', inclusive, the Ith element of the
parameter array 'CLU' has a value implying that contour lines
at the contour level specified by the Ith element of 'CLV'
are to be labeled.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpcldm,
ccplbam,
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
ccprc,
ccpscam,
colcon,
cpex01,
cpex02,
cpex03,
cpex04,
cpex06,
cbex01,
vvex01,
fcover,
ffex03,
ffex05.
.SH ACCESS
To use CPLBAM or c_cplbam, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, 
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
