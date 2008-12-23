.TH CPCICA 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPCICA - 
Incorporates into a user's cell array color
indices determined by examining where the user's contours
lie relative to the cell array.
.SH SYNOPSIS
 CALL CPCICA (ZDAT, RWRK, IWRK, ICRA, ICA1, ICAM, ICAN, 
.br
+ XCPF, YCPF, XCQF, YCQF)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpcica (float *zdat, float *rwrk, int *iwrk, 
.br
int *icra, int ica1, int icam, int ican, float xcpf, 
.br
float ycpf, float xcqf, float ycqf)
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
.IP ICRA 12
(INTEGER array, dimensioned ICA1 by "n", where "n" is 
greater than or equal to the value of the argument ICAN, 
input/output) is the user's cell array.
.IP ICA1 12
(INTEGER, input) is the first dimension of the FORTRAN 
array ICRA, which contains the user's cell array.
.IP ICAM 12
(INTEGER, input) is the first dimension of the user's 
cell array.
.IP ICAN 12
(INTEGER, input) is the second dimension of the user's 
cell array.
.IP "XCPF and YCPF" 12 
(REAL, input) are the coordinates, in the 
fractional coordinate system, of a point P. P is the point 
at that corner of the rectangular area into which the cell 
array maps that corresponds to the cell (1,1).
.IP "XCQF and YCQF" 12
(REAL, input) are the coordinates, in the 
fractional coordinate system, of a point Q. Q is the point 
at that corner of the rectangular area into which the cell 
array maps that corresponds to the cell (ICAM,ICAN).
.RE
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions, with the following exceptions:
.IP icra 12
icra is dimensioned n by ica1 where n is greater than or equal to the 
value of the argument ican
.IP ica1 12
The second dimension of the array icra.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples:
ccpcica,
ccpmovi.
.SH ACCESS
To use CPCICA or c_cpcica, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online: 
conpack, 
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
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
