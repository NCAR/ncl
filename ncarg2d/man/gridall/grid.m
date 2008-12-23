.TH GRID 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GRID - Draws an unlabeled grid.
.SH SYNOPSIS
CALL GRID (MJRX, MNRX, MJRY, MNRY)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_grid (int mjrx, int mnrx, int mjry, int mnry)
.SH DESCRIPTION 
.IP "MJRX, MNRX, MJRY, and MNRY" 12 
(input expressions of type
INTEGER) specify the major and minor divisions of 
the horizontal (X) and vertical (Y) axes
of the current viewport. The meanings of these parameters
depend on the current setting of the internal parameter
\'LS\' of SPPS:
.RS
.IP \(bu
If the value of \'LS\' implies that the axis is linear:
MJRX(Y) specifies the number of major divisions of the X(Y)
axis and MNRX(Y) specifies the number of minor divisions
within each major division. In each case, the value
specifies the number of spaces between grid lines or ticks
rather than the number of lines or ticks. Including the
ones at the ends of the axes, there is always one more
major division line or mark than the number of major
divisions specified by MJRX(Y). Similarly, there is always
one less minor division line or tick per major division
than the number of minor divisions per major division
specified by MNRX(Y).
.IP \(bu
If the value of \'LS\' implies that the axis is logarithmic:
Each major division point occurs at a value 10**MJRX(Y)
times the previous point. For example, if the minimum 
X-axis value were 3., the maximum X-axis value 3000. and MJRX
1, then the major division points would be 3., 30., 300.,
and 3000. If MNRX(Y).LE.10, there are nine minor divisions
within each major division. For example, between 3. and
30., there would be minor division points at 6., 9., 12., .
\&. . 27. If MNRX(Y).GT.10., minor divisions are omitted.
.RE
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The statement
.RS 3 
.sp
CALL GRID (MJRX,MNRX,MJRY,MNRY)
.sp
.RE
is equivalent to 
.RS 3
.sp
CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,0,0,0,0.,0.)
.RE
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:
tgrida.
.SH ACCESS
To use GRID or c_grid, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
gridall,
gridall_params,
gacolr,
gagetc,
gageti,
gagetr,
gasetc,
gaseti,
gasetr,
gridal,
gridl,
halfax,
labmod,
perim,
periml,
tick4,
ticks,
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
