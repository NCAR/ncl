.TH HALFAX 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
HALFAX - Draws orthogonal axes intersecting at a specified point
and with a specified set of labels.
.SH SYNOPSIS
CALL HALFAX (MJRX,MNRX,MJRY,MNRY,XINT,YINT,IXLB,IYLB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_halfax (int mjrx, int mnrx, int mjry, int mnry, 
.br
float xint, float yint, int ixlb, int iylb)
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
times the previous point. For example, if the minimum X-axis
value were 3., the maximum X-axis value 3000. and MJRX
1, then the major division points would be 3., 30., 300.,
and 3000. If MNRX(Y).LE.10, there are nine minor divisions
within each major division. For example, between 3. and
30., there would be minor division points at 6., 9., 12., .
\&. . 27. If MNRX(Y).GT.10., minor divisions are omitted.
.RE
.IP "XINT and YINT" 12
(input expressions of type REAL)
are the user coordinates of the point of
intersection of the two axes.
.IP IXLB 12
(an input expression of type INTEGER) is defined as
follows:
.RS
.IP \(bu
IXLB = -1 implies that no X axis is to be drawn.
.IP \(bu
IXLB = 0 implies that the X axis is to be drawn unlabeled.
.IP \(bu
IXLB = 1 implies that the X axis is to be drawn and
labeled.
.RE
.IP IYLB 12
(an input expression of type INTEGER) is defined as
follows:
.RS
.IP \(bu
IYLB = -1 implies that no Y axis is to be drawn.
.IP \(bu
IYLB = 0 implies that the Y axis is to be drawn unlabeled.
.IP \(bu
IYLB = 1 implies that the Y axis is to be drawn and
labeled.
.RE
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The statement
.RS 3 
.sp
CALL HALFAX (MJRX,MNRX,MJRY,MNRY,XINT,YINT,IXLB,IYLB)
.sp
.RE
is equivalent to 
.RS 3
.sp
CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,IXLB,IYLB,10,XINT,YINT)
.RE
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
tgrida.
.SH ACCESS
To use HALFAX or c_halfax, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
gridall,
gacolr,
gagetc,
gageti,
gagetr,
gasetc,
gaseti,
gasetr,
grid,
gridal,
gridl,
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
