'\" t
.TH GRIDAL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GRIDAL - 
Draws any of the supported types of backgrounds. Each of
the other background-drawing routines is implemented by a
call to GRIDAL.
.SH UTILITY
This routine is part of the Gridall utility in NCAR Graphics.  To
see the overview man page for this utility, type "man gridall".
.SH SYNOPSIS
 CALL GRIDAL (MJRX, MNRX, MJRY, MNRY, IXLB, IYLB, IGPH, 
.br
+ XINT, YINT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gridal (int mjrx, int mnrx, int mjry, int mnry, 
.br
int ixlb, int iylb, int igph, float xint, float yint)
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
.IP IGPH 12
(an input expression of type INTEGER) specifies the
background type, as follows:
.sp
.in +10
.TS
tab (%);
l l l.
IGPH%X axis%Y axis
----%------%------
0%grid%grid
1%grid%perim
2%grid%halfax
4%perim%grid
5%perim%perim
6%perim%halfax
8%halfax%grid
9%halfax%perim
10%halfax%halfax
.TE
.in -10
.IP "XINT and YINT" 12
(input expressions of type REAL), if IGPH has
the value 10, are the user coordinates of the point of
intersection of the two axes. For other values of IGPH for
which one of the axes is of type HALFAX, XINT and/or YINT
specify the position of that axis.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tgrida,
ccpga,
ccpmpxy.
.SH ACCESS
To use GRIDAL or c_gridal, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the gridall man page for a description of all Gridall error
messages and/or informational messages.
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
grid,
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
NCAR Graphics Contouring and Mapping Tutorial
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
