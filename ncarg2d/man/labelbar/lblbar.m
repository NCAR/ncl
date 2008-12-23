.TH LBLBAR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
LBLBAR - Draws a complete label bar.
.SH SYNOPSIS
 CALL LBLBAR (IHOV, XLEB, XREB, YBEB, YTEB, NBOX, WSFB, 
.br
+ HSFB, LFIN, IFTP, LLBS, NLBS, LBAB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_lblbar (int ihov, float xleb, float xreb, 
.br
float ybeb, float
yteb, int nbox, float wsfb, 
.br
float hsfb, int *lfin, 
int iftp, 
char *llbs[], 
.br
int nlbs, int lbab)
.SH DESCRIPTION
.IP IHOV 12
(an input expression of type INTEGER) is zero if the bar is to be 
oriented horizontally and non-zero if the bar is to be oriented 
vertically.
.IP "XLEB, XREB, YBEB, YTEB" 12
(input expressions of type REAL) specify a rectangular area 
in the plotter frame in which the entire bar, including 
labels, is to fit. Each is a real number between 0 and 1, 
inclusive. XLEB specifies the position of the left edge of the
area; XREB, the position of the right edge of the area; YBEB, 
the position of the bottom edge of the area; and YTEB, the 
position of the top edge of the area. A horizontal bar should
probably be made wider than it is high; similarly, a vertical 
bar should probably be made higher than it is wide.
.IP NBOX 12
(an input expression of type INTEGER) may be positive or negative;
its absolute value specifies the number of rectangular pieces
(let's call them boxes) into which the bar should be divided. 
All the boxes will be the same size. A horizontal bar will be
divided into boxes using vertical lines and a vertical bar will
be divided into boxes using horizontal lines. The boxes in a
horizontal bar are considered to be ordered from
left to right and the boxes in a vertical bar from bottom to top.
This ordering determines the order in which fill indices and 
labels are given. If NBOX is positive, the boxes will be outlined
after being drawn; if NBOX is negative, the boxes will not be
outlined.
.IP "WSFB, HSFB" 12
(input expressions of type REAL) determine what part of 
each box is to be color-filled or pattern-filled. Each is a number
between 0 and 1; they specify the horizontal and vertical
dimensions, respectively, of the filled sub-box, as fractions 
of the horizontal and vertical dimensions of the whole box. 
The exact position of each sub-box within its box will be
determined for you, depending on how you choose to 
have the bar labeled.
.IP LFIN 12
(an input array of type INTEGER) contains a list of indices, one 
for each box in the bar; each index determines how the filling 
of the associated box is to be done. These may be color indices 
or some other kind of indices, depending on the value of the 
next argument, IFTP.
.IP IFTP 12
(an input expression of type INTEGER) specifies how the sub-boxes 
should be filled, as follows:
.RS
.IP \(bu 4
If the value of IFTP is zero, LBLBAR fills each sub-box by 
calling the routine SFSGFA, in the package Softfill. In this 
case, the value of the Softfill internal parameter `TY' 
determines whether filling is done by calls to the GKS 
routine GFA or by drawing closely-spaced colored lines, 
which simulates solid fill, or by drawing lines which form 
patterns of different apparent densities.
.IP \(bu 4
If the value of IFTP is non-zero, the routine LBFILL will be 
called to do the filling. You may supply your own version of
the routine LBFILL.
.RE
.IP ""
Prior to the filling of the boxes, the current polyline color 
and width will be set as specified by the values of the internal
parameters 'CFL' and 'WFL', respectively. Therefore, if lines 
are used to do the fill and the method chosen does not 
otherwise set polyline color and width, the values of 'CFL' and
\&'WFL' will take effect.
.sp
After filling is complete, the filled sub-boxes may be outlined
using the color and line width specified by the Labelbar internal
parameters `CBL' and `WBL'. (As mentioned above, this is done 
only if the argument NBOX is positive; it is suppressed if 
NBOX is negative.)
.IP LLBS 12
(an input array of type CHARACTER) provides a list of labels for the 
bar; the number of such labels may be equal to: 
.RS
.IP \(bu 4
the number of boxes less one, in which case the labels are
associated with the divisions between the boxes, or to
.IP \(bu 4
the number of boxes, in which case the labels are associated 
with the boxes themselves, or to 
.IP \(bu 4
the number of boxes plus one, in which case the first label is
associated with the beginning of the bar, the last label with 
the end of the bar, and the ones in between with the divisions
between the boxes.
.RE
.IP ""
The labels are drawn by calls to the Plotchar routine PLCHHQ.
They are drawn in the color specified by the Labelbar internal
parameter 'CLB' and using the polyline width specified by the
Labelbar internal parameter 'WLB'.
.IP NLBS 12
(an input expression of type INTEGER) specifies the number 
of labels in the array LLBS.
.IP LBAB 12
(an input expression of type INTEGER) specifies on which side or 
sides of the bar the labels are to be written. The value 0 
specifies that the bar is to be unlabeled, the value 1 that 
the labels are to be below a horizontal bar or to the right of 
a vertical bar, the value 2 that the labels are to be above a
horizontal bar or to the left of a vertical bar, 
and the value 3 that the labels are to be repeated on both 
sides of the bar. The labels will be scaled in such a way as 
not to overlap one another or to run outside the area in which 
the label bar is to lie (except perhaps for labels at the ends 
of the bar).
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncarg command to see the following relevant examples: 
colcon, cpex05, cpex07, cpex08, elblba, tconpa, and tlblba.
.SH ACCESS
To use LBLBAR or c_lblbar, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the labelbar man page for a description of all Labelbar error
messages and/or informational messages.
.SH SEE ALSO
Online:
labelbar, labelbar_params, lbfill, lbgeti, lbgetr, lbseti, lbsetr,
ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
