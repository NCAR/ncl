.TH Labelbar_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Labelbar_params - This document briefly describes all 
Labelbar internal parameters.
.SH DESCRIPTION 
Labelbar has six internal parameters, each of which affects
the behavior of LBLBAR in a particular way. The current value
of one of these parameters may be retrieved by calling either
LBGETI or LBGETR and the value of one of them may be reset 
by calling either LBSETI or LBSETR. 
.sp
Parameter descriptions, in alphabetical order, of all Labelbar
internal parameters follow. Each description begins with a line
giving the parameter name and the intrinsic FORTRAN type of the
parameter.
.IP "'CBL' - Integer"
Color of box lines. The default value is -1.
.IP "'CFL' - Integer"
Color of fill lines. The default value is -1.
.IP "'CLB' - Integer"
Color of labels. The default value is -1.
.IP "'WBL' - Real"
Width of box lines. The default value is 0.
.IP "'WFL' - Real"
Width of fill lines. The default value is 0.
.IP "'WLB' - Real"
Width of label lines. The default value is 0.
.PP
Each of the "color" parameters may be given a negative value 
to indicate that color is unspecified; in this case, the 
color of an object drawn will be that specified by the 
current polyline color index (in the case of box lines 
and fill lines) or the current text color index (in the case 
of labels). A value which is zero or greater specifies the 
color index to be used.
.sp
Each of the "width" parameters may be given a value of zero 
or less to indicate that line width is unspecified; in this 
case, the line width used to draw an object will be that 
specified by the current polyline width scale factor. 
Note that setting polyline width before drawing the 
labels does affect their appearance; this is because 
labels are drawn by calls to the routine PLCHHQ, which 
strokes out characters using lines.
.SH SEE ALSO
Online:
lbgeti, lbgetr, lbseti, lbsetr
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2005
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
