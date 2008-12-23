'\" t
.TH SFSETP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SFSETP - Defines a new dot pattern.
.SH SYNOPSIS
CALL SFSETP (IDP) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_sfsetp (int idp[8][8]) 
.SH DESCRIPTION 
.IP IDP 12
(an input array of type INTEGER, dimensioned 8x8) contains 0s 
and 1s specifying the dot pattern to be transferred to the 
internal array LDP for use by SFWRLD and SFNORM. When dot fill 
is selected, each fill line is drawn using dots the same 
distance apart along the line as the lines are from each other.
The dots thus fall at the intersection points of a square grid.
Each dot is associated with a particular element of LDP. If 
that element is a 1, the dot is drawn; if it is a 0, the dot 
is not drawn. The association is done in such a way as to 
replicate the pattern specified by LDP across the entire 
filled area.
.sp
If the fill angle is 0 (the default), then incrementing the 
first subscript of LDP corresponds to a horizontal motion 
across the plot from left to right and incrementing the 
second subscript of LDP corresponds to a vertical motion 
across the plot from top to bottom. This allows the contents of 
IDP to be declared in a DATA statement which creates a 
"picture" of the pattern. For example, the FORTRAN statements 
.sp
.RS 16
 DIMENSION IDP(8,8)
.br
 ...
.br
 DATA IDP / 0,0,0,0,0,0,0,0,
.br
+           0,1,1,1,1,1,1,0,
.br
+           0,1,0,0,0,0,0,1,
.br
+           0,1,0,0,0,0,0,1,
.br
+           0,1,1,1,1,1,1,0,
.br
+           0,1,0,0,0,1,0,0,
.br
+           0,1,0,0,0,0,1,0,
.br
+           0,1,0,0,0,0,0,1/
.RE
.IP ""
creates the letter "R", with the correct orientation.
.sp
The contents of the array IDP are transferred to the internal 
array LDP. The new dot pattern is used until a subsequent call 
to SFSETP changes the contents of LDP again.
.sp
The default dot pattern consists of all 1s.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
This routine allows you to set the current value of 
Softfill's dot pattern. For a description of Softfill's dot
pattern parameter, see the softfill_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples:
sfex01, 
tsoftf.
.SH ACCESS
To use SFSETP or c_sfsetp, load the NCAR Graphics libraries ncarg, 
ncarg_gks, and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
softfill, softfill_params, sfgetc, sfgeti, sfgetp, sfgetr,
sfsetc, sfseti, sfsetr, sfsgfa, sfwrld, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
