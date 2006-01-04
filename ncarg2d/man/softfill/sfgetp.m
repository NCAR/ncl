'\" t
.TH SFGETP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SFGETP - Used to retrieve the current dot pattern.
.SH SYNOPSIS
CALL SFGETP (IDP) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_sfgetp (int idp[8][8]) 
.SH DESCRIPTION 
.IP IDP 12
(an output array of type INTEGER, dimensioned 8x8) contains 0s and 1s specifying the dot pattern currently contained in the internal array LDP. When dot fill is selected, each fill line is drawn using dots the same distance apart along the line as the lines are from each other. The dots thus fall at the intersection points of a square grid. Each dot is associated with a particular element of LDP. If that element is a 1, the dot is drawn; if it is a 0, the dot is not drawn. The association is done in such a way as to replicate the pattern specified by LDP across the entire filled area.
.sp
If the fill angle is 0 (the default), then incrementing the first subscript of LDP corresponds to a horizontal motion across the plot from left to right and incrementing the second subscript of LDP corresponds to a vertical motion across the plot from top to bottom. This allows the contents of IDP to be declared in a DATA statement which creates a "picture" of the pattern. For example, the FORTRAN statements 
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
The default dot pattern consists of all 1s.
.SH USAGE
This routine allows you to retrieve the current value of 
Softfill's dot pattern. For a description of Softfill's dot
pattern parameter, see the softfill_params man page.
.SH ACCESS
To use SFGETP or c_sfgetp, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
softfill, softfill_params, sfgetc, sfgeti, sfgetr, sfsetc, sfseti,
sfsetp, sfsetr, sfsgfa, sfwrld, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2006
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

