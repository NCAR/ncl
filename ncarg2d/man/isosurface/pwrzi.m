.TH PWRZI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PWRZI - A character-plotting routine for plotting characters in
three-space when using Isosurface.  For a large class of possible
positions, the hidden character problem is solved.
.SH SYNOPSIS
SUBROUTINE PWRZI (X,Y,Z,ID,N,ISIZE,LINE,ITOP,ICNT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pwrzi (float x, float y, float z, char *id, int n, 
.br
int isize, int line, int itop, int icnt)
.SH DESCRIPTION
.IP X,Y,Z 12
(input expressions of type REAL) are positioning coordinates for the
characters to be drawn.
These are floating point numbers in the same
three-space coordinate system as that used in Isosurface.
.IP ID 12
(an input expression of type CHARACTER) is the character string to be
drawn.
.IP N 12
(an input expression of type INTEGER) is the number of characters in ID.
.IP ISIZE 12
(an input expression of type INTEGER) specifies the size of the characters
to be used:
.RS
.IP \(bu 2
If between 0 and 3, ISIZE is 1., 1.5, 2., or
3. times a standard width equal to 1/128th
of the screen width.
.IP \(bu 2
If greater than 3, ISIZE is the character
width in plotter address units (by default, 1/1024th of the screen width).
.RE
.IP LINE 12
(an input expression of type INTEGER) specifies the direction in which the
characters are to be written.
.sp
.nf
+1 = +X
-1 = -X
+2 = +Y
-2 = -Y
+3 = +Z
-3 = -Z
.fi
.IP ITOP 12
(an input expression of type INTEGER) specifies the direction from the
center of the first character to
the top of the first character (the potential values
for ITOP are the same as those for LINE as given
above). Note that LINE cannot equal ITOP even in
absolute value.
.IP ICNT 12
(an input expression of type INTEGER) is the centering option:
.RS
.IP -1 4
(X,Y,Z) is the center of the left edge of the first character.
.IP +0 4
(X,Y,Z) is the center of the entire string.
.IP +1 4
(X,Y,Z) is the center of the right edge of the last character.
.RE
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
Call PWRZI after calling ISOSRF and before calling FRAME.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tpwrzi,
fsrpwrzs.
.SH ACCESS
To use PWRZI or c_pwrzi, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
isosurface, isosurface_params, ezisos, 
isgeti, isgetr, isosrf, isseti, issetr, 
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
