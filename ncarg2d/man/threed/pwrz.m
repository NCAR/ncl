.TH PWRZT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PWRZT - Draws the projections of character strings that are
positioned in a plane parallel to one of the three
coordinate planes.
.PP
NOTE: At one time, a Threed routine called PWRZ was supported.  PWRZ has been
superseded by PWRZT and is therefore considered obsolete.  If you call PWRZ,
you will get an error message telling you to use PWRZT instead and execution
will be terminated.
.SH SYNOPSIS
CALL PWRZT (U,V,W,CHRS,LCHRS,ISIZE,IDIR,ITOP,ICEN)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pwrzt (float u, float v, float w, char *chrs, int lchrs,
.br
int isize, int idir, int itop, int icen)
.SH DESCRIPTION
.IP U,V,W 12
(input expressions of type REAL) are the U, V, and W coordinates of a point,
in the user's 3-space, relative to which the character string is to be
positioned.
.IP CHRS 12
(an input constant or variable of type CHARACTER) is the character string to
be drawn.  It may contain uppercase alphabetic characters, the digits 0
through 9, and a small set of "special" characters (plus, minus, asterisk,
slash, left and right parentheses, equals sign, blank, comma, and period).
Other characters are treated as blanks.  (Note especially that lowercase
alphabetic characters are not available.)
.IP LCHRS 12
(an input expression of type INTEGER) is the number of characters in CHRS.
.IP ISIZE 12
(an input expression of type INTEGER) specifies the character width to be
used, defined in terms of the width of the plotter frame.  Because projected
characters vary in width depending on just where they are placed within the
box being viewed and the position from which that box is viewed, ISIZE is
interpreted as specifying the width of a character when that character is
positioned and viewed in such a way as to make it as large as it could
possibly be - when the character is on the near side of the box and in a
plane perpendicular to the line of sight.  Specifically,
.RS
.IP "  " 4
If between 0 and 3, ISIZE is 1., 1.5, 2., or
3. times a standard width equal to 1/128th
of the screen width.
.IP "  " 4
If greater than 3, ISIZE is the character width in units of 1/1024th of the
plotter frame.
.RE
.IP " " 12
One third of the "width" referred to here is white space.  What ISIZE really
specifies is the distance between the centers of adjacent characters in a
string.  Characters are digitized to be 7/6 * ISIZE units high and 4/6 *
ISIZE units wide, excluding white space.
.IP IDIR 12
(an input expression of type INTEGER) is the direction in which the character
string is to be written, as follows:
.RS
.IP "  " 4
1 = +U    -1 = -U
.IP "  " 4
2 = +V    -2 = -V
.IP "  " 4
3 = +W    -3 = -W
.RE
.IP ITOP 12
(an input expression of type INTEGER) is the direction from the center of the
first character to the top of the first character; possible values of ITOP
are the same as those of IDIR, above.  ABS(ITOP) must not be equal to
ABS(IDIR).
.IP ICEN 12
(an input expression of type INTEGER) is the centering option, specifying
where (U,V,W) is relative to the string written, as follows:
.RS
.IP "-1" 4
(U,V,W) is the center of the left edge of the first character.
.IP " 0" 4
(U,V,W) is the center of the entire string.
.IP " 1" 4
(U,V,W) is the center of the right edge of the last character.
.RE
.PP
Because characters drawn by PWRZT are stroked using the GKS polyline
primitive (so that they can be projected from 3-D to 2-D), they are drawn
in the current polyline color, as determined by the last call to the GKS
routine GSPLCI; by default, color index 1 is used. Line width is determined
by the last call to the GKS routine GSLWSC; by default, the line width scale
factor is 1.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
tpwrzt.
.SH ACCESS
To use PWRZT or c_pwrzt, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
threed,
curve3,
fence3,
frst3,
line3,
perim3,
point3,
psym3,
pwrz,
set3,
threed,
tick3,
tick43,
vect3,
ncarg_cbind.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
