.TH PWRZT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PWRZT - A character-plotting routine for plotting characters in
three-space when using Threed.
.sp
PSYM3 is an alternate name for the routine PWRZT.
.SH SYNOPSIS
CALL PWRZT (X,Y,Z,ID,N,ISIZE,LINE,ITOP,ICNT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pwrzt (float x, float y, float z, char *id, int n, 
.br
int isize, int line, int itop, int icnt)
.SH DESCRIPTION
.IP X,Y,Z 12
Positioning coordinates for the characters to be drawn.
These are floating point numbers in the same
three-space as used in THREED.
.IP ID 12
Character string to be drawn. ID is of type CHARACTER.
.IP N 12
The number of characters in ID.
.IP ISIZE 12
Size of the character:
.RS
.IP \(bu 2
If between 0 and 3, ISIZE is 1., 1.5, 2., or
3. times a standard width equal to 1/128th
of the screen width.
.IP \(bu 2
If greater than 3, ISIZE is the character
width in plotter address units.
.RE
.IP LINE 12
The direction in which the characters are to be
written.
.RS 17
.sp
1 = +X -1 = -X
.br
2 = +Y -2 = -Y
.br
3 = +Z -3 = -Z
.RE
.IP ITOP 12
The direction from the center of the first character to
the top of the first character (the potential values
for ITOP are the same as those for LINE as given
above). Note that LINE cannot equal ITOP even in
absolute value.
.IP ICNT 12
Centering option.
-1 (X,Y,Z) is the center of the left edge of
the first character.
0 (X,Y,Z) is the center of the entire string.
1 (X,Y,Z) is the center of the right edge of
the last character.
.sp
Note that the hidden character problem is solved correctly for
characters near, but not inside, the three-space object.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
Call PWRZT after calling THREED and before calling FRAME.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
tpwrzt.
.SH ACCESS
To use PWRZT, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_pwrzt, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
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
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

