.\"
.\"	$Id: pwrzs.m,v 1.1 1993-03-11 16:34:15 haley Exp $
.\"
.TH PWRZS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
PWRZS - a character-plotting routine for plotting characters in
three-space when using SRFACE.
.SH SYNOPSIS
CALL PWRZS (X,Y,Z,ID,N,ISIZE,LINE,ITOP,ICNT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pwrzs (float x, float y, float z, char *id, int n, int isize, int line, int itop, int icnt)
.SH DESCRIPTION
.IP X,Y,Z 12
Positioning coordinates for the characters to be drawn.
These are floating point numbers in the same
three-space as used in SRFACE.
.IP ID 12
Character string to be drawn. ID is of type CHARACTER.
.IP N 12
The number of characters in ID.
.IP ISIZE 12
Size of the character:
.sp
.IP \(bu 2
If between 0 and 3, ISIZE is 1., 1.5, 2., or
3. times a standard width equal to 1/128th
of the screen width.
.sp
.IP \(bu 2
If greater than 3, ISIZE is the character
width in plotter address units.
.sp
.IP LINE 12
The direction in which the characters are to be
written.
1 = +X -1 = -X
2 = +Y -2 = -Y
3 = +Z -3 = -Z
.sp
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
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH USAGE
Call PWRZS after calling SRFACE and before calling FRAME.
.SH ACCESS
To use PWRZS load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_pwrzs load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
srface, ezsrfc, pwrzs, trn32s, ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

