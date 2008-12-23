.TH AGPWRT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AGPWRT - 
Provides a way for the user to change the style of all text
strings drawn by Autograph.
.SH SYNOPSIS
SUBROUTINE AGPWRT (XPOS,YPOS,CHRS,NCHS,ISIZ,IORI,ICEN)
.SH DESCRIPTION 
.IP XPOS 12
(an input expression of type REAL) is the X coordinate
of a point relative to which the text string is to be
positioned.
.IP YPOS 12
(an input expression of type REAL) is the Y coordinate
of a point relative to which the text string is to be
positioned.
.IP CHRS 12
(an input expression of type CHARACTER) is the text
string to be drawn.
.IP NCHS 12
(an input expression of type INTEGER) is the length of
CHRS - the number of characters in the text string.
.IP ISIZ 12
(an input expression of type INTEGER) specifies the
size of the characters to be used. The values 0, 1, 2, and
3 imply character widths of 8, 12, 16, and 24 plotter
units, respectively. Larger values specify the character
width directly.
.IP IORI 12
(an input expression of type INTEGER) is the
orientation angle of the text string, measured in degrees
counter-clockwise from a vector which is horizontal and
pointing to the right.
.IP ICEN 12
(an input expression of type INTEGER) is the centering
option. The value "-1" implies that the text is to be
written with (XPOS,YPOS) in the center of the left edge of
the leftmost character, the value "0" that (XPOS,YPOS) is
to be in the center of the entire string, and the value
"+1" that (XPOS,YPOS) is to be in the center of the right
edge of the rightmost character.
.SH USAGE
This routine is not called by the user program, but by
Autograph itself, to draw a numeric label or an
informational label on the graph. The default version just
calls the plot-package routine PWRIT. The user may supply a
replacement version.
.sp
Note:  A user version of AGPWRT should not call any other
Autograph routine.
.SH ACCESS
To use AGPWRT, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
autograph,
agback,
agbnch,
agchax,
agchcu,
agchil,
agchnl,
agcurv,
agdshn,
aggetc,
aggetf,
aggeti,
aggetp,
aggetr,
agrstr,
agsave,
agsetc,
agsetf,
agseti,
agsetp,
agsetr,
agstup,
agutol,
anotat,
displa,
ezmxy,
ezmy,
ezxy,
ezy
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
