.TH PCMEQU 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PCMEQU - 
Draws characters of "medium quality". It does this
by drawing lines, just as PCHIQU does, but it does not
produce quite such fancy characters. No function codes may
be used. Using PCMEQU to draw a given string of characters
will create a larger metafile than if PCLOQU were used,
which may be a disadvantage. However, it may also be more
dependable, in that it does not depend on capabilities the
translator may or may not have.
.sp
PLCHMQ is an alternate name for the routine PCMEQU.
.SH SYNOPSIS
CALL PCMEQU (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pcmequ (float xpos, float ypos, char *chrs, 
.br
float size, float angd, float cntr)
.SH DESCRIPTION 
.IP "XPOS,YPOS" 12
(input expressions of type REAL) specify
positioning coordinates for the characters to be drawn. If
the internal parameter \'MA\' has the value 0, these are
given in the current user coordinate system; otherwise,
they are given in an arbitrary X/Y coordinate system, as
implied by the value of \'MA\' and the nature of the routine
CPMPXY. (For example, if \'MA\' has the value 1 and the
default version of CPMPXY is being used, then XPOS is a
longitude, in degrees, and YPOS is a latitude, in degrees.)
The argument CNTR (described below) specifies how the
characters are to be positioned relative to the point
(XPOS,YPOS).
.IP CHRS 12
(an input constant or variable of type CHARACTER) contains a character string
to be drawn. The number of characters in CHRS is taken to
be LEN(CHRS); to use characters "m" through "n" from a
character variable CHRS, use the FORTRAN-77 substring
notation "CHRS(m:n)". The string may include any of the 95
characters space, exclamation point, double quote, pound
sign, dollar sign, percent sign, ampersand, apostrophe,
left parenthesis, right parenthesis, asterisk, plus sign,
comma, minus sign, period, slash, 0-9, colon, semi-colon,
less than sign, equals sign, greater than sign, question
mark, at sign, A-Z, left square bracket, backslash, right
square bracket, hat, underscore, backwards quote, a-z, left
curly bracket, vertical bar, right curly bracket, and
tilde. Function codes, like those in calls to PCHIQU, may
not be used.
.IP SIZE 12
(an input expression of type REAL)
specifies the desired character size. If the internal
parameter \'MA\' is zero, then the following comments apply:
.RS
.IP \(bu
If SIZE is less than or equal to zero, its absolute value
specifies the size as a multiple of a default digitized
size on a 1024x1024 grid, on which blanks are 16 units wide.
.IP \(bu
If SIZE is greater than zero, but less than one, it
specifies the desired width of a blank as a fraction of the
distance across the plotter frame. This is the recommended
scheme.
.IP \(bu
If SIZE is greater than or equal to one, it specifies the
desired width of a blank in plotter coordinates, as defined
by default or by a user\'s call to the SPPS routine SETI.
Note that use of the routine SETI is now discouraged.
.RE
.IP "" 12
If \'MA\' is nonzero, then SIZE is the desired width of a
blank as a value in the X/Y coordinate system in which XPOS
and YPOS are given.
.sp
Capital letters are 4/6 as wide as a blank and, by default,
7/4 as high as they are wide (the internal parameter \'HW\'
may be used to change the latter value). The distance from
the center of each character to the center of the next is
constant and is equal to one blank width.
.sp
Note that, if the desired character height ("h") is known,
one should use SIZE = (6/7) * h.
.sp
SIZE is defined in such a way as to be consistent with
PCHIQU. If one changes a "CALL PCHIQU" to a "CALL PCMEQU"
(assuming that no function codes are used in the string, of
course), the results will be much the same, but the length
of the string will be different. If one uses SIZE less than
zero and one has reset the PCHIQU internal parameter \'PW\'
to something other than 16, the difference in the output of
the two routines will be greater.
.IP ANGD 12
(an input expression of type REAL)
is the angle, in degrees counterclockwise from the
positive X axis, at which the character string is to be
written.
.IP CNTR 12
(an input expression of type REAL)
is the centering option, as follows:
.RS
.IP \(bu
CNTR = -1. means that (XPOS,YPOS) is the center of the left
edge of the first character.
.IP \(bu
CNTR = 1. means that (XPOS,YPOS) is the center of the right
edge of the last character.
.IP \(bu
CNTR = s means that (XPOS,YPOS) is a point obtained by
linear interpolation along the line joining the two points
mentioned above (the first point being associated with the
value -1 and the second with the value +1). The value "0."
gives the midpoint of the line.
.RE
.IP "" 12
Upon return from PCMEQU, all arguments are unchanged.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cbex01,
epltch,
tpltch.
.SH ACCESS
To use PCMEQU or c_pcmequ, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
plotchar,
pcdlsc,
pcgetc,
pcgeti,
pcgetr,
pchiqu,
pcloqu,
pcmpxy,
pcpnwi,
pcrset,
pcsetc,
pcseti,
pcsetr,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
