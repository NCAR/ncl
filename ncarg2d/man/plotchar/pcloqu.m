.TH PCLOQU 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PCLOQU - 
Draws characters of "low quality" by calling the GKS
character-drawing routines. No function codes may be used.
Using PCLOQU to draw a given string of characters will
create a smaller metafile than if PCHIQU or PCMEQU were
used; the results will depend on capabilities of the
translator.
.sp
PLCHLQ is an alternate name for the routine PCLOQU.
.SH SYNOPSIS
CALL PCLOQU (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pcloqu (float xpos, float ypos, char *chrs, 
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
Note that SIZE is defined in such a way as to be consistent
with PCHIQU.
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
CNTR < 0. means that (XPOS,YPOS) is the center of the left
edge of the first character.
.IP \(bu
CNTR > 0. means that (XPOS,YPOS) is the center of the right
edge of the last character.
.IP \(bu
CNTR = 0. means that (XPOS,YPOS) is the midpoint of the
string.
.RE
.IP "" 12
Note that this argument is not quite the same as that for
PCHIQU (though it does include the three most useful cases).
.sp
Upon return from PCLOQU, all arguments are unchanged.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
arex01,
tcnqck,
tcnsmt,
tcnsup,
tconre,
tezmap,
tgflas,
tgrida,
thafto,
thstgr,
tisohr,
tisosr,
tlblba,
tpltch,
tpwrzi,
tpwrzs,
tsoftf,
tstrml,
tthree,
tvelvc.
.SH ACCESS
To use PCLOQU or c_pcloqu, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
plotchar,
pcdlsc,
pcgetc,
pcgeti,
pcgetr,
pchiqu,
pcmequ,
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
