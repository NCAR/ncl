.TH PCHIQU 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PCHIQU - 
Draws high quality characters. By default, it uses
the same database as the old NCAR Graphics routine PWRITX,
but it can also use characters from any of the fontcap-defined
databases, it has an improved interface, and it has
many more capabilities than PWRITX.
.sp
PLCHHQ is an alternate name for the routine PCHIQU.
.SH SYNOPSIS
CALL PCHIQU (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_pchiqu (float xpos, float ypos, char *chrs, 
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
.IP "CHRS" 12 
(an input constant or variable of type CHARACTER) specifies
the characters to be drawn.  The number of characters in CHRS
is taken to be LEN(CHRS); to use characters "m" through "n"
from a character variable CHRV, use the FORTRAN-77
substring notation "CHRV(m:n)". CHRS may contain characters
to be drawn and "function codes", which are used for various
purposes.  The possible function codes are completely
described in the programmer document for Plotchar and are
summarized in a later section of this man page.
.IP "SIZE" 12
(an input expression of type REAL)
specifies the desired character size. If the internal
parameter \'MA\' is zero, then the following comments apply:
.RS
.IP \(bu
If SIZE is less than or equal to zero, its absolute value
specifies the size as a multiple of the digitized size. On
a 1024x1024 grid, principal-size capitals are about \'PW\'
units wide by \'PH\' units high and the vertical spacing
between lines is \'PV\' units (by default, \'PW\', \'PH\', and
\'PV\' are 16, 21, and 32). Indexical-size capitals are \'IW\'
units wide by \'IH\' units high and the vertical spacing
between lines is \'IV\' units (by default, \'IW\', \'IH\', and
\'IV\' are 12, 13, and 20). Cartographic-size capitals are
\'CW\' units wide by \'CH\' units high and the vertical spacing
between lines is \'CV\' units (by default, \'CW\', \'CH\', and
\'CV\' are 8, 9, and 14).
.sp
Example: Using CHRS = \'ABC\' and SIZE = -2. will write three
characters, each of which is about 32/1024 of the plotter
frame in width.
.IP \(bu
If SIZE is greater than zero, but less than one, it
specifies the desired approximate width of a principal-size
capital as a fraction of the distance across the plotter
frame. This is the recommended scheme.
.sp
Example: Using CHRS = \'ABC\' and SIZE = .05 will write
three characters, each of which is about 5 hundredths of
the plotter frame in width.
.IP \(bu
If SIZE is greater than or equal to one, it specifies the
desired approximate width of a principal-size capital in
plotter coordinates, as defined by default or by a user
call to the SPPS routine SETI. Note that use of the routine
SETI is now discouraged.
.sp
Example: Using CHRS = \'ABC\' and SIZE = 13. (assuming there
have been no calls to the SPPS routine SETI) will write
three characters, each of which is about 13/1024 of the
plotter frame in width.
.RE
.IP "" 12
If \'MA\' is nonzero, then SIZE is the desired approximate
width of a principal-size capital as a value in the X/Y
coordinate system in which XPOS and YPOS are given.
.sp
In most of the databases used by PCHIQU, the character
width varies from character to character. Most principal-size
capitals will be a little wider than the value
specified by SIZE (and a few will be narrower). Thus, a
little experimenting may be necessary to get just the size
one wants. A rule of thumb that works pretty well when
using all capitals is as follows:
.sp
.in +5
SIZE = DLOS / REAL(NCIS)
.in -5
.sp
where "DLOS" is the desired length of the string, in the
fractional system, and "NCIS" is the number of characters
in the string.
.sp
Note: As of version 3.2 of NCAR Graphics, the size of all
characters written by PCHIQU has been reduced by a
multiplicative factor (the internal parameter \'SA\') whose
default value is .88888888... The object of this is to make
the size of characters written by PCHIQU consistent with
the size of characters written by PCMEQU and PCLOQU. If the
value of \'SA\' is not changed, then the height of principal-size
capitals written by PCHIQU and PCMEQU will be 7/6
times the value specified by SIZE. Thus, if the desired
character height (CHRH) is known, use "SIZE = 6.*CHRH/7." .
.sp
The values of the internal parameters \'PW\', \'PH\', \'PV\',
\'IW\', \'IH\', \'IV\', \'CW\', \'CH\', and \'CV\' (as well as two
others, \'PS\' and \'IS\') may be changed by user calls to
CPSETR. This can be used to change the effective size and
shape of digitized characters in the PWRITX database (the
effect is the same as if the characters had been digitized
differently at the outset). If, for example, one were to
double the value of \'PH\', it would make the principal
characters twice as tall as they are by default. (In this
case, one would probably also want to double the value of
\'PV\', so as to make the vertical spacing between lines
consistent with the character height.) Note that, if one
changes the value of \'PW\', \'IW\', or \'CW\', the meaning of a
negative value of the argument SIZE may thereby be changed.
.IP ANGD 12
(an input expression of type REAL)
is the angle, in degrees counterclockwise from the
positive X axis, at which the character string is to be
written. If the internal parameter \'TE\' is nonzero (by
default, it is zero) and if ANGD is exactly 360., then no
characters are drawn by PCHIQU; it just computes the
distances, in the fractional coordinate system, from the
point (XPOS,YPOS) to the left edge, the right edge, the
bottom edge, and the top edge of a box enclosing the
string. These are stored as the values of the parameters
\'DL\', \'DR\', \'DB\', and \'DT\' and may be retrieved by calls to
PCGETR.
.IP CNTR 12
(an input expression of type REAL)
is the centering option. If the internal parameter
\'CE\' is zero (the default), then
.RS
.IP \(bu
CNTR = -1. means that (XPOS,YPOS) is the center of the
left edge of the first character (if that character is to
be written "across" the frame) or the center of the top
edge of the first character (if that character is to be
written "down" the frame).
.IP \(bu
CNTR = 1. means that (XPOS,YPOS) is the center of the
right edge of the last character (if the writing direction
at the end of the character string is "across" the frame)
or the center of the bottom edge of the last character (if
the writing direction at the end of the character string is
"down" the frame).
.IP \(bu
CNTR = s, where "s" is any real number, means that
(XPOS,YPOS) is a point obtained by linear interpolation
along the line joining the two points mentioned above (the
first point being associated with the value -1. and the
second with the value +1.). The value "0." gives the
midpoint of the line.
.RE
.IP "" 12
If \'CE\' is nonzero, then the value of CNTR is ignored.
Text-extent quantities are computed and used to exactly
center the output on the point (XPOS,YPOS). This is useful,
among other things, for labeling each of a number of
points with a single character; however, it works as
desired only when \'QU\' is zero.
.sp
Upon return from PCHIQU, all arguments are unchanged.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH FUNCTION CODES
The function codes that may be used for various purposes in the input
character string CHRS are described completely in the programmer document
for Plotchar.  The material that follows will principally be of use as
a memory aid to someone who is already familiar with the use of function
codes.
.sp
As the characters of CHRS are scanned from left to right, the scanner is
always in one of two states: either it is looking for characters to be
drawn or it is looking for characters to be interpreted as function codes.
Each occurrence of the function-code signal character (defined by the
internal parameter \'FC\' - a colon by default) flips the state of the
scanner.  Thus, in the character string \'ABC:L:DEF\', "A", "B", and "C"
will be treated as characters to be drawn, "L" will be treated as a
function code, and "D", "E", and "F" will be treated as characters to be
drawn.  (In this particular example, since the function code "L" requests
lower case, what would be drawn is "ABCdef".)  Occasionally, function
codes may need to be separated by commas or blanks; this is necessary only
when there would otherwise be a syntactical problem.
.IP "F" 12
Switch to using the database specified by the value of \'FN\'.
.IP "F0" 12
Switch to using the PWRITX database.
.IP "Fn" 12
Switch to using fontcap-defined font "n".
.IP "R" 12
Switch to the "Roman" part of the PWRITX database.
If characters are currently being used from a fontcap-defined font, this
function code will have no effect.
.IP "G" 12
Switch to the "Greek" part of the PWRITX database.
If characters are currently being used from a fontcap-defined font, this
function code will have no effect.
.IP "P" 12
Use characters of principal size from the PWRITX database.
If characters are currently being used from a fontcap-defined font, there
will be an appropriate shift in size.
.IP "I" 12
Use characters of indexical size from the PWRITX database.
If characters are currently being used from a fontcap-defined font, there
will be an appropriate shift in size.
.IP "K" 12
Use characters of cartographic size from the PWRITX database.
If characters are currently being used from a fontcap-defined font, there
will be an appropriate shift in size.
.IP "U" 12
Switch to using upper case letters from the PWRITX database.
If characters are currently being used from a fontcap-defined font, this
function code will have no effect.
.IP "Un" 12
For the next "n" characters, switch to using upper case letters from the
PWRITX database and then switch to lower case.
If characters are currently being used from a fontcap-defined font, this
function code will have no effect.
.IP "L" 12
Switch to using lower case letters from the PWRITX database.
If characters are currently being used from a fontcap-defined font, this
function code will force the use of lower case letters from it.
.IP "Ln" 12
For the next "n" characters, switch to using lower case letters from the
PWRITX database and then switch to upper case.
If characters are currently being used from a fontcap-defined font, this
function code will force the use of lower case letters from it.
.IP "A" 12
Switch to writing "across" the frame, in the direction ANGD.  This is the
normal mode.
.IP "D" 12
Switch to writing "down" the frame, in the direction ANGD-90.  This causes
each character to be written beneath the previous one.
.IP "B" 12
Switch to subscript level.
.IP "Bn" 12
For the next "n" characters, use subscript level and then revert to normal
level.
.IP "S" 12
Switch to superscript level.
.IP "Sn" 12
For the next "n" characters, use superscript level and then revert to normal
level.
.IP "E" 12
Switch to normal level in such a way as to allow for another sub- or
superscript on the base character.
.IP "N" 12
Switch to normal level in such a way as not to allow for another sub- or
superscript on the base character.
.IP "Hn" 12
Offset by "n" digitization units in the direction ANGD.
.IP "HnQ" 12
Offset by "n" blank widths in the direction ANGD.
.IP "Vn" 12
Offset by "n" digitization units in the direction ANGD+90.
.IP "VnQ" 12
Offset by "n" blank heights in the direction ANGD+90.
.IP "C" 12
Do a "carriage return".
.IP "Xn or XnQ" 12
Zoom character width to "n" percent of normal.
.IP "Yn" 12
Zoom character height to "n" percent of normal.
.IP "YnQ" 12
Zoom character height to "n" percent of normal and introduce a shift in the
direction ANGD+90 sufficient to keep the bases of characters properly
aligned.
.IP "Zn" 12
Same as "XnYn".
.IP "ZnQ" 12
Same as "XnQYnQ".
.IP "nnnn" 12
An octal number may be used as a function code to select a particular
character from the PWRITX database.  The octal number for a given character
is the sum of a font index (0 for Roman or 600 for Greek), a size index
(0 for Principal, 200 for Indexical, or 400 for Cartographic), a case
index (0 for Upper or 100 for Lower), and the octal equivalent of a
character index (1-32 for A through Z, 33-44 for 0 through 9, or 45-57
for the individual characters, +, -, *, /, (, ), $, =, blank, comma, or
period).
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cmpit,
cmpita,
cmptit,
cpexcc,
elblba,
cbex01,
coex01,
coex02,
coex03,
cpex10,
epltch,
sfex02,
srex01,
tpltch,
fcoord1,
fcoord2.
.SH ACCESS
To use PCHIQU or c_pchiqu, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
plotchar,
pcdlsc,
pcgetc,
pcgeti,
pcgetr,
pcloqu,
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
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
