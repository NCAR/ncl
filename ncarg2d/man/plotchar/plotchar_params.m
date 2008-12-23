.TH Plotchar_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Plotchar_params - This document briefly describes all the
internal parameters of Plotchar.
.SH DESCRIPTION 
The following shows all of the internal parameters
that affect the behavior of PCHIQU. Each entry includes the
name of a parameter, its FORTRAN type, its default value, and
a short description of the parameter.
.IP "Internal Parameters of PCHIQU"
.RS
.IP "\'AS\'   -   Real   -    0."
When this parameter is nonzero,
it forces extra space between
characters. A negative value is
interpreted in digitization
units. A positive value is
interpreted as a multiplier of
the nominal character width
specified by the argument SIZE
(and, in the case of the PWRITX
database, one of \'PW\', \'IW\', or
\'CW\', whichever is appropriate).
.IP "\'BC\'   -   Integer array   -   all -1's"
An array of three "box colors".
Element 1 applies to the outline
of the box, element 2 to fill of
the box, and element 3 to fill
of the box shadow. The value "-1"
means "unspecified"; and a
value greater than or equal to 0
selects a particular color
index. To access element 1, use
the parameter name \'BC(1)\'; to
access element 2, use \'BC(2)\';
etc. The name \'BC\' by itself
refers to element 1.
.IP "\'BF\'   -   Integer   -   0"
When this parameter is nonzero,
it requests a box around each
string of characters written by
PCHIQU. The value is interpreted
as a three-bit flag: Add 1 to
the value to have the outline of
the box drawn, 2 to have the box
filled, and 4 to have its shadow
drawn. (1 implies "draw
outline", 2 implies "fill box",
3 implies "fill box and draw
outline", 4 implies "fill
shadow", 5 implies "fill shadow
and draw outline", 6 implies
"fill shadow and fill box", and
7 implies "fill shadow, fill
box, and draw outline".)
.IP "\'BL\'   -   Real   -   0."
When this parameter is greater
than zero, it specifies the line
width to be used while drawing
the box around a character
string. A value less than or
equal to zero means
"unspecified".
.IP "\'BM\'   -   Real   -   .15"
The width of box margins (the
space between the characters and
the outline of the box), stated
as a fraction of principal
character height.
.IP "\'BX\'   -   Real   -    -.05"
The X offset for box shadows,
stated as a fraction of
principal character height. This
is actually an offset in the
direction ANGD.
.IP "\'BY\'   -   Real   -    -.05"
The Y offset for box shadows,
stated as a fraction of
principal character height. This
is actually an offset in the
direction ANGD+90.
.IP "\'CC\'   -   Integer array   -   all -1\'s"
An array of color indices.
Element 0 specifies the
principal character color and
elements 1 through 16 specify
special colors to be used for
certain characters. The value -1
means "unspecified"; a value
greater than or equal to zero is
a particular color index to be
used. The routine PCDLSC may be
called to define a standard
default set of colors for
elements 1 through 16. To access
element 0, use the parameter
name \'CC(0)\'; to access element
1, use \'CC(1)\'; etc. The name
\'CC\' by itself refers to element
0.
.IP "\'CD\'   -   Integer   -   0"
The value 0 selects the complex
dataset from the PWRITX
database; 1 selects the duplex
dataset.
.IP "\'CE\'   -   Integer   -   0"
The value 0 selects centering of
strings as specified by the
argument CNTR and 1 selects
exact centering, using text-extent
quantities. This works as
desired only when \'QU\' is zero.
.IP "\'CH\'   -   Real   -   9."
Digitized cartographic height.
.IP "\'CL\'   -   Real   -   0."
If greater than zero, this is
the desired principal line width
to be used while drawing
characters. A value less than or
equal to zero says that line
width is unspecified.
.IP "\'CS\'   -   Real   -   0."
Constant-spacing flag. The 
high-quality character sets normally
have variable spacing when
written "across" the frame. When
this parameter is set nonzero,
it forces the centers of the
characters to be spaced a
constant distance apart. If a
negative value is used, its
absolute value specifies the
spacing in digitization units.
If a positive value is used, it
is interpreted as a multiplier
of the nominal character width
specified by the argument SIZE
(and, in the case of the PWRITX
database, one of \'PW\', \'IW\', or
\'CW\', whichever is appropriate).
Medium- and low-quality
characters are not affected by
this parameter.
.IP "\'CV\'   -   Real   -   14."
Digitized cartographic vertical
spacing.
.IP "\'CW\'   -   Real   -   8."
Digitized cartographic width.
.IP "\'DB\'   -   Real   -   none"
Output only - distance from
(XPOS,YPOS) to the bottom edge
of the text-extent box for the
last string drawn, in the
fractional system.
.IP "\'DL\'   -   Real   -   none"
Output only - distance from
(XPOS,YPOS) to the left edge of
the text-extent box for the last
string drawn, in the fractional
system.
.IP "\'DO\'   -   Integer   -   1"
The drawing order for the
characters. If the value is
positive, characters are drawn
in the order in which they appear
in the input character string;
otherwise, they are drawn in the
opposite order. If the absolute
value is a 1, all shadows are
drawn, followed by all character
bodies, followed by all
outlines. If the absolute value
is 2 or greater, each character
is drawn completely (including
its shadow, its body, and its
outline) before moving on to the
next character; this can be
used, together with \'SS\', to
"stack" characters so that they
apparently overlap one another.
.IP "\'DR\'   -   Real   -   none"
Output only - distance from
(XPOS,YPOS) to the right edge of
the text-extent box for the last
string drawn, in the fractional
system.
.IP "\'DT\'   -   Real   -   none"
Output only - distance from
(XPOS,YPOS) to the top edge of
the text-extent box for the last
string drawn, in the fractional
system.
.IP "\'FB\'   -   Real   -   \.00003"
Fidelity parameter for Bezier
curves used to draw the filled
fonts. The value specifies how
close the interpolated curve
should be to the actual Bezier
curve and is specified as a
fraction of the height of the
plotter frame.
.IP "\'FC\'   -   Character   -   \':'\ (a colon)"
Function-code character.
.IP "\'FN\'   -   Character or Integer   -   0"
Font number. The default value,
0, implies the use of the PWRITX
database. Use a positive value
"n" to force the use of fontcap
database "n". Use a character-string
font name, as shown in table 2 in
the programmer document for
PLOTCHAR, in a call to PCSETC; a
call to PCGETC will return one
of these font names.
The list of font names is also
reproduced later in this man page.
.IP "\'IH\'   -   Real   -   13."
Digitized indexical height.
.IP "\'IS\'   -   Real   -   7."
Offset to be used when
subscripting or superscripting a
character of indexical size, in
digitization units.
.IP "\'IV\'   -   Real   -   20."
Digitized indexical vertical
spacing.
.IP "\'IW\'   -   Real   -   12."
Digitized indexical width.
.IP "\'MA\'   -   Integer   -   0"
The mapping flag. The value zero
says that no mapping is to
occur; nonzero values say that
PCMPXY is to be called and
select particular mappings.
.IP "\'OC\'   -   Integer   -   1"
The outline color specifier. The
value "-1" says that outline
color is unspecified; a value of
0 or greater selects a
particular color index.
.IP "\'OF\'   -   Integer   -   0"
The outline flag. The value 0
says that outlines are not to be
drawn; a nonzero value says
that outlines are to be drawn.
.IP "\'OL\'   -   Real   -   0."
The outline line width. A value
less than or equal to zero says
that outline width is
unspecified; a value greater
than zero is the desired line
width, as a fraction of "normal".
.IP "\'OR\'   -   Real   -   0."
The out-of-range flag. The value
0. says that no values returned
by the routine PCMPXY are "out
of range"; any other value is
the value to be used as an 
out-of-range signal, indicating that
a point to be mapped is
invisible under the current
mapping.
.IP "\'PH\'   -   Real   -   21."
Digitized principal height.
.IP "\'PS\'   -   Real   -   10."
Offset to be used when
subscripting or superscripting a
character of principal size, in
digitization units.
.IP "\'PV\'   -   Real   -   32."
Digitized principal vertical
spacing.
.IP "\'PW\'   -   Real   -   16."
Digitized principal width.
.IP "\'QU\'   -   Integer   -   0"
Quality flag. The value 0 means
to use the high-quality
characters, 1 means to use the
medium-quality characters of
PCMEQU, and 2 means to use the
"low-quality" characters of
PCLOQU.
.IP "\'SA\'   -   Real   -   .88888888..."
A multiplier for the sizes of
all characters written directly
by PCHIQU, introduced in version
3.2 of NCAR Graphics in order to
make characters written by
PCHIQU the same size as those
written by PCMEQU. The default
value is (16/21)(7/6). Those
users who want PCHIQU to produce
the same size characters that it
did before may set \'SA\' to 1.,
but this is not recommended.
.IP "\'SC\' - Integer - 0"
The shadow color specifier. The
value "-1" says that shadow
color is unspecified; a value of
0 or greater selects a
particular color index.
.IP "\'SF\'   -   Integer   -   0"
The shadow flag. The value 0
says that shadows are not to be
drawn; a nonzero value says
that shadows are to be drawn.
.IP "\'SL\'   -   Real   -   0."
The shadow line width. A value
less than or equal to zero says
that shadow line width is
unspecified; a value greater
than zero is the desired line
width, as a fraction of "normal".
.IP "\'SS\'   -   Real   -   0."
Subtract-space flag. When this
parameter is nonzero, it
reduces the space between
characters. If a negative value
is used, it is interpreted in
digitization units. If a
positive value is used, it is
interpreted as a multiplier of
the nominal character width
specified by the argument SIZE
(and, in the case of the PWRITX
database, one of \'PW\', \'IW\', or
\'CW\', whichever is appropriate).
.IP "\'SX\'   -   Real  -   -.05"
The X offset for character
shadows, stated as a fraction of
principal character height. This
is actually an offset in the
direction ANGD.
.IP "\'SY\'   -   Real   -   -.05"
The Y offset for character
shadows, stated as a fraction of
principal character height. This
is actually an offset in the
direction ANGD+90.
.IP "\'TE\'   -   Integer   -   0"
The text-extent computation
flag. Zero means do not compute
text-extent quantities, nonzero
means do compute them. If \'TE\'
is nonzero and the value of the
argument ANGD, in a call to
PCHIQU, is exactly 360. instead
of 0., no characters are drawn,
but the text-extent quantities
are still computed. This
provides a way to get these
quantities prior to actually
drawing a particular string.
.IP "\'UN\'   -   Integer   -   3"
This used to be the FORTRAN logical
unit number to be used in reading
the data defining the various
character sets.  It is no longer
used for anything.
.IP "\'XB\'   -   Real   -   0."
For retrieval only. The X
position at the beginning of the
last string written by PCHIQU,
in the fractional system.
.IP "\'XC\'   -   Real   -   0."
For retrieval only. The X
position at the center of the
last character written by
PCHIQU, in the fractional system.
.IP "\'XE\'   -   Real   -   0."
For retrieval only. The X
position at the end of the last
string written by PCHIQU, in the
fractional system.
.IP "\'YB\'   -   Real   -   0."
For retrieval only. The Y
position at the beginning of the
last string written by PCHIQU,
in the fractional system.
.IP "\'YC\'   -   Real   -   0."
For retrieval only. The Y
position at the center of the
last character written by
PCHIQU, in the fractional system.
.IP "\'YE\'   -   Real   -   0."
For retrieval only. The Y
position at the end of the last
string written by PCHIQU, in the
fractional system.
.RE
.sp
.IP "Internal Parameters of PCMEQU" 
.sp
The internal parameter \'HW\' specifies the desired ratio of
the character height (the height of a capital) to the
character width (excluding white space). The default value
of \'HW\' is 1.75, reflecting the fact that the capitals are
digitized to be 7 units high and 4 units wide and 1.75 is
therefore their natural aspect ratio.  Negative values of
\'HW\' may be used. The absolute value will be used as the
ratio, but, in addition, PCHIQU will be prohibited from
changing \'HW\' (which it otherwise does when the quality
flag \'QU\' is set to 1).
.sp
PCMEQU reacts properly to nonzero values of \'MA\' and \'OR\',
which are used to request mapping of characters through the
routine PCMPXY, as described for PCHIQU, above. Characters
that are partly visible and partly invisible are clipped at
the visible/invisible boundary.
.sp
.IP "Internal Parameters of PCLOQU" 
.sp
PCLOQU reacts to nonzero values of \'MA\' and \'OR\', which
are used to request mapping of characters through the
routine PCMPXY, as described for PCHIQU, above. However, it
doesn\'t react in quite the same way: Mapping will affect
the position of the string written by a call to PCLOQU, the
angle at which it is written, and the size of the
characters used. The shape of the characters will not be
affected (and cannot be, since the characters are drawn by
calling the GKS routine GTX instead of GPL). If the point
(XPOS,YPOS) is visible under the mapping, the whole string
is considered to be visible; otherwise, the whole string is
considered to be invisible. The results can be pretty
crude; for that reason, mapping is not recommended.
.sp
.IP "A List of Font Names for Use in Calls to PCSETC That Set \'FN\'"
.sp
.nf
      0    'PWRITX DATABASE   '
      1    'DEFAULT           '
      2    'CARTOGRAPHIC_ROMAN'
      3    'CARTOGRAPHIC_GREEK'
      4    'SIMPLEX_ROMAN     '
      5    'SIMPLEX_GREEK     '
      6    'SIMPLEX_SCRIPT    '
      7    'COMPLEX_ROMAN     '
      8    'COMPLEX_GREEK     '
      9    'COMPLEX_SCRIPT    '
     10    'COMPLEX_ITALIC    '
     11    'COMPLEX_CYRILLIC  '
     12    'DUPLEX_ROMAN      '
     13    'TRIPLEX_ROMAN     '
     14    'TRIPLEX_ITALIC    '
     15    'GOTHIC_GERMAN     '
     16    'GOTHIC_ENGLISH    '
     17    'GOTHIC_ITALIAN    '
     18    'MATH_SYMBOLS      '
     19    'SYMBOL_SET1       '
     20    'SYMBOL_SET2       '
     21    'HELVETICA         '
     22    'HELVETICA-BOLD    '
     25    'TIMES-ROMAN       '
     26    'TIMES-BOLD        '
     29    'COURIER           '
     30    'COURIER-BOLD      '
     33    'GREEK             '
     34    'MATH-SYMBOLS      '
     35    'TEXT-SYMBOLS      '
     36    'WEATHER1          '
     37    'WEATHER2          '
    121    'O_HELVETICA       '
    122    'O_HELVETICA-BOLD  '
    125    'O_TIMES-ROMAN     '
    126    'O_TIMES-BOLD      '
    129    'O_COURIER         '
    130    'O_COURIER-BOLD    '
    133    'O_GREEK           '
    134    'O_MATH-SYMBOLS    '
    135    'O_TEXT-SYMBOLS    '
    136    'O_WEATHER1        '
    137    'O_WEATHER2        '
.fi
.SH SEE ALSO
Online:
pcdlsc,
pcgetc,
pcgeti,
pcgetr,
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
