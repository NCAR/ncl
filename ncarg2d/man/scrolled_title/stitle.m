.\"
.\"	$Id: stitle.m,v 1.1 1993-03-11 16:30:19 haley Exp $
.\"
.TH STITLE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
STITLE - produces scrolled movie titles.
.SH SYNOPSIS
CALL STITLE (CARDS, NCARDS, NYST, NYFIN, TST, TMV, TFIN, MV)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_stitle (char *cards[], int ncards, int nyst, int nyfin, float tst, float tmv, float tfin, int mv)
.SH DESCRIPTION
.IP CARDS(80,NCARDS) 12
A CHARACTER*80 array dimensioned for NCARDS.
This array must be filled prior to calling STITLE.
.sp
Each element of the CARDS array specifies text, sizing,
and positioning information for the text. STITLE uses a
local coordinate system for positioning its text. The
coordinates for positioning text in the horizontal, or
X, direction range from 1 to 1024 with coordinate 1
corresponding to the left edge and coordinate 1024
corresponding to the right edge.  The coordinates in
the vertical, or Y, direction range from 1 at the
bottom to any positive integer at the top. In this
coordinate system the default characters are
approximately 21 units high and 16 units wide.
.sp
Columns	Description
.sp
1-5     An integer, called MX for this discussion,
which denotes the X coordinate of this line of
text on the scroll, or an indicator that this
line of text is a continuation of the previous
line. MX is the coordinate of the middle of the
line if ICNTR is 1, and the coordinate of the
left edge of the first character if ICNTR is 0.
(See columns 11-15 for ICNTR.)
.sp
MX = -9999 is the continuation card indicator.
Any number of continuation cards may be used.
Trailing blanks are omitted from each line,
including those followed by a continuation
card. MX should be right justified in columns
1-5 with blank fill to the left.
.sp
6-10    An integer, called MY for this discussion,
which denotes the Y coordinate of this line of
text on the scroll. In the case of a continue
line, columns 6-20 are ignored.
.sp
11-15   The value of ICNTR, which is the centering option.
.sp
= 0 to start the text at MX.
= 1 to center the text about MX.
= 2 to end the text at MX.
.sp
16-20   The relative size of characters.  This
multiplies the PLOTCHAR character height.  The
recommended range is 1. to 2.5 (you can also
use PLOTCHAR function codes to change sizes).
.sp
21-80   The text for this line, or the continuation of
a line when X = -9999.  These columns must be
legal PLOTCHAR character strings.
.sp
.IP NCARDS 12
The second dimension of CARDS (the number of lines in CARDS).
.IP NYST 12
The STITLE coordinate that will be at the center of the
screen when the text is first displayed.
.IP NYFIN 12
The coordinate that will be at the center of the screen
when text is last displayed.
.IP TST 12
The time, in seconds, that the scroll will be
stationary at NYST. One second is recommended.
.IP TMV 12
The time, in seconds, to move the scroll from NYST to
NYFIN. This should be the time required to read the
text aloud at slow to normal speed.
.IP TFIN  12
The time, in seconds, that the scroll will be
stationary at NYFIN. One second is recommended.
.IP MV 12
A switch to indicate whether this is a movie production
run or a practice run. Set this variable to 0 (zero) if
a movie is in production; set it to 1 if you are making
a practice run.
.sp
A practice run will display a legend indicating the
number of seconds the frame will be shown at the start
or finish or the number of seconds into the total
moving time that a particular frame represents. A
representative outline of frames from the scroll is
output during a practice run. When the movie is being
made, this practice output is suppressed.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH USAGE
STITLE takes input through its argument list and generates
graphic output that moves a body of text up through the viewing
window. This is done by outputting the appropriate number of
frames required to generate a movie sequence of a duration
specified by you.
.sp
At each frame STITLE skips plotting lines of text that are
completely outside of the viewing window and clips those that
are partially outside the window.
.SH ACCESS
To use STITLE load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_stitle load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online: 
ftitle movies slgeti slgetr slseti slsetr stitle ncarg_cbind
.sp
Hardcopy:
"NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

