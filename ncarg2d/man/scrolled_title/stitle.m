.TH STITLE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STITLE - 
Creates scrolled movie titles. It receives all
input through the argument list.
.SH UTILITY
This routine is part of the Scrolled_title utility in NCAR Graphics.  To
see the overview man page for this utility, type "man scrolled_title".
.SH SYNOPSIS
 CALL STITLE (CARDS, NCARDS, NYST, NYFIN, TST, TMV, TFIN, 
.br
+ MOVIE)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_stitle (char *cards[], int ncards, int nyst, \\
.br
int nyfin, float tst, float tmv, float tfin, int movie)
.SH DESCRIPTION
.IP CARDS 12
(an input array, dimensioned NCARDS, of type
CHARACTER*n, where "n" is greater than or equal to 21) is
the "card input buffer". This array must be filled, prior
to calling STITLE, either by internal manipulations or by
reading n-character "cards". Each element of the array
CARDS represents one line on the scroll (or, sometimes, a
continuation of a previous line) and contains the following:
.RS
.IP \(bu
Columns 1-5: MX, the X coordinate of the line of text on
the scroll. This is normally a value between 1 and 1024,
inclusive. Exactly how the line of text is positioned
relative to the specified X coordinate depends on the value
of ICNTR (in columns 11-15).
.sp
If the value -9999 is used for MX, it indicates a
continuation line: characters from columns 21 through "n"
are just appended to the characters from the previous card
to form the line of text. Any number of continuation cards
may be used, but the total number of characters in a line
of text must not be greater than 512.
.sp
Trailing blanks are omitted from each card, including those
that are followed by a continuation card; thus, if there
are to be blanks between the characters from one card and
the characters from a continuation card, those blanks must
be placed in columns 21 and following of the continuation
card.
.sp
On a continuation card, columns 6-20 are ignored.
.IP \(bu
Columns 6-10: MY, the Y coordinate of the line of text on
the scroll. MY may range from -9999 to 99999.
.IP \(bu
Columns 11-15: ICNTR, the centering option:
.RS
.IP 0 
means "start the text at MX".
.IP 1 
means "center the text about MX".
.IP 2 
means "end the text at MX".
.RE
.IP \(bu
Columns 16-20: SIZE, the desired size of the characters to
be used in writing the line. SIZE is given as a multiplier
of a default height specified by the value of the internal
parameter \'PSZ\', the default value of which is 21 (out of
1024). Values of SIZE from .75 to 2.5 are recommended.
.IP \(bu
Columns 21-n: Text for this line (or for continuation of a
line when MX = -9999).
.RE
.IP NCARDS 12
(an input expression of type INTEGER) is the
dimension of the array CARDS (i.e., the number of card
images in it).
.IP NYST 12
(an input expression of type INTEGER) is the Y
coordinate that will be at the center of the screen when
the text is first displayed.
.IP NYFIN 12
(an input expression of type INTEGER) is the Y
coordinate that will be at the center of the screen when
the text is last displayed.
.IP TST 12
(an input expression of type REAL) is the time in
seconds that the scroll will be stationary at NYST. One
second is recommended.
.IP TMV 12
(an input expression of type REAL) is the time to move
the scroll from NYST to NYFIN. This should be the time
required to read the text aloud at slow to normal speed.
.IP TFIN 12
(an input expression of type REAL) is the time that
the scroll will be stationary at NYFIN. One second is
recommended.
.IP MOVIE 12
(an input expression of type INTEGER) is a switch to
indicate whether this is a "real" run or a "practice" run.
.RS
.IP 0
means "real run".
.IP 1 
means "practice run".
.RE
.IP ""
During real runs, frames are created for the fade-in
sequence (if the user has turned on fade-in by setting the
internal parameter \'FIN\' non-zero), the stationary sequence
at the start (if TST is non-zero), the scrolling time (if
TMV is non-zero), the stationary sequence at the end (if
TFIN is non-zero), and the fade-out sequence (if the user
has turned on fade-out by setting the internal parameter
\'FOU\' non-zero).
.sp
During practice runs, only selected frames are created: a
frame for the fade-in sequence (if fade-in is turned on), a
frame for the stationary time at the start, a set of frames
representing the scrolling sequence, a frame for the
stationary time at the end, and a frame for the fade-out
sequence (if fade-out is turned on). Each has a legend
indicating either for how many seconds the frame will be
shown or, if it is part of a scroll sequence, how many
seconds into the scroll time it occurs; during real runs,
these legends are omitted, of course.
.sp
Fade-in and fade-out are also affected by the values of the
internal parameters \'SBK\', which can be set in such a way
as to allow or to suppress fade-in and fade-out of the
background color, and \'SFG\', which serves the same function
for the foreground color.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE@@@
STITLE takes input through its argument list and generates
graphic output that moves a body of text up through the viewing
window. This is done by outputting the appropriate number of
frames required to generate a movie sequence of a duration
specified by you.
.sp
At each frame STITLE skips plotting lines of text that are
completely outside of the viewing window and clips those that
are partially outside the window.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
stex01,
tstitl.
.SH ACCESS
To use STITLE, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, ncarg_c, and ncarg_loc, preferably in that order.  To use c_stitle, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the scrolled_title man page for a description of all Scrolled_title error
messages and/or informational messages.
.SH SEE ALSO
Online:
ftitle,
scrolled_title,
slgeti,
slgetr,
slseti,
slsetr,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
