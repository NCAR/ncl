.TH STITLE 3NCARG "July 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STITLE - 
Creates scrolled movie or video titles. It receives all
input through the argument list.
.SH UTILITY
This routine is part of the Scrolled_title utility in NCAR Graphics.  To
see the overview man page for this utility, type "man scrolled_title".
.SH SYNOPSIS
 CALL STITLE (CRDS,NCDS,IYST,IYND,TMST,TMMV,TMND,MTST)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_stitle (char *crds[], int ncds, int iyst,
.br
int iynd, float tmst, float tmmv, float tmnd, int mtst)
.SH DESCRIPTION
.IP CRDS 12
(an input array, dimensioned NCDS, of type
CHARACTER*n, where "n" is greater than or equal to 21) is
the "card input buffer". This array must be filled, prior
to calling STITLE, either by internal manipulations or by
reading n-character "cards". Each element of the array
CRDS represents one line on the scroll (or, sometimes, a
continuation of a previous line) and contains the following:
.RS
.IP \(bu
Columns 1-5: MX, the X coordinate of the line of text on
the scroll. This is normally a value between 1 and 1024,
inclusive. Exactly how the line of text is positioned
relative to the specified X coordinate depends on the value
of ICNT (in columns 14-15).
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
Columns 11-13: ICLR, the index of the color to be used for the line
of text.  If this field is blank, the default foreground color specified
by the value of the internal parameter \'FGC\' will be used.
.IP \(bu
Columns 14-15: ICNT, the centering option:
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
.IP NCDS 12
(an input expression of type INTEGER) is the
dimension of the array CRDS (i.e., the number of card
images in it).
.IP IYST 12
(an input expression of type INTEGER) is the Y
coordinate that will be at the center of the screen when
the text is first displayed.
.IP IYND 12
(an input expression of type INTEGER) is the Y
coordinate that will be at the center of the screen when
the text is last displayed.
.IP TMST 12
(an input expression of type REAL) is the time in
seconds that the scroll will be stationary at IYST. One
second is recommended.
.IP TMMV 12
(an input expression of type REAL) is the time to move
the scroll from IYST to IYND. This should be the time
required to read the text aloud at slow to normal speed.
.IP TMND 12
(an input expression of type REAL) is the time that
the scroll will be stationary at IYND. One second is
recommended.
.IP MTST 12
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
at the start (if TMST is non-zero), the scrolling time (if
TMMV is non-zero), the stationary sequence at the end (if
TMND is non-zero), and the fade-out sequence (if the user
has turned on fade-out by setting the internal parameter \'FOU\' non-zero).
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
internal parameters \'BGF\', which selects the type of fade-in/fade-out
to be used for the background color, and \'FGF\', which serves the same
function for the foreground color.  (The older parameters \'SBK\'
and \'SFG\' may still be referenced, but their use is no longer
recommended; setting \'SBK\' has the effect of giving \'BGF\' an
appropriate value and setting \'SFG\' has the effect of giving \'FGF\'
an apprpriate value.)
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
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
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
fslfont,
slex01,
slex02,
tstitl.
.SH ACCESS
To use STITLE or c_stitle, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.  
.SH MESSAGES
See the scrolled_title man page for a description of all Scrolled_title error
messages and/or informational messages.
.SH SEE ALSO
Online:
ftitle,
scrolled_title,
scrolled_title_params,
slgeti,
slgetr,
slogap,
slrset,
slseti,
slsetr,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
