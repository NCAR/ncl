.TH FTITLE 3NCARG "July 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
FTITLE - 
Used for unscrolled movie or video titles. It
reads, from standard input, the information necessary to
define the desired title frames, and then calls STITLE to
create those frames.
.SH SYNOPSIS
CALL FTITLE (MTST)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ftitle (int mtst)
.SH DESCRIPTION 
.IP MTST 12
(an input expression of type INTEGER) is a switch
indicating whether this is a "real" run or a "practice" run:
.RS
.IP 0
means "real run".
.IP 1
means "practice run".
.RE
.IP ""
During real runs, each title frame is repeated as many
times as necessary to display it for a user-specified time
period (at \'NFS\' frames per second). Blank frames are placed
before the first title frame (\'TM1\' seconds worth of them), between
consecutive title frames (\'TM2\' seconds worth of them), and after
the last title frame (\'TM2\' + \'TM3\' seconds worth of them).
Blank frames are to allow for splicing.
.sp
If the user has turned on fade-in (by setting
the internal parameter \'FIN\' non-zero), and/or fade-out (by
setting the internal parameter \'FOU\' non-zero), the
required fade-in and fade-out frames will be generated, as
well.
.sp
During practice runs, each title frame will occur from one
to three times (once for fade-in, once for the stationary
holding time, and once for fade-out); each of these frames
will have a legend indicating how many seconds the frame
represents in a real run. Each sequence of blank frames
will be replaced by a single frame with a message
indicating how many seconds worth of blank frames it
represents.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
FTITLE reads data from the unit specified by the internal parameter \'ICU\',
which has the default value 5, to specify standard input; this parameter
can be given a different value to specify that another unit should be read
instead.
.sp
The input data are read in groups.  Each group represents one title frame.
There can be any number of groups.  FTITLE keeps processing groups until
a group with NCDS = 0 is read or an end-of-file is encountered.  A group
consists of the following:
.IP \(bu 4
A header line from which variables NCDS, TIME, and SIZE are read, using the
FORTRAN format "(I5,2F5.1)".
.IP " " 4
NCDS is the number of text lines that follow.  If NCDS = 0, FTITLE quits
(returns to the calling routine without doing anything else).
.IP " " 4
TIME is the time, in seconds, that the title frame should be displayed (not
including fade-in and fade-out, if any).
.IP " " 4
SIZE is the desired character size, given as a multiplier of the default
height specified by the value of the internal parameter \'PSZ\', the
default value of which is 21 (out of 1023).  Values of SIZE from .75 to
2.5 are recommended.
.IP \(bu 4
Text lines, each containing one line of the title frame.  PLOTCHAR function
codes may be used as specified in the documentation for that package.
Characters should not appear beyond column 80.
.PP
The internal parameters 'ALN', 'BGB', 'BGC', 'BGF', 'BGG', 'BGR', 'FGB', 'FGC',
\&'FGF', 'FGG', 'FGR', 'FIN', 'FOU', 'GSZ', 'ICO', 'ICU', 'LOG', 'LX1', 'LX2',
\&'LY1', 'LY2', 'MAP', 'NFS', 'NXE', 'NXS', 'ORV', 'PSZ', 'SBK', 'SFG', 'TM1',
\&'TM2', 'TM3', 'VPB', 'VPL', 'VPR', 'VPT', and 'WID'
all affect the behavior of FTITLE in one way or another.  Some of these have
been mentioned above; all are described in the "man" page for
"scrolled_title_params".
.sp
Example: Suppose the input file contains the following three lines
.sp
.nf
    3   3.  1.5
A
Frame
of Titles
.fi
.sp
The resulting title frame has three lines of text.  It is displayed for
three seconds.  Characters have a size of 1.5 times the default character
size.
.sp
FTITLE allows a maximum of 80 characters per line of text including
Plotchar function codes.  No more than 120 lines of text can be
displayed on a single frame.  Titles are centered horizontally
unless you have changed the value of the internal parameter 'ICO'.
Vertical spacing is automatically determined using the current value of
the internal parameter 'GSZ'.
.sp
For more detailed control of titles, use the routine STITLE, which can be
used to generate either fixed or scrolled titles.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:
slex02.
.SH ACCESS
To use FTITLE or c_ftitle, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_c, preferably in that order.  
.SH MESSAGES
See the scrolled_title man page for a description of all Scrolled_title error
messages and/or informational messages.
.SH SEE ALSO
Online:
plotchar,
scrolled_title,
scrolled_title_params,
slgeti,
slgetr,
slogap,
slrset,
slseti,
slsetr,
stitle,
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
