.TH FTITLE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
FTITLE - 
Used for unscrolled movie titles. It
reads, from standard input, the information necessary to
define the desired title frames, and then calls STITLE to
create those frames.
.SH SYNOPSIS
CALL FTITLE (MOVIE)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ftitle (int movie)
.SH DESCRIPTION 
.IP MOVIE 12
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
period (at 24 frames per second). Blank frames are placed
before the first title frame, between consecutive title
frames, and after the last title frame. (Blank frames are
to allow for splicing; the internal parameters \'TM1\' and
\'TM2\' specify how many blank frames are used in each
position.) If the user has turned on fade-in (by setting
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
.SH ACCESS
To use FTITLE, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, ncarg_c, and ncarg_loc, preferably in that order.  To use c_ftitle, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the scrolled_title man page for a description of all Scrolled_title error
messages and/or informational messages.
.SH SEE ALSO
Online:
scrolled_title,
slgeti,
slgetr,
slseti,
slsetr,
stitle,
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
