.\"
.\"	$Id: ftitle.m,v 1.1 1993-03-11 16:29:59 haley Exp $
.\"
.TH FTITLE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
FTITLE - creates fixed movie titles.
.SH SYNOPSIS
CALL FTITLE (MOVIE)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ftitle (int movie)
.SH DESCRIPTION 
.IP MOVIE 12
An integer used to indicate whether this is a practice
.sp
0	makes a movie 
1   for a practice run. When MOVIE is set to 1, you
will see an outlined, representative frame with
a legend indicating how many seconds the frame
will be shown. The number of blank frames that
will be output before and after the title
sequence is also shown while practicing. The
legend is suppressed when MOVIE is set to
zero.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the Fortran 
argument description.
.SH USAGE
FTITLE reads groups of data from standard input and generates
enough frames of titles to provide adequate reading time. There
can be any number of groups; FTITLE keeps processing until it
reads an NCARD = 0. A group consists of the following:
.sp
* A header line defining NCARD, TIME, and SIZE in the format
(I5,2F5.1).  NCARD An integer value specifying the number of
text lines that follow. If NCARD = 0, FTITLE will return to the
calling routine.
.sp
.IP TIME 12
A floating value specifying the time in seconds this
frame should be displayed.
.IP SIZE 12
A floating value specifying the relative size of
characters; multiplies the PLOTCHAR character height.
.sp
* Text lines, each containing one line of the movie title.
Since PLOTCHAR is used to produce the text for FTITLE, any text
string that is legal as an input character string in the
PLOTCHAR package can be used as a text line for input in
FTITLE. (See the documentation for the PLOTCHAR utility in
Chapter 5 of this manual.) Here is an example input file for
FTITLE. This file contains two groups, hence two different
titles would be produced. The first title would run for three
seconds, and the second title for five seconds. The size of the
characters used in the first title will be 1.5 times the
default character size; the size of the characters used in the
second title will be 1.2 times the default character size.
.sp
3   3.   1.5
A
Frame
of Titles
6   5.   1.2
Produced by
The National Center for
Atmospheric Research,
Boulder, Colorado 80307
Under sponsorship of the
National Science Foundation
.sp
FTITLE allows a maximum of 60 characters per line of text,
including PLOTCHAR function codes, and no more than 120 lines
of text may be displayed on any given frame. Titles are
centered horizontally unless you reset the default value of the
internal parameter 'ICO', accessible by the SLSETI subroutine.
Vertical spacing is automatic unless you reset the default
value of the internal parameter 'GSZ', accessible by SLSETR.
.sp
In FTITLE production mode, blank frames are generated before
and after each group of title frames. The number of blank
frames is determined by the values set in the internal
parameters 'TM1' and 'TM2', accessed by the SLSETR subroutine.
'TM1' defaults to one second's worth of blank frames prior to
any title frames; 'TM2' defaults to half a second of blank
frames between sets of title frames and after the last set of
title frames.
.sp
.SH ACCESS
To use FTITLE load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_ftitle load 
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

