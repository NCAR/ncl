.\"
.\"	$Id: stitle.m,v 1.1.1.1 1992-04-17 22:30:44 ncargd Exp $
.\"
.TH STITLE 3NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE3
.dsNA " STITLE - Plot stationary or scrolling titles
.dsS1 " CALL STITLE (CARDS,NCARDS,NYST,NYFIN,TST,TMV,TFIN,MV) ~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Produces movie titles that may be scrolled and/or ~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~ faded in and out
.dsS2 " CALL FTITLE (MOVIE) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Produces titling frames with the text in each frame ~~~~~~~~~~~~~~~~~~~~~~~~ ~~ being in the same position
.dsS3 " CALL SLSETI (PA,IVAL) Sets integer values
.dsS4 " CALL SLSETR (PA,RVAL) Sets real values
.dsS5 " CALL SLGETI (PA,IVAL) Retrieves current integer values
.dsS6 " CALL SLGETR (PA,RVAL) Retrieves current real values
.nrsN 6
.ds f. man/stitle.l
./" revised 9/27/89 w/new headers, footers, L1 heads, *i, *X  -Laurie Pentell
./" revised 6/13/89 to include tab macro .tA
./" revised 6/27/89 to include correct headers & footers
./" revised 6/29/89 to include macro for right-adjusted headings (cg)
.de *X		.\ Make an Index Entry
.if \\nJ .tm .IE\tENTRY\t\\$1\t\\$2\t\\$3\t\\$4\t\\$5\t\\$6\t\\n%
..
.tr ~
.PH ""
.PF ""
.pn 24
.EF "@\s126-%  \s9Movies\fR@@NCAR Graphics Guide to New Utilities@"
.OF "@\s9Version 3.00, October 1989@@\s9Movies~~\s126-%\s9@"
./"  Headers & footers: replace chapter name, utility, and digit before the %
./"  Put odd header command after first page of text so it won't print on pg1;
./"  Odd header:  .OH "@@@\s9TITLE@"
.ll 6.5i
.EQ         \"  Makes all equations be 12 point, Roman font
.S 99
.EN
.de tA
.sp
.ta 2i-1n   \"  tab setup for Type:  Default: line
Type:  \\$1\tDefault:  \\$2
.sp
..
.de >>       \"  display for indented lines
.sp
.nf
..           \"  subsequent lines are indented
.de <<       \"  end of display for indented lines
.fi
.sp
..           \"  subsequent lines are not indented
.de sF       \"  begin FORTRAN (constant spacing font)
.ps 10
.vs 12
.nf
.ft L
..
.de eF       \"  end FORTRAN (resume previous font & prev. size & font)
.ft
.fi
.ps
.vs
..
\&
.ft B
.ps 16
.*i "STITLE"
.ft R
.sp 3.5
.L1 "STITLE Introduction"
The STITLE utility lets you produce movie titles  
with a minimum of effort.  Titles may scroll or remain
in a fixed position; they may also fade in and out.   
.sp
This utility contains six user-callable subroutines
that fall into three categories:  two that create titles (FTITLE, STITLE), 
two that set internal parameter values (SLSETI, SLSETR), 
and two that retrieve internal parameter values (SLGETI,
SLGETR).
.L2 "If You Have Been Using a Pre-GKS Version of NCAR Graphics"
The STITLE utility replaces the 
SCROLL utility of the pre-GKS version of the NCAR Graphics package.  User 
entries SLSETI and SLSETR have been added, as have fade in
and fade out options.          
.L2 "Subroutines for Creating Movie Titles" 
FTITLE creates fixed titles and should be 
called when no scrolling is desired.  
Call STITLE when a scrolled title is what you want. 
.L2 "Subroutines for Setting Internal Parameter Values"
The subroutines SLSETI and SLSETR are available for setting
internal parameters.
You are able to reassign values for
attributes such as the 
size, position, and spacing of characters;
foreground and background colors; 
and the amount of time you want to use to fade in,
display (and scroll, if desired), 
and fade out the titles.
.L2 "Subroutines for Retrieving Internal Parameter Values"
The subroutines SLGETI and SLGETR retrieve the current
values of STITLE's internal parameters.  This capability is
furnished for consistency with other utilities in the NCAR
Graphics package and also for programmers who wish to build
applications around STITLE.  
.OH "@@@\s9STITLE" ./"  Headers & footers: replace chapter name, 
.\"                      utility, and digit before the % .ll 6.5i
.EH "@\s9STITLE@@@"
.L1 "STITLE Calls"  
.ne 6
.in 0
.ft B 
.S 14 
FTITLE
.S 12
.L2 Purpose
FTITLE creates fixed movie titles.  
.sp 
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL FTITLE\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~(MOVIE)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput 
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 108 file man/stitle.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br 
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL FTITLE\fR\s12\h'|\n(41u'~~~~(MOVIE)\h'|\n(42u'Integer\h'|\n(43u'Input \h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.VL 1.2i
.LI "\fBMOVIE\fR"
An integer used to indicate whether this is a practice run
or a production run.  Set to 0 (zero) to make a 
movie; set to 1 for a practice run. When MOVIE is set to 1,
you will see an
outlined, representative frame with a legend indicating how many
seconds the frame will be shown.
The number of blank frames that will be output before and
after the title sequence is also shown while
practicing.  The legend is suppressed when 
MOVIE is set to zero. 
.LE
.L2 "Usage"
FTITLE reads groups of data from standard input and generates
enough frames of titles to provide adequate reading
.hw FTITLE 
time.  There can be any number of groups; FTITLE keeps
processing until it reads an NCARD = 0.  A group consists of
the following:
.BL2
.LI
A header line defining NCARD, TIME, and SIZE in the format 
(I5,2F5.1).
.LE
.in .5i  
.VL 1.2i
.LI "\fBNCARD\fR"
An integer value specifying the number of text lines that follow.  If NCARD = 0, FTITLE will return to the calling routine.
.LI "\fBTIME\fR"
A floating value specifying the time in seconds this frame should be displayed.
.LI "\fBSIZE\fR"
A floating value specifying the relative size of characters; multiplies the PLOTCHAR character height.
.LE
.BL2
.LI
Text lines,  
each containing one line of the movie title.
Since PLOTCHAR is used to produce the text for FTITLE, any
text string that is legal as an input character string in
the PLOTCHAR package can be used as a text line for input in
FTITLE.  (See the documentation for the PLOTCHAR utility in
Chapter 5 of this manual.)
.LE  
Here is an example input file for FTITLE.  This file contains two groups, 
hence two different titles would be produced.  The first title would run for 
three seconds, and the second title for five seconds.  
The size of the characters
used in the first title will be 1.5 times
the default character size;   
the size of the characters used in the second title will be 1.2 
times the default character size.
.sp
.sF
    3   3.  1.5
A
Frame
of Titles
    6   5.  1.2
Produced by
The National Center for
Atmospheric Research,
Boulder, Colorado 80307
Under sponsorship of the 
National Science Foundation
.eF
.sp
FTITLE allows a maximum of 60 characters per line of text,
including PLOTCHAR function codes, and no more than 120 lines of
text may be displayed on any given frame.  Titles are
centered horizontally unless you reset the default value 
of the internal parameter \&'ICO', accessible by the SLSETI
subroutine.  Vertical spacing is automatic
unless you reset the default value of the internal parameter \&'GSZ', 
accessible by SLSETR.
.sp
In FTITLE production mode, blank frames are generated before and
after each group of title frames.  The number of blank
frames is determined by the values
set in the internal parameters \&'TM1' and \&'TM2', accessed by the
SLSETR subroutine.  \&'TM1' defaults to one second's worth of blank
frames prior to any title frames; \&'TM2' defaults to half a
second of blank frames between sets of title frames and
after the last set of title frames.
.ne 6
.in 0
.ft B
.S 14
\l'.9i_'
.S 12
.L2 Purpose
STITLE produces scrolled movie titles. 
.sp 
.in -.5i 
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL STITLE\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~(CARDS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~NCARDS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~NYST,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~NYFIN,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~TST,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~TMV,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~TFIN,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~MV)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNCARDS
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 217 file man/stitle.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~~~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL STITLE\fR\s12\h'|\n(41u'~~~~~(CARDS,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'NCARDS
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~NCARDS,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~NYST,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~NYFIN,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~TST,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~TMV,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~TFIN,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~MV)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE  
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.in +.5i
.VL 1.2i
.sp 
.LI "\fBCARDS\fR" 
A CHARACTER*80 array dimensioned for NCARDS.
This array must be filled prior to calling
STITLE. 
.sp
Each element of the CARDS array specifies text, sizing, and
positioning information for the text.  STITLE uses a local
coordinate system for positioning its text.  The coordinates
for positioning text in the horizontal, or X, direction
range from 1 to 1024 with coordinate 1 corresponding to the
left edge and coordinate 1024 corresponding to the right
edge.  The coordinates in the vertical, or Y, direction
range from 1 at the bottom to any positive integer at the
top.  In this coordinate system the default characters are
approximately 21 units high and 16 units wide.
.sp
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
An integer, called MX for this discussion,
which denotes the X coordinate of this line of text on the scroll, 
or an indicator that this
line of text is a continuation of the
previous line.  MX is the coordinate of
the middle of the line if ICNTR is 1, and
the coordinate of the left edge of the
first character if ICNTR is 0. (See
columns 11-15 for ICNTR.)
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
MX = -9999 is the continuation card
indicator.  Any number of continuation
cards may be used.  Trailing blanks are
omitted from each line, including those
followed by a continuation card.  MX should be 
right-justified in columns 1-5 with blank fill to the left.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 81
.br
.di c+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
An integer, called MY for this discussion, which denotes the
Y coordinate of this line of text on
the scroll.  In the case of a
continue line, columns 6-20 are ignored.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 81
.br
.di d+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The value of ICNTR, which is the centering option.
.br
\&= 0  to start the text at MX.
.br
\&= 1  to center the text about MX.
.br
\&= 2  to end the text at MX.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.eo
.am 81
.br
.di e+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The relative size of characters.
This multiplies the PLOTCHAR character
height.  The recommended range is 1. to 2.5
(you can also use PLOTCHAR function codes to
change sizes).
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.eo
.am 81
.br
.di f+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The text for this line, or the continuation
of a line when X = -9999.
These columns must be legal PLOTCHAR character strings.
.br
.di
.nr f| \n(dn
.nr f- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBColumns
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-5
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w6-10
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w11-15
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w16-20
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w21-80
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(d-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(e-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(f-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 292 file man/stitle.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBColumns\h'|\n(41u'Description\fR
.sp  
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-5\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'6-10\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'11-15\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.d+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(e|u+\n(.Vu
.if (\n(e|+\n(#^-1v)>\n(#- .nr #- +(\n(e|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'16-20\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.e+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(f|u+\n(.Vu
.if (\n(f|+\n(#^-1v)>\n(#- .nr #- +(\n(f|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'21-80\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.f+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.rm f+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-55
.LE
.VL 1.2i 
.sp 
.LI "\fBNCARDS\fR"
The second dimension of CARDS (the number
of lines in CARDS).
.LI "\fBNYST\fR"
The STITLE coordinate that will be at the
center of the screen when the text is first
displayed.
.LI "\fBNYFIN\fR"
The coordinate that will be at the center of the screen
when text is last displayed.
.LI "\fBTST\fR"
The time, in seconds, that the scroll will be
stationary at NYST.  One second is
recommended.
.LI "\fBTMV\fR" 
The time, in seconds, to move the scroll from NYST to NYFIN.
This should be the time required to read the
text aloud at slow to normal speed.
.LI "\fBTFIN\fR"
The time, in seconds, that the scroll will be stationary at
NYFIN.  One second is recommended.
.LI"\fBMV\fR"
A switch to indicate whether this is a movie production run
or a practice run.  Set this variable to 0 (zero) if a movie 
is in production; set it to 1 if you are making a practice
run.
.sp
.ne 5
A practice run will display a legend indicating the number of seconds
the frame will be shown at the start or finish or the number of
seconds into the total moving time that a particular frame represents.
A representative outline of frames from the scroll is output during a
practice run.  When the movie is being made, this practice output is
suppressed.
.LE
.L2"Usage"
STITLE takes input through its argument list and generates
graphic output that moves a body of text up  
through the viewing window.  This is done by outputting the
appropriate number of frames
required to generate a movie sequence of
a duration specified by you. 
.sp
At each frame STITLE skips plotting lines of text that are completely
outside of the viewing window and clips those that are partially
outside the window.
.L1 "STITLE Parameter Access Subroutines" 
The STITLE utility includes 26 internal parameters that you
can access to further your control over movie titling
capabilities.  Four subroutines will access these
parameters: two that set values, and 
two that retrieve
values.  These subroutines are described here, after
which the internal parameters
accessed by these subroutines are discussed. 
.L2 "Subroutines for Setting Internal Parameter Values"
Use the following subroutines to reset the current values
of internal parameters.  Changes remain in effect until you
change them with another call to one of these subroutines.  
Only the first three characters of the parameter string are
used in determining legal values.
.sp  
The subroutine SLSETI sets integer-valued parameters; the 
subroutine SLSETR sets real-valued parameters.  
.sp
.in -.5i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL SLSETI\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~~(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~~~IVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 369 file man/stitle.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~~~~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SLSETI\fR\s12\h'|\n(41u'~~~~~~(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~~~IVAL)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR" 
The internal parameter name of type character (for example, \&'ALN').
.LI "\fBIVAL\fR"
The integer value you set for the internal parameter.
.LE
.sp
.in -1.25i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL SLSETR\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~~RVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 386 file man/stitle.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~~~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br 
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SLSETR\fR\s12\h'|\n(41u'~~~~~(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~~RVAL)\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name of type character (for example, \&'FIN').
.LI "\fBRVAL\fR"
The real value you select for the parameter.
.LE
.L2 "Subroutines for Retrieving Current Internal Parameter Values"
Use the following subroutines to retrieve the current
values of internal parameters.  The subroutine
SLGETI gets integer-valued parameters; the
subroutine SLGETR gets real-valued parameters.
Only the first three characters of PNAM are used in
determining legal values.
.in -1i 
.sp
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL SLGETI\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~~IVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 410 file man/stitle.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~~~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SLGETI\fR\s12\h'|\n(41u'~~~~~(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~~IVAL)\h'|\n(42u'Integer\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i  
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name of type character (for example, \&'ICU').
.LI "\fBIVAL\fR"
The integer value you select for the parameter.
.LE
.sp
.in -1.25i
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83 84
.nr 80 0
.nr 38 \w\fB\s11CALL SLGETR\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~RVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 427 file man/stitle.l is too wide - \n(TW units
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SLGETR\fR\s12\h'|\n(41u'~~~~(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~RVAL)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i  
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name of type character (for example, \&'BGR').
.LI "\fBRVAL\fR"
The real value you select for the parameter.
.LE
.L1 "STITLE Internal Parameter Descriptions"
Internal parameters are variables with scopes
limited to the subroutines in the 
.br
STITLE utility.
They determine background and foreground color
components and the time employed to fade titles in
and out.  In the case of the FTITLE subroutine,
internal parameters also determine spacing,
character size, and time allotted to blank frames
generated before and after title frames.  
.sp
.VL 1.2i
.ft B
.LI \&'ALN'
.ft R
Flag to control whether alignment frames with
dots in the corners are put out in non-test mode.
Set to 1 to put the frames out; set to 0 to    
suppress the frames.
.tA "Integer" "0"
.LE
.ft R
.in .5i
.VL 1.2i
.ft B
.LI \&'BGR'  
.LI \&'BGG'
.LI \&'BGB'
.ft R
Real values to be specified in the range 0. to 1. and
used for the background color.  
You may set the value of the red
background component ('BGR'), 
the green background component ('BGG'),
and the blue background component ('BGB').
.tA "Real" "Device Default"
.LE
.in .5i
.VL 1.2i
.ft B
.LI \&'FGR'
.LI \&'FGG'
.LI \&'FGB'
.ft R
Real values to be specified in the range 0. to 1. and
used for the foreground color.  
The characters are drawn in the
foreground color. A call to the GKS subroutine GSCR can
alternatively be used to set the foreground color representation.
.sp 
You may set the value of the red foreground
component ('FGR'), the green foreground component ('FGG'), and 
the blue foreground component ('FGB').
.tA "Real" "Device Default"
.in .5i
.VL 1.2i
.ft B
.LI \&'FIN'
.ft R
Number of seconds to fade in the first title.  The
background color and foreground colors are faded in
independently.  Each color is faded in from black to its current color
by varying the value parameter in a Hue, Saturation, and
Value (HSV) representation of the color in a linear manner
over a time period you specify.  For further information on
color spaces and conversion routines between them, consult
the documentation for the COLCONV utility in the Color
chapter of this manual.
.tA "Real" "0."
.in .5i
.ne 6
.VL 1.2i
.ft B
.LI \&'FOU'
.ft R
Number of seconds to fade out the last title.  The
background color and foreground colors are faded out
independently.  Each color is faded out from its current
color to black by varying the value parameter in a Hue, Saturation, and
Value (HSV) representation of the color in a linear manner
over a time period you specify.  For further information on
color spaces and conversion routines between them, consult
the documentation for the COLCONV utility in the Color
chapter of this manual.
.tA "Real" "0."
.in .5i
.ft B
.LI \&'GSZ'
.ft R
Value for interline spacing.  Used only by FTITLE.
.tA "Real" "40."
.in .5i
.ft B
.LI \&'ICO'
.ft R
Centering option used only by FTITLE.   	
Set to 0 to get left edges lined up at X-coordinate 64,
set to 1 for centered text, 
and set to 2 to get right edges lined up at X-coordinate 960.
.tA "Integer" "1"
.in .5i
.ft B
.LI \&'ICU'
.ft R
Unit number for reading input.  Used only by
FTITLE.
.tA "Integer" "5 (Standard Input)"
.ft B 
.LI \&'INC'
.ft R
Vertical STITLE coordinate spacing
between practice frames.  
.br
Used only by STITLE.
.tA "Integer" "300"
.in .5i
.ft B    
.LI \&'LOG'
.ft R
Fortran logical unit number for opening the
Workstation Independent Segment Storage (WISS).    
The number chosen here should not conflict with any other 
logical unit numbers being used in the application program.    
Consult GKS documentation for details on WISS. 
.tA "Integer" "9"
.ne 13 
.in .5i
.ft B
.LI \&'LX1'
.LI \&'LX2'
.LI \&'LY1'
.LI \&'LY2'
.ft R
Four integer values specifying where the output is to be
positioned on the frame.  The values range from 0 to 32767,
and \&'LX2' must be greater than \&'LX1', and \&'LY2' must be greater
than \&'LY1'.  The default values position the output on the
largest possible square in the frame.  If \&'LX1'=0, \&'LX2'=16383, 
\&'LY1'=0, and \&'LY2'=16383, then the output would be placed on the
lower left quarter of the frame.  If \&'LX1'=16383, \&'LX2'=32767,
\&'LY1'=16383, and \&'LY2'=32767, then the output would be
positioned on the upper right quarter of the frame.  If the
background color has been set to a non-default value, then
that background color will fill only the part of the frame
specified by \&'LX1', \&'LX2', \&'LY1', and \&'LY2'.
.tA "Integer" "'LX1' and \&'LY1'=0"
.ti +2.6i
\&'LX2' and \&'LY2'=32767
.in .5i
.ne 6
.ft B
.LI \&'NXE'
.ft R
Analogous to argument NYFIN to allow for
limited scrolling in the X direction.
\&'NXE' must be within the current STITLE
window, and text must leave the window
through the top and not the sides.
Used only by STITLE.
.tA "Integer" "512"
.in .5i
.ft B
.LI \&'NXS'
.ft R
Analogous to argument NYST to allow for
limited scrolling in the X direction.
\&'NXS' must be within the current STITLE
window, and text must leave the window
through the top and not the sides.
Used only by STITLE.
.tA "Integer" "512"
.in .5i  
.ft B
.LI \&'PSZ'
.ft R
Value for character height.  Used only by FTITLE.
.tA "Real" "21."
.in .5i
.ft B
.LI \&'SBK'
.ft R
Suppress fade in or out of the background color
when a fade in or out time has been specified 
by \&'FIN' or \&'FOU'.  The
background color will appear at full intensity 
during a fade in/out.  If 0, then the fade
in/out will be honored; otherwise not.
.tA "Integer" "0"
.ne 6
.in .5i
.ne 6
.ft B
.LI \&'SFG'
.ft R
Suppress fade in or out of the foreground color
when a fade in or out time has been specified.  The
foreground color will appear at full intensity 
during a fade in/out.  If 0, then the fade
in/out will be honored; otherwise not.
.tA "Integer" "0"
.in .5i
.ne 4
.ft B
.LI \&'TM1'
.ft R
Number of seconds' worth of blank frames generated
before any title frames (at 24 frames per second).
Used only be FTITLE.
.tA "Real" "1." 
.in .5i
.ne 13
.ft B      
.LI \&'TM2' 
.ft R
Number of seconds' worth of blank frames
between sets of title frames, and after
.nh "FTITLE"
the last set of title frames.  Used only by FTITLE.
.tA "Real" "5."
.in .5i
.ft B
.LI \&'WID'
.ft R
Workstation identifier used internally in a GKS GOPWK call
to open Workstation Independent Segment
Storage (WISS).  This number should be chosen so that it
does not conflict with any other GKS workstation identifier
used by the application program.
.tA "Integer" "9"
.ce 1
.L2 "Functional Listing of Internal Parameters "
The following table lists all the internal parameters
available for the STITLE utility.  
The parameters are organized
according to
functionality so you may have a better understanding of 
their contextual relationship to one
another.
.sp
The internal parameters with integer values are listed below; those
with real values are on the next page.  For more detailed information
regarding these parameters, reference them in their alphabetical
placement earlier in this chapter.
.sp 2
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83
.nr 34 \n(.lu
.eo
.am 83
.br
.di a+
.35
.ft \n(.f
.ll 2.5in
.if \n(.l<\n(83 .ll \n(83u
.in 0
(FTITLE) Unit number for reading input 
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 83
.br
.di b+
.35
.ft \n(.f
.ll 2.5in
.if \n(.l<\n(83 .ll \n(83u
.in 0
(STITLE) Analogous to NYST to allow limited scrolling in X
direction
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 83
.br
.di c+
.35
.ft \n(.f
.ll 2.5in
.if \n(.l<\n(83 .ll \n(83u
.in 0
(STITLE) Analogous to NYFIN to allow limited scrolling in X
direction
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 83
.br
.di d+
.35
.ft \n(.f
.ll 2.5in
.if \n(.l<\n(83 .ll \n(83u
.in 0
(STITLE) Vertical coordinate spacing between practice frames
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.eo
.am 83
.br
.di e+
.35
.ft \n(.f
.ll 2.5in
.if \n(.l<\n(83 .ll \n(83u
.in 0
Allows/suppresses alignment frames in production mode
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.eo
.am 83
.br
.di f+
.35
.ft \n(.f
.ll 2.5in
.if \n(.l<\n(83 .ll \n(83u
.in 0
Fortran logical unit number for opening WISS
.br
.di
.nr f| \n(dn
.nr f- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBParameter~
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ICU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ICO'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NXS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NXE'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'INC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LX1'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LX2'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LY1'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LY2'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ALN'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'SBK'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'SFG'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'WID'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LOG'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 1in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wType
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 .75in
.if \n(81<\n(38 .nr 81 \n(38
.nr 82 0
.nr 38 \w~Default
.if \n(82<\n(38 .nr 82 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w512
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w512
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w300
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w32767
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w32767
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w9
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w9
.if \n(31<\n(38 .nr 31 \n(38
.82
.rm 82
.nr 62 \n(31
.nr 38 \n(62+\n(32
.if \n(38>\n(82 .nr 82 \n(38
.if \n(38<\n(82 .nr 62 +(\n(82-\n(38)/2
.nr 38 .75in
.if \n(82<\n(38 .nr 82 \n(38
.nr 83 0
.nr 38 \wBrief Description\fR
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w(FTITLE) Text centering option
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wLower-left X value of viewport
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wUpper-right X value of viewport
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wLower-left Y value of viewport 
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wUpper-right Y value of viewport
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wSuppress background fade in/out
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wSuppress foreground fade in/out
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wWISS identifier
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 38 \n(a-
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \n(b-
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \n(c-
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \n(d-
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \n(e-
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \n(f-
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 2.5in
.if \n(83<\n(38 .nr 83 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 62 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr TW \n(83
.if t .if \n(TW>\n(.li .tm Table at line 716 file man/stitle.l is too wide - \n(TW units
.nr #I \n(.i
.in +(\n(.lu-\n(TWu-\n(.iu)/2u
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.sp
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBParameter~\h'|\n(41u'Type\h'|\n(42u'~Default\h'|\n(43u'Brief Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ICU'\h'|\n(41u'Integer\h'|\n(42u'5\h'|\n(43u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(43u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ICO'\h'|\n(41u'Integer\h'|\n(42u'1\h'|\n(43u'(FTITLE) Text centering option
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NXS'\h'|\n(41u'Integer\h'|\n(42u'512\h'|\n(43u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(43u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NXE'\h'|\n(41u'Integer\h'|\n(42u'512\h'|\n(43u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(43u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'INC'\h'|\n(41u'Integer\h'|\n(42u'300\h'|\n(43u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(43u
.in +\n(37u
.d+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LX1'\h'|\n(41u'Integer\h'|\n(42u'0\h'|\n(43u'Lower-left X value of viewport
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LX2'\h'|\n(41u'Integer\h'|\n(42u'32767\h'|\n(43u'Upper-right X value of viewport
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LY1'\h'|\n(41u'Integer\h'|\n(42u'0\h'|\n(43u'Lower-left Y value of viewport 
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LY2'\h'|\n(41u'Integer\h'|\n(42u'32767\h'|\n(43u'Upper-right Y value of viewport
.ne \n(e|u+\n(.Vu
.if (\n(e|+\n(#^-1v)>\n(#- .nr #- +(\n(e|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ALN'\h'|\n(41u'Integer\h'|\n(42u'0\h'|\n(43u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(43u
.in +\n(37u
.e+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'SBK'\h'|\n(41u'Integer\h'|\n(42u'0\h'|\n(43u'Suppress background fade in/out
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'SFG'\h'|\n(41u'Integer\h'|\n(42u'0\h'|\n(43u'Suppress foreground fade in/out
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'WID'\h'|\n(41u'Integer\h'|\n(42u'9\h'|\n(43u'WISS identifier
.ne \n(f|u+\n(.Vu
.if (\n(f|+\n(#^-1v)>\n(#- .nr #- +(\n(f|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(62u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LOG'\h'|\n(41u'Integer\h'|\n(42u'9\h'|\n(43u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(43u
.in +\n(37u
.f+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp   
.fc
.nr T. 1
.T# 1
.in \n(#Iu
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.rm f+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-36
.in 0
.*i "continued on next page"
\s10~~~~(continued on next page)\s12
.in 0
.sp 2
.TS
.if \n+(b.=1 .nr d. \n(.c-\n(c.-1
.de 35
.ps \n(.s
.vs \n(.vu
.in \n(.iu
.if \n(.u .fi
.if \n(.j .ad
.if \n(.j=0 .na
..
.nf
.nr #~ 0
.if n .nr #~ 0.6n
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.fc
.nr 33 \n(.s
.rm 80 81 82 83
.nr 80 0
.nr 38 \w\fBParameter~
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'BGR'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'BGG'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'BGB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'FGR'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'FGG'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'FGB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'FIN'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'FOU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'GSZ'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PSZ'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'TM1'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'TM2'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 1in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wType
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 .75in
.if \n(81<\n(38 .nr 81 \n(38
.nr 82 0
.nr 38 \w~Default
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wDevice
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wDevice
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wDevice
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wDevice
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wDevice
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wDevice
.if \n(82<\n(38 .nr 82 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w40
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w21
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.82
.rm 82
.nr 62 \n(31
.nr 38 \n(62+\n(32
.if \n(38>\n(82 .nr 82 \n(38
.if \n(38<\n(82 .nr 62 +(\n(82-\n(38)/2
.nr 38 .75in
.if \n(82<\n(38 .nr 82 \n(38
.nr 83 0
.nr 38 \wBrief Description\fR
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wRed background component
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wGreen background component
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wBlue background component
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wRed foreground component
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wGreen foreground component
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wBlue foreground component
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wSeconds to fade in the first title
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wSeconds to fade out the last title
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w(FTITLE) Interline spacing
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w(FTITLE) Default character height
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w(FTITLE) Time for blank frames
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wbefore any title frames
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w(FTITLE) Time for blank frames
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wbetween frames and after last title
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wframe
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 38 2.5in
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 62 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr TW \n(83
.if t .if \n(TW>\n(.li .tm Table at line 745 file man/stitle.l is too wide - \n(TW units
.nr #I \n(.i
.in +(\n(.lu-\n(TWu-\n(.iu)/2u
.fc  
.nr #T 0-1
.nr #a 0-1
.eo
.de T#
.ds #d .d
.if \(ts\n(.z\(ts\(ts .ds #d nl
.mk ##
.nr ## -1v
.ls 1
.ls
..
.ec
.sp
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBParameter~\h'|\n(41u'Type\h'|\n(42u'~Default\h'|\n(43u'Brief Description\fR
.sp
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'BGR'\h'|\n(41u'Real\h'|\n(42u'Device\h'|\n(43u'Red background component
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'BGG'\h'|\n(41u'Real\h'|\n(42u'Device\h'|\n(43u'Green background component
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'BGB'\h'|\n(41u'Real\h'|\n(42u'Device\h'|\n(43u'Blue background component
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'FGR'\h'|\n(41u'Real\h'|\n(42u'Device\h'|\n(43u'Red foreground component
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'FGG'\h'|\n(41u'Real\h'|\n(42u'Device\h'|\n(43u'Green foreground component
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'FGB'\h'|\n(41u'Real\h'|\n(42u'Device\h'|\n(43u'Blue foreground component
.ta \n(80u \n(81u \n(62u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'FIN'\h'|\n(41u'Real\h'|\n(42u'0.\h'|\n(43u'Seconds to fade in the first title
.ta \n(80u \n(81u \n(62u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'FOU'\h'|\n(41u'Real\h'|\n(42u'0.\h'|\n(43u'Seconds to fade out the last title
.ta \n(80u \n(81u \n(62u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'GSZ'\h'|\n(41u'Real\h'|\n(42u'40.\h'|\n(43u'(FTITLE) Interline spacing
.ta \n(80u \n(81u \n(62u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PSZ'\h'|\n(41u'Real\h'|\n(42u'21.\h'|\n(43u'(FTITLE) Default character height
.ta \n(80u \n(81u \n(62u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'TM1'\h'|\n(41u'Real\h'|\n(42u'1.\h'|\n(43u'(FTITLE) Time for blank frames
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'\h'|\n(43u'before any title frames
.ta \n(80u \n(81u \n(62u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'TM2'\h'|\n(41u'Real\h'|\n(42u'5.\h'|\n(43u'(FTITLE) Time for blank frames
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'\h'|\n(43u'between frames and after last title
.ta \n(80u \n(81u \n(82u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'\h'|\n(43u'frame
.sp
.fc
.nr T. 1
.T# 1
.in \n(#Iu
.35
.TE  
.if \n-(b.=0 .nr c. \n(.c-\n(d.-23
./".*X "STITLE, utility" "internal parameters, descriptions" PAGE END
./".bp
./".*X "STITLE, utility" "examples, output" "" "" PAGE START
./".L1 "STITLE Examples \(em Output"
./".sp -2
./".L2 "Example 1-1"
./".sp 2
./".MF stitle.cgm 1 5.0
./".bp
./".L2 "Example 1-2"
./".sp 2
./".MF stitle.cgm 2 5.0
./".bp
./".L2 "Example 1-3"
./".sp 2
./".MF stitle.cgm 3 5.0
./".bp
./".L2 "Example 1-4"
./".sp 2
./".MF stitle.cgm 4 5.0
./".bp
./".L2 "Example 1-5"
./".sp 2
./".MF stitle.cgm 5 5.0
./".bp
./".L2 "Example 1-6"
./".sp 2
./".MF stitle.cgm 6 5.0
./".bp
./".L2 "Example 1-7"
./".sp 2
./".MF stitle.cgm 7 5.0
./".bp
./".L2 "Example 1-8"
./".sp 2  
./".MF stitle.cgm 8 5.0
./".bp
./".L2 "Example 2"
./".sp 2  
./".MF stitle.meta 1 5.0
./".SK 1
./".nf
./".*X "STITLE, utility" "examples, output" "" "" PAGE END
./".L1 "STITLE Examples \(em Code"
./".*X "STITLE, utility" "examples, code" "" "" PAGE START
./".sp -2  
.L2 "Example 1-1 through 1-8"
.in -.5i
.sF
      SUBROUTINE EXSTL0 (IERROR)
C
C  This subroutine provides a simple example of STITLE usage.
C  It is assumed that GKS has been opened prior to calling
C  this subroutine.
C
      CHARACTER*80    CARDS(12)
C
C  First, set up the CARDS array to specify the text strings, the 
C  text sizes, horizontal centering, and vertical coordinate
C  information.  This information is supplied for each line of text.
C  In this example, all the X-coordinates are set to 512, and the
C  horizontal centering is always set to 1, so each line of text will
C  be centered horizontally.  All character sizes will be 1.5 times
C  the default PLOTCHAR character size except the equation supplied
C  in CARD(8) that will be 3.5 times the default PLOTCHAR character
C  size.  Notice that each line has a Y-coordinate specified for it;
C  these coordinates range from 1500 at the top to 200 at the bottom
C  of the scroll.
C
      NCARDS = 12
      CARDS( 1) = \&'  512 1500    1  1.5The PLOTCHAR'
      CARDS( 2) = \&'  512 1400    1  1.5utility'
      CARDS( 3) = \&'  512 1300    1  1.5can be'
      CARDS( 4) = \&'  512 1200    1  1.5used to'
      CARDS( 5) = \&'  512 1100    1  1.5write'
      CARDS( 6) = \&'  512 1000    1  1.5an equation'
      CARDS( 7) = \&'  512  900    1  1.5like'
      CARDS( 8) = \&'  512  700    1  3.5C:S:2:N:=A:S:2:N:+B:S:2:N:'
      CARDS( 9) = \&'  512  500    1  1.5in a'
      CARDS(10) = \&'  512  400    1  1.5movie title'
      CARDS(11) = \&'  512  300    1  1.5created by'
      CARDS(12) = \&'  512  200    1  1.5STITLE'
C
C  Define the remaining inputs for STITLE.  
C
C  NYST specifies the Y-coordinate that will be at the vertical center
C  of the frame when scrolling begins.  The value specified (1300)
C  means that the line "can be" will be centered vertically on the
C  first frame and scrolling will proceed from there.
C
      NYST  = 1300
C
C  NYFIN specifies the Y-coordinate that will be at the vertical center 
C  of the frame when scrolling is terminated.  The value specified (400)       
C  means that the line "movie title" will be centered vertically on
C  the frame when scrolling terminates.
C
      NYFIN = 400
C
C  TST specifies how many seconds the first frame will remain 
C  stationary before scrolling begins.
C
      TST   = 1.0
C
C  Specify the scroll time.  This is the time in seconds that the 
C  text will be scrolled from position NYST to NYFIN.
C
      TMV   = 10.0
C
C  Specify how many seconds  the final frame will remain stationary
C  after scrolling stops.
C
      TFIN  =  0.5
C
C  Indicate that this will be a practice run, and not a production
C  run.
C
      MOVIE = 1
C
C  Call SLSETR to indicate that the first frame should be faded 
C  in for 2.5 seconds.
C
      CALL SLSETR('FIN',2.5)
C
C  Call SLSETR to indicate that the last frame should be faded 
C  out for 2.0 seconds.
C
      CALL SLSETR('FOU',2.0)
C
C  Call STITLE.
C
      CALL STITLE (CARDS,NCARDS,NYST,NYFIN,TST,TMV,TFIN,MOVIE)
C
      RETURN
      END
.eF
.ad
.fi
.L2 "Example 2"  
.in -.5i
Note:  The user will have to provide a driver similar to DSTITL below in
order to utilize test subroutine TSTITL, which is on the distribution tape.
The PWRITX binary database (referred to below as "pcrbin.out") must be
created prior to execution of DSTITL (see instructions in file PWRITXNT).
.sp
.nf
.sF
      PROGRAM DSTITL
C
C OPEN BINARY DATA FILE ON UNIT 3 TO BE USED WITH PWRITX
C
      OPEN(UNIT=3,FILE='pcrbin.out',STATUS='OLD',FORM='UNFORMATTED')
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL TSTITL (IER)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END


=========================================================================
=========================================================================

      SUBROUTINE TSTITL (IERROR)
C
C PURPOSE                To provide a simple demonstration of the
C                        routine STITLE.
C
C USAGE                  CALL TSTITL (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                            = 0  If there is a normal exit from STITLE
C                            = 1  Otherwise
C
C I/O                    If there is a normal exit from STITLE,
C                        the message
C
C                          STITLE TEST SUCCESSFUL . . . SEE PLOTS TO
C                          VERIFY PERFORMANCE
C
C                        is written on unit 6
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       STITLE
C FILES
C
C LANGUAGE               FORTRAN
C
C HISTORY                Written  by members of the
C                        Scientific Computing Division of NCAR,
C                        Boulder Colorado
C
C PORTABILITY            FORTRAN 77
C
C
      CHARACTER*80    CARDS(6)
C
C Initialize the error parameter.
C
      IERROR = 1
C
C Store character strings in array CARDS.  These strings contain text,
C plus information regarding character size and location of the text
C on the scroll.
C
      NCARDS = 4
      CARDS(1) = \&'  512  760    1  1.5Demonstration'
      CARDS(2) = \&'  512  600    1  1.5Plot'
      CARDS(3) = \&'  512  440    1  1.0for'
      CARDS(4) = \&'  512  280    1  1.5STITLE'
C
C Define the remaining inputs to routine STITLE.  Note that the
C output produced (a single frame with no scrolling to appear for
C 6.0 seconds) could equally well have been produced by FTITLE.
C We call STITLE in this demo to avoid reading the input lines.
C
      NYST  = 512
      NYFIN = 512
      TST   = 0.0
      TMV   = 0.0
      TFIN  = 6.0
      MOVIE =   1
C
C Call STITLE.
C
      CALL STITLE (CARDS,NCARDS,NYST,NYFIN,TST,TMV,TFIN,MOVIE)
      IERROR = 0
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT ('     STITLE TEST SUCCESSFUL',24X,
     1        \&'SEE PLOTS TO VERIFY PERFORMANCE')
C
      END
.eF
.fi
.ad
.L1 "STITLE Errors" 
STITLE uses the subroutine SETER in the error handling
package ERPRT77 to generate error messages.  (See Appendix A
for more information about the NCAR Graphics Error Handling
Package.)  
.sp
SLINIT \(em No active workstations
Fatal error.
.sp
SLSETI \(em Invalid keyword
Non-fatal error.
.sp
SLGETI \(em Invalid keyword
Non-fatal error.
.sp
FTITLE \(em Number of input cards exceeds 120  
Fatal error.
.sp
SLSETR \(em Invalid keyword
Non-fatal error.
.sp
SLGETR \(em Invalid keyword
Non-fatal error.
.sp
.LE

