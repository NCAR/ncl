.\"
.\"	$Id: labelbar.m,v 1.1.1.1 1992-04-17 22:30:34 ncargd Exp $
.\"
.TH LABELBAR 3NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE3
.dsNA " LABELBAR - Create a labeled, filled, rectangular bar to serve ~~~~~~~~~~~~ ~~~~~~~~~~~~~~~ as a key for a filled plot
.dsS1 " CALL LBLBAR (IHOV,XLEB,XREB,YBEB,YTEB,NBOX,WSFB,HSFB,LFIN, ~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ IFTP,LLBS,NLBS,LBSB) Produces a complete label bar
.dsS2 " CALL LBGETI (WHCH,IVAL) Retrieves current integer values
.dsS3 " CALL LBGETR (WHCH,RVAL) Retrieves current real values
.dsS4 " CALL LBFILL (IFTP,XCRA,YCRA,NCRA,INDX) Fills label bars
.dsS5 " CALL LBSETI (WHCH,IVAL) Sets integer values
.dsS6 " CALL LBSETR (WHCH,RVAL) Sets real values
.nrsN 6
.ds f. man/labelbar.l
.pn 51
./" revised 9/27/89 w/new headers, footers, L1 heads, *i, *X -Laurie Pentell
./" revised 6/13/89 to include tab macro .tA
./" revised 6/27/89 to include correct headers & footers
./" revised 6/29/89 to include macro for right-adjusted headings (cg)
.tr ~
.PH ""
.PF ""
.EF "@\s125-%  \s9Text, Labels, and Legends\fR@@NCAR Graphics Guide to New Utilities@"
.OF "@\s9Version 3.00, October 1989@@\s9Text, Labels, and Legends \s125-%\s9@"
.EH "@\s9LABELBAR@@@"
./"  Headers & footers: replace chapter name, utility, and digit before the %
.ll 6.5i
.\"   heading macros for Graphics, Version 3.00, written by Nancy Dawson, 5/89
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
.de *X
.if \\nJ .tm .IE\tENTRY\t\\$1\t\\$2\t\\$3\t\\$4\t\\$5\t\\$6\t\\n%
..
\&
.ps 16
.ft B
.*i "LABELBAR"
.sp 3.5
.ps 12
.L1 "LABELBAR Introduction"
The utility LABELBAR lets you create a labeled, filled,
rectangular bar to 
serve as a key for a filled plot.
LABELBAR contains the user-callable
subroutines LBLBAR, LBFILL, LBSETI, LBSETR, LBGETI, and LBGETR.
These subroutines fall into three categories: subroutines for
creating a label bar, subroutines for setting internal parameter values, and
subroutines for retrieving internal parameter values.
.L2 "Subroutines for Creating a Label Bar"
A single call to the routine LBLBAR draws a complete label bar.
Arguments in the call let you specify the orientation of the bar, 
the portion of the plotter frame
in which the bar is to be drawn, the number of boxes into which the bar
is to be divided, the portion of each box to be filled, the type of fill,
and the labels (which are written in the unfilled portion of the bar).
Reading through the
discussion of LBLBAR's arguments will give you a feel for the
kinds of label bars that it can draw.  LBLBAR does the fill in one of
two ways: by a call to SFSGFA (a SOFTFILL subroutine) or
by a call to LBFILL.
.sp
The default version of the subroutine LBFILL does color
fill by calling the GKS routine GFA.  You can substitute your own
version of LBFILL to make LBLBAR use some other kind of fill.
.OH "@@@\s9LABELBAR@"
.L2 "Subroutines for Setting Internal Parameter Values"
Internal parameters of LABELBAR (variables whose scope is limited to the
subroutines in the LABELBAR utility) determine the color and
width of box lines, fill lines, and label lines.  You can use the
subroutines with names of the form LBSET\fIx\fR to give new values
to internal parameters.  For example, you can call LBSETI to set the
internal parameter \&'CBL', which specifies the color of the
box lines.
.L2 "Subroutines for Retrieving Internal Parameter Values"
The subroutines with names of the form LBGET\fIx\fR 
retrieve the current values of LABELBAR's internal parameters.
This capability is furnished for consistency with other utilities
in the NCAR Graphics package and also for programmers who
build interfaces to LABELBAR and need to save
and restore the values of its internal parameters.
.L1 "LABELBAR Calls"
.ne 6
.in 0
.ft B
.S 14
LBLBAR
.S 12
.L2 "Purpose"
LBLBAR draws a complete label bar.
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
.nr 38 \w\s11CALL LBLBAR\fR\s12~~~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(IHOV,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~XLEB,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~XREB,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~YBEB,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~YTEB,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NBOX,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~WSFB,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~HSFB,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~LFIN,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~IFTP,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~LLBS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NLBS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~LBAB)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
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
.nr 38 \wDimension\fB
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNBOX
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNLBS
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
.if t .if \n(TW>\n(.li .tm Table at line 123 file man/labelbar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fB
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\s11CALL LBLBAR\fR\s12~~~~~~~\h'|\n(41u'(IHOV,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~XLEB,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~XREB,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~YBEB,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~YTEB,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NBOX,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~WSFB,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~HSFB,\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~LFIN,\h'|\n(42u'Integer Array\h'|\n(43u'Input\h'|\n(44u'NBOX
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~IFTP,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~LLBS,\h'|\n(42u'Character Array\h'|\n(43u'Input\h'|\n(44u'NLBS
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NLBS,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~LBAB)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-18
.in +.5i
.VL 1.2i
.LI "\fBIHOV\fR"
An integer specifying whether the bar should be oriented horizontally
or vertically.  Use zero to orient the bar horizontally and a non-zero
value to orient the bar vertically.
.ft B
.LI "\fBXLEB"
.LI "XREB"
.LI "YBEB"
.LI "YTEB\fR"
.ft R
Four real values in the range 0. to 1. that together specify a rectangular
subset of the plotter frame in which the entire bar, including labels, is to
fit.
XLEB specifies the
position of the left edge of the bar; XREB, the position of the right edge of
the bar; YBEB, the position of the bottom edge of the bar; and YTEB, the
position of the top edge of the bar.
You should probably make a horizontal bar 
wider than it is high and a vertical bar higher than
it is wide.
.LI "\fBNBOX\fR"
An integer specifying the number of boxes (rectangular pieces)
into which the bar should be divided.
All boxes are equal in size.
A horizontal bar is divided into boxes by vertical lines,
and a vertical bar is divided into boxes by horizontal lines.  The
boxes in a horizontal bar are ordered from left to right,
and the boxes in a vertical bar are ordered from bottom to top.
This ordering determines
the sequence in which the boxes are filled and labeled.
.br
.ne 6
.ft B
.LI "WSFB"
.LI "HSFB"
.ft R
Two real numbers determining what part of each box is to be
filled.  (The rest of the box is unfilled or is used for labels.)
.sp
Each argument is a number in the range 0. to 1.  WSFB specifies a fraction
of the width of the box, and HSFB specifies a fraction of the height of the box.
For example, if you use WSFB = 0.333 and HSFB = 1.0,
the width of the filled portion of each
box will be one-third of the box's width, and fill will extend
all the way from the bottom
of the box to its top.
The exact position of each filled
portion within its box depends on how you choose to label the bar (see the
description of the argument LBAB on the next page);
positioning is automatically handled for you by LBLBAR.
.LI "\fBLFIN\fR"
An integer array containing a list of fill indices, one for each box in
the bar.  Each index determines how the filling of its box is to be done:
.sp
If you set IFTP (next argument) to 0, elements of this array will
be passed to the subroutine SFSGFA, in the package SOFTFILL, as values
for the argument ICI.
In this case, depending on the value of SOFTFILL's internal parameter \&'TY',
an element of LFIN will specify either
an area fill color index, a polyline color index, or a pattern
index.  For a detailed explanation of ICI's function, see the section
titled "How Fill is Done: ICI and Type of Fill ('TY')" 
in the documentation of the SOFTFILL routine SFSGFA.
.sp
If you set IFTP (next argument) to a non-zero value, elements of LFIN
will be passed to the subroutine LBFILL as
values for the argument INDX.
If you are using the default version of LBFILL, the LFIN element is
used as an argument of the GKS
subroutine GSFACI, specifying a color index.
.LI "\fBIFTP\fR"
An integer specifying how the filled portion of the boxes should be filled:
.sp
If the value of IFTP is zero, LBLBAR calls the
routine SFSGFA, in the package SOFTFILL, to do the filling.
The value of the
SOFTFILL internal parameter \&'TY' determines whether filling is done by
calls to the GKS routine GFA, by drawing
closely spaced colored lines to simulate solid fill, or by drawing
lines at varying angles and spacings to form patterns.
.sp
If the value of IFTP is non-zero, the routine LBFILL will be called to do
the filling.  If you have not supplied your own version of
LBFILL, fill will be done by calling the GKS routine GFA.
.LI "\fBLLBS\fR"
A character array providing a list of labels for the bar.
If the number of labels is equal to the number of boxes, LBLBAR
associates the labels with the boxes themselves.  If the number
of labels is one less than the number of boxes, LBLBAR associates
the labels with the divisions between boxes.  If the 
number of labels is one more than the number of boxes, LBLBAR
associates the first label with the beginning of the bar, the
last label with the end of the bar, and the labels in between with
the divisions between the boxes.
.sp
LBLBAR calls the PLOTCHAR routine PLCHHQ to draw the labels.
The LABELBAR internal parameter \&'CLB' determines the
color of the labels and the LABELBAR internal parameter \&'WLB' determines
the line-width scale factor to be used while drawing them.
.LI "\fBNLBS\fR"
An integer specifying the number of labels in the array LLBS.
NLBS must be such that NBOX-1 LE  NLBS LE  NBOX+1.
.LI "\fBLBAB\fR"
An integer in the range 0 to 3 specifying on which side or 
sides of the bar the labels are to be written.
The value 0 specifies that the bar is to be unlabeled;
the value 1, that the labels are to be below a horizontal bar or to the right
of a vertical bar; the value 2, that the labels are to be above a horizontal
bar or to the left of a vertical bar; and the value 3, that the labels are to
be repeated on both sides of the bar.
LBLBAR automatically scales labels so they do not overlap 
one another or run outside the area for the label bar.
Labels at the ends of the bar may run outside this area, however.
.LE
.L2 "Usage"
When IFTP is zero, LBLBAR does fill by calling the SOFTFILL routine
SFSGFA.  The argument array LFIN
contains the values that are passed to SFSGFA as values of its argument ICI.
Before calling LBLBAR, you can call the SOFTFILL subroutine
SFSETI to set the internal parameter \&'TY', which determines the type
of fill that SFSGFA will do.
For a detailed explanation of the use of \&'TY', see the section
titled "How Fill is Done: ICI and Type of Fill ('TY')" in the
documentation of the SOFTFILL routine SFSGFA.
.sp
By default, \&'TY' is zero, forcing SFSGFA to do fill by calling
the GKS subroutine GFA.
GFA does either hollow, solid, or
pattern fill.  Hollow fill (only boundaries are drawn) 
is the GKS default, but you can change the
type of fill used by calling the GKS subroutine GSFAIS.
Notice that this is one of the first steps in the code for Example 1.
.sp
If you make \&'TY' non-zero, forcing SFSGFA to do fill by drawing lines,
you can then use the LABELBAR internal parameter \&'WFL'
to change the width of the lines.  If you make \&'TY' negative, forcing
SFSGFA to do fill by drawing lines that form patterns, you can use the
LABELBAR internal parameter \&'CFL' to set the color of the lines.
When \&'TY' is positive, line color is determined by the indices in the
array LFIN.
.sp
After filling is complete, LBLBAR outlines the filled portions of the boxes,
using the color and line width specified by 
the LABELBAR internal parameters \&'CBL' and \&'WBL'.
.in 0
.ft B
.S 14
LBFILL
.S 12
.L2 "Purpose"
LBFILL is called by LBLBAR (when the argument IFTP is non-zero) to fill
portions of the bar.  The
default version of this routine does color fill by calling GSFACI and GFA.
You may supply your own version of LBFILL to do any desired kind of fill.
LBFILL is called by a statement like the following:
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
.nr 38 \w\s11CALL LBFILL\fR\s12~~~~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(IFTP,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~XCRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~YCRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NCRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~INDX)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
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
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fB
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNCRA
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNCRA
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
.if t .if \n(TW>\n(.li .tm Table at line 293 file man/labelbar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fB
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\s11CALL LBFILL\fR\s12~~~~~~~~\h'|\n(41u'(IFTP,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~XCRA,\h'|\n(42u'Real Array\h'|\n(43u'Input\h'|\n(44u'NCRA
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~YCRA,\h'|\n(42u'Real Array\h'|\n(43u'Input\h'|\n(44u'NCRA
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NCRA,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~INDX)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-10
.in +.5i
.VL 1.2i
.LI "\fBIFTP\fR"
A fill-type selector.  When LBFILL is called by LBLBAR, this will be the
argument IFTP from your call to LBLBAR, and it will always be non-zero.
.LI "\fBXCRA\fR
.LI "\fBYCRA\fR
Two real arrays containing the X and Y normalized device coordinates
in the range from 0. to 1. that define an area to be filled.
When LBFILL is called by LBLBAR, there will be
five such points, and the last point will be a duplicate of the first.
.LI "\fBNCRA\fR"
The number of points defining the area to be filled.  When LBFILL is called
by LBLBAR, its value will always be 5.
.LI "\fBINDX\fR"
A color index or pattern selector.  When LBFILL is called by LBLBAR, this
will always be an element from the array LFIN in your call to LBLBAR.
Its value may be used as a color index or as a pattern selector.
.LE
.br
.ne14
.L2 "Usage"
This routine is called when the LBLBAR argument IFTP is
non-zero.  It is
called once for each filled portion of the label bar.
It is expected to fill that portion of the box defined by the argument
arrays XCRA and YCRA.  The default
version of the routine looks like this:
.sp
.sF
SUBROUTINE LBFILL (IFTP,XCRA,YCRA,NCRA,INDX)
  DIMENSION XCRA(*),YCRA(*)
  CALL GSFACI (INDX)
  CALL GFA (NCRA-1,XCRA,YCRA)
  RETURN
END
.eF
.sp
.ne 4
This default version fills an area by calling GFA.  The argument INDX
is used as a color index.
You may replace this routine with a version that does any desired
sort of fill; usually, this can be done by simply compiling your own version,
so that the default one from the package will not be loaded.
.L1 "LABELBAR Parameter Access Subroutines"
LABELBAR has six internal parameters that further
affect the appearance of a label bar.
There are two subroutines to set the values of internal parameters
\(em one for each type of parameter: integer and real.
Likewise, there are two subroutines to retrieve the current
values of internal parameters.
These four subroutines are
described below, followed by a description of each of the six
internal parameters.
.L2 "Setting Internal Parameter Values"
Use the following subroutines to reset the current values of
internal parameters.
Only the first three characters of the internal parameter
name are examined by these subroutines.  It is recommended that
the rest of the character string be used to improve the
readability of your code.  For example, instead of just \&'WBL', use
.br
\&'WBL - WIDTH OF BOX LINES'.
.sp
Change the value of an internal parameter before the call to LBLBAR that
you want to be affected.
An internal parameter retains a value that it has been given
until another call to one of these routines changes its value again.
.sp
The subroutine LBSETI is passed an integer value \fIi\fR.
If you are setting an integer parameter, it
receives the value \fIi\fR.  If you are setting
a real parameter, it receives the value REAL(\fIi\fR).
.sp
The subroutine LBSETR is passed a real value \fIr\fR.  If
you are setting a real parameter, it
receives the value \fIr\fR.  If you are setting
an integer parameter, it
receives the value INT(\fIr\fR).  Note that the Fortran
intrinsic INT does truncation rather than rounding.
.sp
Thus, the subroutine LBSETR is the more general routine; it
provides access to all real and integer parameters.  LBSETI 
allows for more natural access to integer parameters
and to those real parameters whose values have no fractional
part.
.sp 2
.ne 8
.in 0
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
.nr 38 \w\fB\s11CALL LBSETI\fR\s12~~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~IVAL)
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
.if t .if \n(TW>\n(.li .tm Table at line 391 file man/labelbar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL LBSETI\fR\s12~~~~~~\h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~IVAL)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example,
\&'CBL').
.LI "\fBIVAL\fR"
The integer value you select for the parameter.
.LE
.in 0
.sp 2
.ne 8
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
.nr 38 \w\fB\s11CALL LBSETR\fR\s12~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~RVAL)
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
.if t .if \n(TW>\n(.li .tm Table at line 411 file man/labelbar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL LBSETR\fR\s12~~~~\h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~RVAL)\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example,
\&'WBL').
.LI "\fBRVAL\fR"
The real value you select for the parameter.
.LE
.sp
.L2 "Retrieving Current Internal Parameter Values"
If you need to recover the current value of one of the internal
parameters, use one of the following calls.  Only the first three
characters of the internal parameter name are examined by these
subroutines.  It is recommended that the rest of the character
string be used to improve the readability of your code.  For
example, instead of just \&'CBL', use \&'CBL - COLOR OF BOX LINES'.
.sp
The subroutine LBGETI returns an integer value.
If you are getting the value of an integer parameter, LBGETI
returns that integer value.  If you are getting the value of a
real parameter with value \fIr\fR,
then the value returned is INT(\fIr\fR).  Note
that the Fortran intrinsic INT does truncation rather than
rounding.
.sp
The subroutine LBGETR returns a real value.  If
you are getting the value of a real parameter, LBGETR returns that real
value.  If you are getting the value of an integer parameter with
value \fIi\fR,
then the value returned is REAL(\fIi\fR).
.sp
Thus, the subroutine LBGETR is the more general routine; it
provides access to all real and integer parameters.  LBGETI 
allows for more natural access to integer parameters
and to those real parameters whose values have no fractional
part.
.in 0
.sp 2
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
.nr 38 \w\fB\s11CALL LBGETI\fR\s12~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~IVAL)
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
.if t .if \n(TW>\n(.li .tm Table at line 458 file man/labelbar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL LBGETI\fR\s12~~~~~\h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~IVAL)\h'|\n(42u'Integer\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example,
\&'CFL').
.LI "\fBIVAL\fR"
An integer variable in which the current value of the parameter
is to be returned.
.LE
.br
.ne 10
.in 0
.sp 2
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
.nr 38 \w\fB\s11CALL LBGETR\fR\s12~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~RVAL)
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
.if t .if \n(TW>\n(.li .tm Table at line 480 file man/labelbar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL LBGETR\fR\s12~~~\h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~RVAL)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example,
\&'WFL').
.LI "\fBRVAL\fR"
A real variable in which the current value of the parameter is to
be returned.
.LE
.L1 "LABELBAR Internal Parameter Descriptions"
Internal parameters of LABELBAR are variables whose scope is limited to the
subroutines in the LABELBAR utility.  They affect how the box lines,
fill lines, and labels are drawn and colored.
This section details the use of each of the six internal
parameters available in LABELBAR.
.sp
.VL 1.2i
.ft B
.LI \&'CBL'
.LI \&'CFL'
.LI \&'CLB'
.ft R
Three integer values that control the color of box lines ('CBL'), the color
of fill lines ('CFL'), and the color of labels ('CLB').  \&'CFL' is used
only when the LBLBAR argument IFTP is zero and the SOFTFILL internal
parameter \&'TY' has a value in the range from -4 to -1, requesting pattern
fill.
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
The value specifies the color index to be used.
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
The color is unspecified by this parameter: the current polyline
color index will be used for box lines and fill lines; the
current text color index will be used for labels.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGE 0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0
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
.if t .if \n(TW>\n(.li .tm Table at line 523 file man/labelbar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBRange\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'GE 0\h'|\n(41u'
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
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'<0\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-14
.tA "Integer" "-1"
.sp
.ft B
.LI \&'WBL'
.LI \&'WFL'
.LI \&'WLB'
.ft R
Three real values that control the width of box lines ('WBL'), the width
of fill lines ('WFL'), and the width of the lines used to draw
labels ('WLB').  Labels are affected by a change in line width
because they are drawn by the
routine PLCHHQ in the utility PLOTCHAR, 
which strokes out characters using lines.  \&'WFL' is used only when
the LBLBAR argument IFTP is zero and the SOFTFILL internal
parameter \&'TY' has a non-zero value so that fill is done by drawing lines.
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
The width will be the "normal" width multiplied by this factor.
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
The width is unspecified by this parameter: the current line-width
scale factor will be used.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLE 0
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
.if t .if \n(TW>\n(.li .tm Table at line 553 file man/labelbar.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBRange\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'>0\h'|\n(41u'
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
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LE 0\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.tA "Real" "0."
./".SK 1
./".L1 "LABELBAR Example \(em Output"
./".sp 20
./".S 18
./"\fBReplace this page with a color plate\fR
./".S 12
./".MF labelbar.meta 1 3.0
./".SK 1
.L1 "LABELBAR Example \(em Code"
.in -.5i
This example shows how to produce several different label bars on
a single frame.
.sp
.nf
.sF
Note:  The user will have to provide a driver similar to DLBLBA below in
order to utilize test subroutine TLBLBA, which is on the distribution tape.

      PROGRAM DLBLBA
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL TLBLBA (IER)
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
 
      SUBROUTINE TLBLBA (IERR)
C
C LATEST REVISION       July, 1989
C
C PURPOSE               To provide a simple demonstration of the use of
C                       LABELBAR to draw various kinds of labelled bars.
C
C USAGE                 CALL TLBLBA (IERR)
C
C ARGUMENTS (OUTPUT)    IERR
C
C                         An integer variable
C                         = 0, if the test was successful,
C                         = 1, otherwise
C
C I/O                   If the test is successful, the message "LABELBAR
C                       TEST EXECUTED--SEE PLOTS TO CERTIFY" is printed
C                       on unit 6.  In addition, a single frame is drawn
C                       on the graphics device.  In order to determine
C                       if the test was successful, it is necessary to
C                       examine this frame.
C
C PRECISION             Single.
C
C LANGUAGE              FORTRAN 77.
C
C REQUIRED ROUTINES     LABELBAR, SOFTFILL, and PLOTCHAR packages.
C
C REQUIRED GKS LEVEL    0A.
C
C ALGORITHM             Three simple label bars are drawn.
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Declare the arrays to hold the list of indices and the lists of labels
C required by the label-bar routine.
C
        DIMENSION LND1(20),LND2(16),LND3(16)
C
        CHARACTER*12 LLB1(20)
        CHARACTER*10 LLB2(17)
        CHARACTER*1  LLB3(16)
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Define the list of indices required by the label-bar routine.
C
        DATA LND1 / 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20 /
        DATA LND2 / 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 /
        DATA LND3 / 12,13,14,15,8,9,10,11,4,5,6,7,0,1,2,3 /
C
C Define labels for various bars.
C
        DATA LLB1 /   \&'0 to 5:H2Q', \&'5 to 10:H1Q','10 to 15:H1Q',
     +              \&'15 to 20:H1Q','20 to 25:H1Q','25 to 30:H1Q',
     +              \&'30 to 35:H1Q','35 to 40:H1Q','40 to 45:H1Q',
     +              \&'45 to 50:H1Q','50 to 55:H1Q','55 to 60:H1Q',
     +              \&'60 to 65:H1Q','65 to 70:H1Q','70 to 75:H1Q',
     +              \&'75 to 80:H1Q','80 to 85:H1Q','85 to 90:H1Q',
     +              \&'90 to 95:H1Q','95 to 100'   /
C
        DATA LLB2 / \&'-2000 feet',' Sea level',' 2000 feet',
     +              \&' 4000 feet',' 6000 feet',' 8000 feet',
     +              \&'10000 feet','12000 feet','14000 feet',
     +              \&'16000 feet','18000 feet','20000 feet',
     +              \&'22000 feet','24000 feet','26000 feet',
     +              \&'28000 feet','30000 feet'/
C
        DATA LLB3 / \&'M','N','O','P','I','J','K','L','E','F','G','H',
     +              \&'A','B','C','D'/
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Define color indices.
C
        CALL LBCLRS
C
C Force PLOTCHAR to use constant spacing of characters.
C
        CALL PCSETR ('CS - CONSTANT SPACING',1.25)
C
C Set some parameter values.
C
        CALL LBSETR ('WBL - WIDTH OF BOX LINES',4.)
        CALL LBSETR ('WFL - WIDTH OF FILL LINES',2.)
        CALL LBSETR ('WLB - WIDTH OF LABEL LINES',2.)
C
C Put the first label bar vertically along the left edge of the plotter
C frame.  Use patterns.
C
        CALL SFSETI ('ANGLE OF FILL LINES',15)
        CALL SFSETI ('TYPE OF FILL',-4)
        CALL LBLBAR (1,.05,.30,.05,.95,20,.3333,1.,LND1,0,LLB1,20,2)
C
C Put the second label bar vertically along the right edge.  Use solid
C color fill.
C
        CALL SFSETI ('TYPE OF FILL',0)
        CALL LBLBAR (1,.70,.95,.05,.95,16,.3333,1.,LND2,0,LLB2,17,1)
C
C The remaining label bars are arranged horizontally in such a way as
C to form a rectangular key for color indices 1 through 12.  The
C default version of LBFILL is used.
C
        CALL LBLBAR (0,.35,.65,.05,.20,4,.5,.5,LND3( 1),1,LLB3( 1),4,1)
        CALL LBLBAR (0,.35,.65,.20,.35,4,.5,.5,LND3( 5),1,LLB3( 5),4,1)
        CALL LBLBAR (0,.35,.65,.35,.50,4,.5,.5,LND3( 9),1,LLB3( 9),4,1)
        CALL LBLBAR (0,.35,.65,.50,.65,4,.5,.5,LND3(13),1,LLB3(13),4,1)
C
C Put a title on the plot.  We must first call SET to define the ranges
C of the X and Y coordinates to be used.  The constant spacing feature
C is turned off so that the title will look normal.
C
        CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
        CALL WTSTR  (.5,.90,'THREE',3,0,0)
        CALL WTSTR  (.5,.85,'LABELBAR',3,0,0)
        CALL WTSTR  (.5,.80,'EXAMPLES',3,0,0)
C
C Advance the frame.
C
        CALL FRAME
C
C Log a successful-completion message and return to the caller.
C
        WRITE (6,1001)
C
        RETURN
C
 1001 FORMAT (' LABELBAR TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
      SUBROUTINE LBCLRS
C
C Define a set of RGB color triples for colors 1 through 15.
C
        DIMENSION RGBV(3,15)
C
C Define the RGB color triples needed below.
C
        DATA RGBV / 1.00 , 1.00 , 1.00 ,
     +              0.70 , 0.70 , 0.70 ,
     +              0.75 , 0.50 , 1.00 ,
     +              0.50 , 0.00 , 1.00 ,
     +              0.00 , 0.00 , 1.00 ,
     +              0.00 , 0.50 , 1.00 ,
     +              0.00 , 1.00 , 1.00 ,
     +              0.00 , 1.00 , 0.60 ,
     +              0.00 , 1.00 , 0.00 ,
     +              0.70 , 1.00 , 0.00 ,
     +              1.00 , 1.00 , 0.00 ,
     +              1.00 , 0.75 , 0.00 ,
     +              1.00 , 0.38 , 0.38 ,
     +              1.00 , 0.00 , 0.38 ,
     +              1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
        CALL GSCR (1,0,0.,0.,0.)
C
        DO 101 I=1,15
          CALL GSCR (1,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
.eF
.L1 "LABELBAR Errors"
When error conditions are detected, the support routine SETER is called.
SETER writes a message to the standard error file (as defined
by I1MACH(4)) and then terminates execution of your program.
(See Appendix A for more information about SETER and
the NCAR Graphics Error Handling Package, ERPRT77.)  The 
possible error messages are as follows.  Where \fIx\fR appears
below, it will be replaced by the erroneous parameter name.
.sp
.nf
LBLBAR - ERROR EXIT FROM GQFACI
LBLBAR - ERROR EXIT FROM GQLWSC
LBLBAR - ERROR EXIT FROM GQPLCI
LBLBAR - ERROR EXIT FROM GQTXCI
.fi
These errors indicate that GKS was in the wrong state
when the listed GKS routine was called.  Check that you have opened
GKS by calling either the SPPS subroutine OPNGKS or the appropriate GKS
routines.
.sp
LBGETI OR LBGETR - PARAMETER NAME TOO SHORT - \fIx\fR
Internal parameter names are at least three characters long.
Correct the name of the internal parameter.
.sp
LBGETI OR LBGETR - PARAMETER NAME NOT KNOWN - \fIx\fR
The subroutine does not recognize the internal parameter name you
have used.
Correct the name of the internal parameter.
.sp
LBSETI OR LBSETR - PARAMETER NAME TOO SHORT - \fIx\fR
Internal parameter names are at least three characters long.
Correct the name of the internal parameter.
.sp
LBSETI OR LBSETR - PARAMETER NAME NOT KNOWN - \fIx\fR
The subroutine does not recognize the internal parameter name you
have used.
Correct the name of the internal parameter.
.ad
