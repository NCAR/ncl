.\"
.\"	$Id: conpack.m,v 1.1.1.1 1992-04-17 22:30:23 ncargd Exp $
.\"
.TH CONPACK 3NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE3
.dsNA " CONPACK - Contour regularly distributed (gridded) data
.dsS1 " CALL CPRECT (ZDAT,KZDT,MZDT,NZDT,RWRK,KRWK,IWRK,KIWK) ~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Initializes contouring of rectangular array of data
.dsS2 " CALL CPSPRS (ZSPS,KSPS,MSPS,NSPS,RWRK,KRWK,IWRK,KIWK,ZDAT,LZDT) ~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Interpolates data to a dense grid and initializes contouring
.dsS3 " CALL CPPKCL (ZDAT,RWRK,IWRK) Picks a set of contour levels
.dsS4 " CALL CPPKLB (ZDAT,RWRK,IWRK) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Picks a set of labels for labeled contour levels
.dsS5 " CALL CPCLDR (ZDAT,RWRK,IWRK) Draws contour lines
.dsS6 " CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,RTPL) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Draws contour lines masked by existing area map
.dsS7 " CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA) Adds contour lines to area map
.dsS8 " CALL CPLBDR (ZDAT,RWRK,IWRK) Draws labels
.dsS9 " CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA) Adds label boxes to area map
.dss1 " CALL CPBACK (ZDAT,RWRK,IWRK) Draws a background
.dss2 " CALL CPDRPL (XCS,YCS,NCS,IAI,IAG,NAI) Provides polyline-drawing for CPCLDM
.dss3 " CALL CPMPXY (IMAP,XINP,YINP,XOTP,YOTP) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Maps from rectangular coord. system to other coord. system
.dss4 " CALL CPCNRC (ZDAT,KZDT,MZDT,NZDT,FLOW,FHGH,FINC,NSET,NHGH,NDSH) ~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Simulates old routine CONREC
.dss5 " CALL CPEZCT (ZDAT,MZDT,NZDT) Simulates old subroutine EZCNTR (in CONREC)
.dss6 " CALL CPCHCF (IFLG) Changes drawing of constant-field message
.dss7 " CALL CPCHCL (IFLG) Changes drawing of contour lines
.dss8 " CALL CPCHHL (IFLG) Changes drawing of high and low labels
.dss9 " CALL CPCHIL (IFLG) Changes drawing of informational labels
.dsN1 " CALL CPCHLL (IFLG) Changes drawing of line labels
.dsN2 " CALL CPSETC (WHCH,CVAL) Sets character values
.dsN3 " CALL CPSETI (WHCH,IVAL) Sets integer values
.dsN4 " CALL CPSETR (WHCH,RVAL) Sets real values
.dsN5 " CALL CPRSET Resets default values
.dsN6 " CALL CPGETC (WHCH,CVAL) Retrieves current character values
.dsN7 " CALL CPGETI (WHCH,IVAL) Retrieves current integer values
.dsN8 " CALL CPGETR (WHCH,RVAL) Retrieves current real values
.nrsN 26
.ds f. man/conpack.l
./" revised 6/13/89 to include tab macro .tA
./" revised 9/18/89 w/new headers, footers, L1 heads
./" revised 6/13/89 to include tab macro .tA
./" revised 6/27/89 to include correct headers & footers
./" revised 6/29/89 to include macro for right-adjusted headings (cg)
.\" Index Entry Macro Definition
.de *X
.if \\nJ .tm .IE\tENTRY\t\\$1\t\\$2\t\\$3\t\\$4\t\\$5\t\\$6\t\\n%
..
.tr ~
.PH ""
.PF ""
.EH "@\s9CONTOURS@@@"
.EF "@\s123-%  \s9Contours\fR@@NCAR Graphics Guide to New Utilities@"
.OF "@\s9Version 3.00, October 1989@@\s9Contours~~\s123-%\s9@"
./"  Headers & footers: replace chapter name, utility, and digit before the %
./"  Put odd header command after first page of text so it won't print on pg1;
./"  Odd header:  .OH "@@@\s9TITLE@"
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
.ft B
.ps 18
.*i CONTOURS
.sp 4
.L1 "Chapter Overview"
Given a surface defined over a portion of the plane, contouring is the
process of drawing curves representing horizontal slices through the surface
at each of a set of "contour levels."
.sp
Contouring algorithms may be classified according to the distribution of
the data that define the surface.  If the data are defined on a regular
rectangular grid, contouring can be done rather easily.  Irregularly spaced,
but still rectangular, grids can be handled almost as easily.  Rectangular
grids that have been subjected to a continuous mathematical transformation
(as, for example, that which transforms a rectangle into a ring) can be
handled with only a little more difficulty.  Such data are often referred
to as \fIgridded\fR data.
.sp
If, on the other hand, the data are scattered "at random" in the plane
(referred to loosely as \fIrandom\fR data), contouring is not done so easily.
Contouring directly from the random data is difficult and is seldom done.
The approach that will eventually be used by CONPACK will be to use an
interpolation scheme to obtain gridded data from the original random data
and then to contour that gridded data.
.sp
Which contouring utility you use depends on whether you are working
with gridded or random data.
.L2 "Contouring from Gridded Data"
If you have gridded data, use CONPACK,
which is a new utility in the Version 3.00 release of NCAR Graphics.
CONPACK duplicates the functions of
the older utilities CONREC, CONRECSPR, and CONRECQCK,
which were part of the
previous version of NCAR Graphics (Version 2.00).
CONPACK also provides new functionality, such as color capabilities.  It
is documented in this chapter.
.L2 "Contouring from Random Data"
This first release of CONPACK does not include the ability to 
contour random data \(em something that will be included in the Version 4.00
release.
If you have random data,
we suggest that you use the BIVAR utility 
to interpolate from the random data
to gridded data, and then use CONPACK to produce your contours.
BIVAR is 
public domain software and is 
included on the NCAR Graphics Version 3.00 distribution tape.
It is documented internally;
no printed documentation is provided.
For information on how to access the BIVAR documentation from the tape,
see your NCAR Graphics site representative.
.sp
The Version 2.00 utilities CONRAN, CONRAQ, and CONRAS are
still available for contouring random data.
However, using the CONPACK/BIVAR combination has two distinct
advantages:  (1) it gives you the option of color contouring, and
(2) CONRAN, CONRAQ, and CONRAS are known to contain elusive bugs.
For these reasons, we recommend using BIVAR/CONPACK
instead of CONRAN, CONRAQ, and CONRAS.
\&
.ps 16
.B
.*i "CONPACK"
.R
.sp 3.5
.ps 12
.EH "@\s9CONPACK@@@"
.OH "@@@\s9CONPACK@"
.L1 "CONPACK Introduction"
CONPACK constructs contour plots from rectangular arrays of data.
It provides a "tool kit" of Fortran subroutines
that may be called in various combinations in order to draw different
styles of contour plots.
CONPACK is a powerful and complex
utility, and as such,
requires some learning time.
We recommend that 
you examine the examples
and accompanying codes frequently to help you understand the documentation. 
.sp
To read this chapter and to use CONPACK effectively, you should 
be familiar with NCAR Graphics in general.
Using some of CONPACK's capabilities will require knowledge
of the utilities PLOTCHAR, DASHCHAR, AREAS, and EZMAP.
(PLOTCHAR is documented in Chapter 5 of this manual;
DASHCHAR, AREAS, and EZMAP are documented in the
\fINCAR Graphics User's Guide, Version 2.00.\fR)
.sp
In this first release of the CONPACK contouring package, there are only two
routines that will draw a complete contour plot in response to a single
call.  These are the transition subroutines CPCNRC and CPEZCT, which simulate
the routines CONREC and EZCNTR of the Version 2.00 CONREC contouring utility.
The purpose of these routines is to allow users to easily make the transition
from the use of CONREC to the use of CONPACK.  These routines, however, do
not take advantage of all the new functionality of CONPACK; in particular,
they do not allow for the use of color.
.sp
In this first release, to access the full functionality of CONPACK, you
must call the lowest-level CONPACK routines.  Many of you will probably write
a CONPACK driver \(em a single routine which, when called, draws a complete
contour plot in the style that you prefer.  
The CONPACK examples later in this chapter
will be very helpful in constructing such a
driver.
.sp
It is possible that future releases of CONPACK will include additional "one
call does all" driver routines for CONPACK.  Your feedback will help
us decide what routines would be useful.
Use the Reader Comment Form in the back of this manual, or send a letter 
with your comments to:
.sp .5
.in 1.5i
.nf
NCAR/SCD
Scientific Visualization Group Head
P.O. Box 3000
Boulder, CO 80307-3000
.fi
.in -1.5i
.sp .5
The remainder of this introduction gives brief descriptions of all the routines
of CONPACK and discusses strategies for using them.
.L2 "Subroutines \(em Brief Descriptions"
CONPACK contains 26 user-callable subroutines
that fall into 6 functional categories.
The categories and each subroutine are described briefly in this section.
Note that the parameter access subroutines, which are described last,
are used to set internal parameter values and retrieve internal parameter
values.
The name of each CONPACK subroutine starts with CP (for CONPACK), 
followed by four letters
derived from the words that describe
the subroutine's function.
.sp
Details of using each subroutine are
under "CONPACK Calls," later in this chapter.
An alphabetical list of the subroutines (and page references)
appears at the beginning of "CONPACK Calls."
.L3 "Initialization Subroutines"
Drawing a contour plot starts with a call to an initialization routine
(perhaps preceded by one or more calls to set the values of the internal
parameters).
There are two initialization routines; which one you call
depends on the type of data to be contoured.  The two possibilities
are as follows:
.sp
.ne 5
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, rectangular array).
Initializes the contouring of a rectangular array of data.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, sparse array).  Interpolates from an array of data on a \fIsparse\fR
rectangular grid to an array of data on a \fIdense\fR rectangular grid and then
initializes contouring from the array on the dense grid.
A \fIsparse grid\fR refers to a grid with such small dimensions that
contour lines drawn directly on it are composed of long straight segments.
A \fIdense grid\fR refers to a grid with
sufficiently large dimensions to avoid this problem.
This may be viewed as a type of three-dimensional smoothing.
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
.nr 38 \w\fBCPRECT\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPSPRS\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 4.8in
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
.if t .if \n(TW>\n(.li .tm Table at line 215 file man/conpack.l is too wide - \n(TW units
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
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBCPRECT\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPSPRS\fR\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-18
.L3 "Contour Level and Label Subroutines"
Among the internal parameters are arrays that completely specify
contour levels
of interest and what is to be done at each of those levels.  You can use
these arrays
to take complete control of the contouring process.  Most users will
probably elect not to do this, but let CONPACK choose the
levels.  After initialization,
then,
as a rule, none of these internal parameter arrays will have been filled.
You do not need to do calls to fill them; 
if they are empty when they are needed,
CONPACK will select the required values at that point.  
.sp
For certain applications,
however, it is desirable to force the selection of contour levels and perhaps
the selection of
the character strings that are to be used as contour labels.  Do this
by calls to one or both of the following subroutines:
.sp
.ne 5
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, pick contour levels).  Picks a set of contour levels.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, pick labels).  Picks labels to be associated 
with those levels for which the contour lines are 
to be labeled and for which no labels have yet been specified.
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
.nr 38 \w\fBCPPKCL\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPPKLB\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 4.8in
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
.if t .if \n(TW>\n(.li .tm Table at line 249 file man/conpack.l is too wide - \n(TW units
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
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBCPPKCL\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPPKLB\fR\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-12
.sp
The advantage of calling one or both of these subroutines is that, after each
call, 
you can modify what the subroutine called has done in order to produce
desired effects.
For example, after CPPKCL is called, 
you might set elements
in internal parameter arrays that will cause the labeled contour lines to be solid
and the unlabeled contour lines to be dashed.  Or, after
CPPKLB is called, 
you might check for the string "0" as a label and change it
to "0.0."
.L3 "Action Subroutines"
After calls to CPPKCL and/or CPPKLB, you can call any of the
following subroutines
to perform the indicated action:
.sp
.ne 5
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, contour lines, draw).  Draws a set of contour lines.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, contour lines, drawn masked).  Draws a 
set of contour lines, as masked by an existing area map.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, contour lines to area map).  Adds a set 
of contour lines to an area map.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, labels, draw).  Draws labels for the contour plot.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, label boxes to area map).  Adds label boxes to an area map.  
The area map will most likely be used to 
prevent contour lines from passing through the labels.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, background).  Draws a simple background.
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
.nr 38 \w\fBCPCLDR\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPCLDM\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPCLAM\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPLBDR\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPLBAM\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPBACK\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
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
.nr 38 4.8in
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
.if t .if \n(TW>\n(.li .tm Table at line 298 file man/conpack.l is too wide - \n(TW units
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
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBCPCLDR\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPCLDM\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPCLAM\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPLBDR\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPLBAM\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPBACK\fR\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-30
.sp
The following routines are used by one or more of the routines listed
above:
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, draw polyline).
CPDRPL may be specified as the routine to be called by CPCLDM
to draw the pieces of contour lines resulting from masking 
by an area map.
It will draw the polyline defined by its first 
three arguments if and only
if none of the area identifiers defined by the remaining arguments is
negative.  CPDRPL 
will use a call to CURVE or a call to CURVED, depending
on the current value of the internal parameter \&'DPU'.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, map X and Y coordinates).  CPMPXY is 
called by various routines (listed above) when the
internal parameter \&'MAP' is non-zero.
It maps the coordinates of points on the data grid to some
other coordinate system.
The default version of CPMPXY handles the mapping 
of longitudes and latitudes into rectangular coordinates on a 
given EZMAP projection and the mapping of polar coordinates 
into rectangular coordinates.
If you want to handle other mappings,
you can supply your own version of CPMPXY.
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
.nr 38 \w\fBCPDRPL\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPMPXY\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 4.8in
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
.if t .if \n(TW>\n(.li .tm Table at line 332 file man/conpack.l is too wide - \n(TW units
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
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBCPDRPL\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPMPXY\fR\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-29
.sp
Finally, to advance the frame, you must call the SPPS subroutine FRAME;
CONPACK won't do it.
.L3 "Transition Subroutines"
For users who have NCAR Graphics Version 2.00 codes that contain
calls to CONREC or EZCNTR, CONPACK provides two 
subroutines to ease the transition 
to CONPACK. 
New users of CONPACK may also elect to use one of these subroutines
as an easy way to produce basic contour plots.
However, while the transition subroutines allow 
you to draw a complete contour plot
with a single call, they
do not provide access to the full range of
capabilities of CONPACK, such as color fill.
The two transition subroutines are as follows:
.sp
.ne 5
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, CONREC simulation).  Simulates the behavior of the routine 
CONREC, in the utility CONREC.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, EZCNTR simulation).  Simulates the behavior 
of the routine EZCNTR, in the utility CONREC.
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
.nr 38 \w\fBCPCNRC\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPEZCT\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 4.8in
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
.if t .if \n(TW>\n(.li .tm Table at line 363 file man/conpack.l is too wide - \n(TW units
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
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBCPCNRC\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPEZCT\fR\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-12
.sp
By setting internal parameters prior to calling CPCNRC or CPEZCT, 
you can
change the quality of the output, thereby simulating either the behavior of
the default version, the smooth version, or the super version of CONREC.
.sp
Users may wish to obtain copies of these routines 
to use as starting points
for the construction of their own CONPACK-based contouring subroutines.
For information on obtaining copies of these routines, contact your NCAR
Graphics site representative.
.L3 "Change Subroutines"
Subroutines with names of the form CPCH\fIxx\fR (the change subroutines)
are not called by you, but by CONPACK itself.
The default versions of the subroutines described in this section
are "place holders" \(em they
exist simply to be replaced by you.  They provide a means by which you can
change the labels and contour lines.  These subroutines are as follows:
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, change constant field label).  Called just before
and just after each step in the process of 
writing a constant-field message 
(which happens when CPRECT or CPSPRS determines that 
the data field being contoured is effectively constant).  
You can supply your own version to change line width, color, and so on.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, change contour lines).
Called just before and just after each contour line is drawn.
You can supply your own version to change line width, color, dash
pattern, and so on.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, change high/low labels).
Called just before and just after each step in
the process of writing the high and low labels.
You can supply your own version to change line width, color, and so on.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, change informational label).
Called just before and just after each step in the
process of writing the informational label.  You can supply your
own version to change line width, color, and so on.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, change line labels).
Called just before and just after each step in the
process of writing a contour line label.  You can supply your own version
to change line width, color, and so on.
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBCPCHCF\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPCHCL\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPCHHL\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPCHIL\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPCHLL\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
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
.nr 38 4.8in
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
.if t .if \n(TW>\n(.li .tm Table at line 423 file man/conpack.l is too wide - \n(TW units
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
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBCPCHCF\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPCHCL\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPCHHL\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPCHIL\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPCHLL\fR\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-40
.L3 "Parameter Access Subroutines"
You may need to set
internal parameters, which affect the behavior of the subroutines 
that are called after that.
Most such parameter-setting is done prior to the initialization call for the
contour plot to be affected; some is done after the initialization call and
after calls to CPPKCL and/or CPPKLB.
All internal parameters have default values \(em you only need to set 
the values of the internal parameters you want to change.
(In some cases, you may not need to set any internal parameters.)
Call the following subroutines to set
the values of internal parameters:
.sp
.ne 5
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, set character).  Assigns a value of type character 
to an internal parameter.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, set integer).  Assigns a value
of type integer to an internal parameter.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, set real).  Assigns a value of type real to an internal 
parameter.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, reset).  Resets all internal parameters to their default values.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBCPSETC\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPSETI\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPSETR\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPRSET\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
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
.nr 38 4.8in
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
.if t .if \n(TW>\n(.li .tm Table at line 460 file man/conpack.l is too wide - \n(TW units
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
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBCPSETC\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPSETI\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPSETR\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPRSET\fR\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
In general, once you assign a value to an internal parameter,
it retains that value until you make another call to reset it.
Thus, you only need to set most internal parameters once;
you need not reset them for each new contour plot.
.sp
At any time, it is possible to retrieve the value of an internal parameter
by calling one of these subroutines:
.sp
.ne 5
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, get character).  Gets a value of type character.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, get integer).  Gets a value of type integer.
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
.ll 4.8in
.if \n(.l<\n(81 .ll \n(81u
.in 0
(CONPACK, get real).  Gets a value of type real.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBCPGETC\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPGETI\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBCPGETR\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 4.8in
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
.if t .if \n(TW>\n(.li .tm Table at line 484 file man/conpack.l is too wide - \n(TW units
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
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBCPGETC\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPGETI\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fBCPGETR\fR\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-14
.L2 "Strategies for Using CONPACK"
The design of CONPACK allows you to construct contour plots in many different
styles.  
Which style you choose depends on the amount of computer time and
memory available, on the capabilities of the device on which the plots are
being drawn, and on the intended use of the plots.  Some possibilities are
outlined below.  
For details of using the subroutines mentioned here, 
see the section entitled "CONPACK Calls."
For details of using any internal parameters mentioned here, see
the section entitled "CONPACK Internal Parameter Descriptions."
.sp
\fBNote:\fR  Brackets [  ] are used below to surround steps that are optional.
.L3 "Simplest Use"
.AL
.LI
[Call PCSETC, PCSETI, and/or PCSETR if you want to set any
internal parameters.]
.LI
Call CPCNRC to produce a contour plot similar to those produced by CONREC
(in Version 2.00 of NCAR Graphics).
.LI
Call FRAME to advance the frame.
.LE
.>>
or
.<<
.AL
.LI
[Call PCSETC, PCSETI, and/or PCSETR if you want to set any internal parameters.]
.LI
Call CPEZCT to produce a contour plot similar to those produced by EZCNTR
(a subroutine of CONREC in Version 2.00 of NCAR Graphics).
.LE
.sp
The advantage of using either of these methods is that you can produce 
contour plots
with just one call to a subroutine (assuming that you are satisfied with the
default values of the internal parameters).
The disadvantage is that not all of the new capabilities of CONPACK are available.
For example, you cannot do color fill.
.L3 "No Color Fill \(em Basic Low-level Calls"
.AL
.LI
[Call PCSETC, PCSETI, and/or PCSETR if you want to set any internal parameters.]
.LI
Call CPRECT or CPSPRS to initialize drawing the contour plot.
.LI
[Call CPPKCL and/or CPPKLB if you want to set the contour levels and/or
the contour labels.
This will require additional calls to
PCSETC, PCSETI, and/or PCSETR, as well.]
.LI
[Call CPBACK if you want to draw a simple background.]
.LI
Call CPCLDR to draw the contour lines, with labels drawn by
the utility DASHCHAR.
.>>
Or, make the following three calls:
.<<
Call ARINAM (in the utility AREAS) to initialize an area map.
.sp
Call CPLBAM to generate a list of label positions and to put label boxes
at those positions into the area map.
.sp
Call CPCLDM to draw contour lines masked against the area map.
Each line is broken into pieces lying outside label boxes and pieces
lying inside label boxes; only the pieces outside the label boxes are
drawn.
.LI
[Call CPLBDR if you want to draw the informational label, the
high/low labels, and the line labels, if they were not drawn
in Step 5, above.]
.LI
Call FRAME to advance the frame.
.LE
.ne 6
.L3 "Color Fill \(em Basic Low-level Calls"
.AL
.LI
[Call PCSETC, PCSETI, and/or PCSETR if you want to set any internal
parameters.]
.LI
Call CPRECT or CPSPRS to initialize drawing the contour plot.
.LI
[Call CPPKCL and/or CPPKLB if you want to set the contour levels and/or
the contour labels.
This will require additional calls to
PCSETC, PCSETI, and/or PCSETR, as well.]
.LI
Call ARINAM (in the utility AREAS) to initialize an area map.
.LI
Call CPCLAM to put contour lines into the area map.
.LI
Call ARSCAM (in the utility AREAS) to scan the area map and to recover from it
the polygons created by the lines in the area map.
A user-defined subroutine is called to fill (or not fill) each polygon.
Filling may be done by calls to subroutines in the utility SOFTFILL or
to the GKS subroutine GFA.
.LI
Call FRAME to advance the frame.
.LE
.sp
You can write labels on a solid-filled contour plot by adding a
call to CPLBAM after Step 5 and a call to CPLBDR after Step 6.
.sp
You can combine
the utilities CONPACK, AREAS, EZMAP, and/or EZMAPA 
to achieve other desired effects.  For example, if 
your contour plot represents output from an ocean model,
you may want to draw
contours (or to fill contour bands) only over the oceans on a background
drawn by EZMAP.  (See Example 8.) 
.L1 "CONPACK Organization and Philosophy"
This section describes various aspects of the design of CONPACK \(em aspects
you should understand to use the package effectively.  If you intend
to use only the routines CPCNRC or CPEZCT, or if you can work from an
example which is close enough to what you want to do, you may be able
to skip reading this section.
.L2 "Contour Level Selection"
The subroutines CPCLAM, CPCLDM, CPCLDR, CPLBAM, and CPLBDR all generate output
for specified sets of contour levels.  The internal parameter \&'NCL' specifies the
number of different contour levels for which something is to be done by one
or more of the above subroutines.  The value of \&'NCL' may not exceed 256.  For
each value of I from 1 through \&'NCL', the Ith element of the internal
parameter array
\&'CLV' specifies a contour level and the Ith element of each of the internal
parameter arrays 
\&'AIA', \&'AIB', \&'CLC', \&'CLD', \&'CLL', \&'CLU', \&'LLC', 
and \&'LLT' is an
associated datum, 
determining something about what is to be done at that contour
level.  See "CONPACK Internal Parameter Descriptions," later in this chapter,
for the detailed descriptions of these internal parameter arrays.
Brief descriptions of elements of these
internal parameter arrays are given here:
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
.rm 80 81 82
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/4u
.if \n(.l<\n(81 .ll \n(81u
.in 0
above the contour level
(where data values are greater than the level).
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
.ll \n(34u*1u/4u
.if \n(.l<\n(81 .ll \n(81u
.in 0
below the contour level
(where data values are less than the level).
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
.ll \n(34u*1u/4u
.if \n(.l<\n(81 .ll \n(81u
.in 0
at the level.
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
.ll \n(34u*1u/4u
.if \n(.l<\n(81 .ll \n(81u
.in 0
at the level.
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
.ll \n(34u*1u/4u
.if \n(.l<\n(81 .ll \n(81u
.in 0
at the level.
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBArray
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'AIA'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'AIB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLD'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLL'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wDescription
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAn area identifier for areas
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAn area identifier for the area
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wA color index for contour lines
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wA dash pattern for contour lines
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wA line width for contour lines
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
.nr 82 0
.nr 38 \wAffects\fR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCPCLAM
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCPCLAM
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCPCLDM and CPCLDR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCPCLDM and CPCLDR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCPCLDM and CPCLDR
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.35
.nf
.ll \n(34u
.nr 38 0+\n(80+\n(81+\n(82
.nr 38 \n(.l-\n(.i-\n(38
.nr 38 \n(38/6
.if \n(38<1n .nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 655 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBArray\h'|\n(41u'Description\h'|\n(42u'Affects\fR
.sp.5
.sp.5
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'AIA'\h'|\n(41u'An area identifier for areas\h'|\n(42u'CPCLAM
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'AIB'\h'|\n(41u'An area identifier for the area\h'|\n(42u'CPCLAM
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLC'\h'|\n(41u'A color index for contour lines\h'|\n(42u'CPCLDM and CPCLDR
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLD'\h'|\n(41u'A dash pattern for contour lines\h'|\n(42u'CPCLDM and CPCLDR
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLL'\h'|\n(41u'A line width for contour lines\h'|\n(42u'CPCLDM and CPCLDR
.ne \n(e|u+\n(.Vu
.if (\n(e|+\n(#^-1v)>\n(#- .nr #- +(\n(e|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-33
.in 0
.*i "continued on next page"
\s10~~~~(continued on next page)\s12
.in 0
.in +.5i
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
.rm 80 81 82
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/4u
.if \n(.l<\n(81 .ll \n(81u
.in 0
level.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBArray
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLT'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wDescription
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wA use flag for the contour level,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \windicating whether or not con-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wtour lines are to be drawn and
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wwhether or not the lines are to
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wbe labeled.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wA color index for labels at the
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wA label for the level.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 82 0
.nr 38 \wAffects\fR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCPCLDM, CPCLDR,
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCPLBAM, and CPLBDR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCPLBDR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCPLBAM and CPLBDR
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.35
.nf
.ll \n(34u
.nr 38 0+\n(80+\n(81+\n(82
.nr 38 \n(.l-\n(.i-\n(38
.nr 38 \n(38/6
.if \n(38<1n .nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 680 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBArray\h'|\n(41u'Description\h'|\n(42u'Affects\fR
.sp.5
.sp.5
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLU'\h'|\n(41u'A use flag for the contour level,\h'|\n(42u'CPCLDM, CPCLDR,
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'indicating whether or not con-\h'|\n(42u'CPLBAM, and CPLBDR
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'tour lines are to be drawn and\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'whether or not the lines are to\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'be labeled.\h'|\n(42u'
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLC'\h'|\n(41u'A color index for labels at the\h'|\n(42u'CPLBDR
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLT'\h'|\n(41u'A label for the level.\h'|\n(42u'CPLBAM and CPLBDR
.sp .5
.fc
.nr T. 1
.T# 1
.35
.rm a+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-19
.sp
The contents of these internal parameter arrays may be specified completely by CONPACK,
completely by you, or partly by CONPACK and partly by you.  
The internal parameter \&'CLS'
determines whether or not CONPACK is to select contour 
levels and associated quantities
and, if so, in what manner.  The default value of \&'CLS' indicates that
CONPACK is to select about 16 contour levels at multiples of some nice
interval, which CONPACK chooses. 
.sp
By default, then, the call to CPRECT or
CPSPRS sets \&'NCL' to zero.
During the first subsequent call that requires contour
levels to have been chosen, they are chosen.  The associated internal
parameter
elements are set to indicate which contour lines should be labeled, what
character strings should be used as labels, which lines should be put in
an area map by a call to CPCLAM, and what area identifiers should be used for
areas above and below these lines.
.sp
Other values of \&'CLS' may be used to change the way in which CONPACK chooses
contour levels.  See the detailed descriptions of \&'CLS' and of the 
parameters \&'CIS', \&'CIT', \&'CIU', \&'CMN', \&'CMX', \&'LIS', \&'LIT', and \&'LIU'.
.sp
You may choose to set all of the contour levels and associated quantities.
For example, suppose that you want to draw labeled solid lines for
each of the values .1, .2, .3, ..., .9 and unlabeled dashed lines for each
of the values .05, .15, .25, ... .95.  The following code, inserted before
the call to CPRECT or CPSPRS, will set the required internal
parameters:
.sF
.sp
    CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION',0)
    CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',19)
    DO 101 I=1,19
      CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)
      CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',REAL(I)/20.)
      IF (MOD(I,2).EQ.1) THEN
        CALL CPSETI ('CLU - CONTOUR LEVEL USE',1)
        CALL CPSETI ('CLD - CONTOUR LINE DASH PATTERN',21845)
      ELSE
        CALL CPSETI ('CLU - CONTOUR LEVEL USE',3)
        CALL CPSETI ('CLD - CONTOUR LINE DASH PATTERN',65535)
      END IF
101 CONTINUE
.eF
.sp
.ne 3
In the above code, 
.BL
.LI
\&'CLS' is set to zero
to suspend the selection of contour levels
by CONPACK itself.  
.LI
\&'NCL' is set to specify how many contour levels are to
be defined.  
.LI
In a loop on I from 1 to 19, \&'PAI' is set to tell CONPACK
which element of each internal parameter array is to be set.
.LI
The Ith element of \&'CLV'
is set to REAL(I)/20., which, for each I, gives one of the desired contour
levels.
.LI
The Ith element of \&'CLU' is set to a 1 if just the line is to be
drawn or to a 3 if both the line and the labels for the line are to be drawn.
.LI
The Ith element of \&'CLD' is set to 21845 (octal 52525) if a dashed line
is to be used or to 65535 (octal 177777) if a solid line is to be used.
.LE
.sp
Note that \&'NCL' must be set prior to setting any element of \&'CLV' or the
associated arrays.  Note also that, when an element of \&'CLV' is set, all of
the associated elements of the associated arrays receive a default value.
(In fact, the default element of \&'CLU' is 1, and the default element of \&'CLD'
is a pattern specifying a solid line, so two of the calls in the code above
are redundant.)
.sp
In some cases, you may want to let CONPACK choose a set of contour
levels and then either add other levels of interest, modify elements of the
associated internal parameter arrays, or both.  Suppose, for example, that 
you want
to have CONPACK pick the levels, that contour lines at positive
levels are to be drawn in red, that contour lines at negative levels are
to be drawn in blue, and that contour lines at the zero level are to be
drawn in white.  The following code, inserted after the call to CPRECT or
CPSPRS, would do the job:
.sp
.sF
.nf
\&      ...calls to define color indices IBLU, IRED, and IWHI ...

    CALL CPPKCL (...)
    CALL CPGETI ('NCL - NUMBER OF CONTOUR LINES',NOCL)
    DO 101 I=1,NOCL
      CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)
      CALL CPGETR ('CLV - CONTOUR LEVEL VALUE',CLEV)
      IF (CLEV.LT.0.) THEN
        CALL CPSETI ('CLC - CONTOUR LINE COLOR INDEX',IBLU)
      ELSE IF (CLEV.GT.0.) THEN
        CALL CPSETI ('CLC - CONTOUR LINE COLOR INDEX',IRED)
      ELSE
        CALL CPSETI ('CLC - CONTOUR LINE COLOR INDEX',IWHI)
      END IF
101 CONTINUE
.eF
.sp
.ne 3
In the code above,
.BL
.LI
The subroutine CPPKCL is called to force CONPACK to pick a
set of contour levels.
.LI
The value of \&'NCL' that CONPACK chose is retrieved,
and a loop is run from 1 to that value.
.LI
On each pass through the loop, the
internal parameter array index \&'PAI' is set to tell CONPACK what element of the
internal parameter arrays is being accessed. 
.LI
One of the contour levels chosen
is retrieved to the variable CLEV.  
.ne 3
.LI
Depending on the value of CLEV, the
associated element of the internal parameter array that specifies the color index of
the contour lines at that level is set to produce a blue line, a red line, or
a white line.
.LE
.L2 "The CONPACK Coordinate System"
The mapping of a contour plot onto the plotter frame depends on two things:
.AL
.LI
The X and Y coordinates used by CONPACK to describe the plot.
.LI
The definitions of the current viewport and window, as specified by
a call to the SPPS subroutine SET or by calls to GKS subroutines.
.LE
.sp
These two items are discussed in detail below.
.L3 "\fBX and Y Coordinates\fR"
To describe contour lines and other objects on the contour plot,
CONPACK (by default) generates X coordinates in the same range as the first subscript of
the array of data being contoured and Y coordinates in the same range as the
second subscript of that array
to describe contour lines and other objects on the contour plot.
Thus, X coordinates range from 1. to REAL(\fIm\fR)
and Y coordinates from 1. to REAL(\fIn\fR), where \fIm\fR and \fIn\fR are the dimensions of the data array.  (The internal parameters \&'ZDM' and \&'ZDN' 
have the values \fIm\fR and \fIn\fR,
respectively, after the call to CPRECT or CPSPRS.)  
.sp
Thus, the lower left
corner of the plot is at (1.,1.), the upper right corner of the plot is at
(REAL(\fIm\fR), REAL(\fIn\fR)), and, assuming the array to be indexed by (I,J), the center
of the grid box bounded by I = 3, I = 4, J = 6, and J = 7 is (3.5,6.5).
.sp
You can use
the internal parameters \&'XC1', \&'XCM', \&'YC1', and \&'YCN' to cause X and Y
coordinates to be generated in arbitrary user ranges.  For example, you could
set \&'XC1' = -2., \&'XCM' = 2., \&'YC1' = -1., 
and \&'YCN' = 1. to generate X coordinates
between -2. (at I = 1) and 2. (at I = M), 
and Y coordinates between -1. (at J = 1) 
and 1. (at J = N).  Similarly, you could set \&'XC1' = LON1, \&'XCM' = LON2,
\&'YC1' = LAT1, and \&'YCN' = LAT2 to generate 
X coordinates between longitudes
LON1, and LON2 and Y coordinates between latitudes LAT1 and LAT2.
.sp
If you give the internal parameter \&'MAP' a non-zero value, each pair of X and Y
coordinates is mapped, prior to use, by a statement of the form
.sF
.>>
CALL CPMPXY (IMAP,XINP,YINP,XOTP,YOTP)
.<<
.eF
IMAP is the non-zero value of \&'MAP', XINP and YINP are the unmapped (input)
coordinates and XOTP and YOTP are the mapped (output) coordinates.
.L3 "Defining the Current Viewport and Window by Calling SET"
The window, in the coordinate space in which
X and Y coordinates generated by CONPACK have meaning,
and the viewport, on the plotter frame, may be defined by
calling the SPPS routine SET or by calling GKS routines;
the former is described below:
.sp
A call to the SPPS subroutine SET has the form
.>>
.sF
CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
.eF
.<<
All arguments are real
except for LNLG, which is an integer.  The first four
arguments must all be between 0. and 1., inclusive; 
they define a rectangular
area on the plotter frame known as the viewport.  The next four arguments
define a rectangular area in user coordinate space known as the window.
The final argument indicates whether the mapping of user coordinates into the
viewport is to be linear or logarithmic in X and Y.  For further details, see 
the SPPS chapter in the \fINCAR Graphics User's Guide,\fR
Version 2.00.
.sp
By default, CONPACK (specifically, one of the two subroutines CPRECT or CPSPRS)
calls SET.  By setting the internal parameter \&'SET' to zero, you can prevent CONPACK
from doing this; in that case, you must issue the call yourself or depend on
some other utility (such as EZMAP) to do it.
.sp
If CONPACK calls SET, it always uses LNLG = 1, requesting a linear-linear
mapping from the window to the viewport, and it positions the viewport and
window as follows:  The viewport is positioned as specified by the current
values of the internal parameters \&'VPL', \&'VPR', \&'VPB', \&'VPT', and \&'VPS'.  The 
first four of these specify the position of a viewport area, in which the
viewport is to be centered and made as large as possible. 
The internal parameter \&'VPS' determines
how the shape of the viewport is to be chosen.  
.sp
By default, the window is
defined by the values XAT1, XATM, YAT1, and YATN, where 
.BL
.LI
XAT1 is the X coordinate corresponding to I = 1,
.LI
XATM is the X coordinate corresponding to I = M,
.LI
YAT1 is the Y coordinate corresponding to J = 1, and
.LI
YATN is the Y coordinate corresponding to J = N.
.LE
.sp
The internal parameters \&'WDL', \&'WDR', \&'WDB', and
\&'WDT' may be used to override this default behavior
and specify the exact
values to be used in the SET call to define the window.
Overriding the defaults is principally
useful when you give the internal parameter \&'MAP' a non-zero value to specify that X
and Y coordinates are to be mapped by calling the subroutine CPMPXY.
.sp
Note that, as long as the internal parameter \&'MAP' is zero, a SET
call done by CONPACK will define the window 
in such a way as to be consistent with the ranges
of the X and Y coordinates it generates.
If \&'MAP' is set non-zero, then \&'XC1', \&'XCM', \&'YC1', \&'YCN'
will probably have to be set by you to cause the unmapped
X and Y coordinates to be generated in the ranges expected by
CPMPXY.
The call to SET must specify window limits commensurate with the
mapped X and Y coordinates.  If CONPACK is to perform the call,
then you will probably have to set \&'WDL', \&'WDR', \&'WDB', and \&'WDT'
to accomplish this.
.L3 "The Use of CPMPXY"
The default version of CPMPXY performs the following mappings:
.BL
.ne 6
.LI
If IMAP = 1, XINP and YINP are assumed to be the longitude and the
latitude, in degrees, of a point on the globe.  ('XC1', \&'XCM', \&'YC1',
and \&'YCN' will need to have been set to generate the proper values.)
It is further assumed that the utility EZMAP has been initialized.  The
EZMAP subroutine MAPTRN is called to obtain XOTP and YOTP, which are the
coordinates of the projected point.
.LI
If IMAP = 2, XINP and YINP are assumed to represent rho and theta, in
polar coordinates.  (Again, \&'XC1', \&'XCM', \&'YC1', and \&'YCN' will need to
have been set to generate the proper values.)  XOTP and YOTP are set
by the statements
.>>
.sF
XOTP=XINP*COS (.017453292519943*YINP)
YOTP=XINP*SIN (.017453292519943*YINP)
.eF
.<<
.LI
If IMAP = any other non-zero value, XOTP = XINP and YOTP = YINP.
.LE
.sp
Thus, if the data array being contoured is such that longitude is a linear
function of its first subscript and latitude a linear function of its second
subscript, then to map CONPACK output onto an EZMAP background, you only
need to do the following:
.AL
.LI
Initialize EZMAP by setting values of its internal parameters and then
calling MAPINT or MAPDRW.
This will cause SET to be called.
.LI
Set \&'MAP' to 1.
.LI
Set \&'XC1' and \&'XCM' to the longitudes at
I = 1 and I = M.
.LI
Set \&'YC1' and \&'YCN' to the latitudes at J = 1 and J = N.
.LI
Set \&'SET' to 0.
.LE
.sp
Similarly, if the data array being contoured is such that rho is a linear
function of its first subscript and theta a linear function of its second
subscript, then to map 
.hw CONPACK
CONPACK output onto a polar coordinate background
(which you would have to draw for yourself), you only need to 
.AL
.LI
Set \&'MAP' to 2.
.LI
Set \&'XC1' and \&'XCM' to the values of rho at I = 1 and I = M.
.LI
Set \&'YC1' and \&'YCN' to the values of theta at J = 1 and J = M.
.LI
Set \&'SET' to 0 and do an appropriate call to SET, or leave
\&'SET' non-zero and set \&'WDL', \&'WDR', \&'WDB', and \&'WDT'
appropriately.
.LE
.sp
You can supply your own version of
the subroutine CPMPXY to obtain other desired mappings.
If the CONPACK subroutines are loaded from a binary library, this can
usually be done by just compiling your own version of the subroutine, so that
it replaces the one from the library.
.L2 "Special-value Parameter and Special-value Areas"
If the internal parameter \&'SPV' is non-zero, 
it specifies a \fIspecial value\fR.  This value
may be used in the data array being contoured to indicate a missing or
unreliable data point.  No contours will be drawn in any box of the grid with
such a special value at one or more of its four corners.
.sp
.ne 2
The union of all grid boxes having a special value at one or more of the four
corners constitutes a set of \fIspecial-value areas\fR.
The subroutine CPCLAM will
add the edges of such areas to the area map.  The subroutines CPCLDM and CPCLDR
may be made to draw the edges of such areas (by giving a non-zero value to
element "-2" of the internal parameter array \&'CLU').
.L2 "Out-of-range Parameter and Out-of-range Areas"
If the internal parameter \&'ORV' is non-zero, 
it specifies an \fIout-of-range value\fR.
This is
only of use when the internal parameter \&'MAP' is non-zero, specifying that coordinates
are to be mapped by calling the subroutine CPMPXY.  
You can set the X coordinate returned
by CPMPXY equal to \&'ORV' to indicate that the mapped point is
outside the range in which the mapping is defined.
.sp
A possible value for \&'ORV', if it is to be set non-zero, is 1.E12, which has
historically been returned by the EZMAP subroutine MAPTRN to indicate a point
that is outside the area depicted by a given map projection.
.sp
.ne 3
The union of all points for which CPMPXY returns the out-of-range value
constitutes a set of \fIout-of-range areas\fR.  Contour lines cannot be
traced in
such areas. 
A binary-halving technique is
used to extend contour lines to the very edge of such areas.  The subroutine
CPCLAM will attempt to generate and add to the area map a set of edges for
such areas.  Also, the subroutines CPCLDM and CPCLDR may be made to attempt to draw
the edges of such areas (by giving a non-zero value to element "-3" of the
internal parameter array \&'CLU').  With the Version 3.00 release of CONPACK,
the edges can only be traced successfully
if they are defined by continuous curves having continuous first derivatives
and no points of inflection.  Out-of-range areas generated by the EZMAP
subroutine MAPTRN obey these restrictions.
.sp
When contour lines are traced, 
if two consecutive points are out-of-range,
the entire line segment connecting them is assumed to be out-of-range.
Similarly, if both points are in range,
the line segment is assumed to be in range.
If the detail of the out-of-range
areas is small enough, this assumption 
may cause errors.  Giving 
the internal
parameter \&'PIC' a non-zero value will 
cause more points to be examined along
each such line segment, perhaps curing the problem.  For similar reasons, the
algorithms used to trace the edge of the grid,
the edges of special-value
areas, and the edges of out-of-range areas may fail.  Giving the 
internal parameter
\&'PIE' a non-zero value will cause these algorithms to use a finer grid, perhaps curing the problem.
.L2 "2-D Smoothing of Contour Lines"
You can smooth contour lines by assigning a
non-zero value to the internal parameter \&'T2D',
which uses cubic splines under tension.
The subroutines CPCLAM, CPCLDM, and CPCLDR (all of which generate contour lines)
and the internal subroutine that positions labels along the lines using
either the penalty scheme or the regular scheme are affected by \&'T2D'.
(The penalty and regular schemes are discussed later in this section
of the chapter.)
.sp
The absolute value of \&'T2D'
specifies the tension to be used; values near zero (.001, for example) yield
approximately cubic splines (which may give very loopy curves), 
and large
values (50., for example) yield nearly polygonal curves.
.sp
Since each contour line is smoothed separately, there is no way
to ensure that smoothing will not cause adjacent contour lines to cross
each other. You must experiment with the tension  
(by using different values for \&'T2D')
to reduce the probability
of line-crossing to a minimum.  A reasonable value to start with is 2.5.
.sp
If \&'T2D' is negative, smoothing is 
done prior to any coordinate mapping (if any) implied by the
setting of the internal parameter \&'MAP'.
If \&'T2D' is positive,
smoothing is done after the mapping.
.sp
.ne 3
The internal parameter \&'PIC' determines how many points are to be interpolated between
each pair of points defining the contour line, before smoothing.  If 
you assign \&'PIC'
a non-zero value when 2-D smoothing is done, the effect is
to constrain the smoothed curves to more closely approximate the original
polygonal lines.
.sp
The internal parameter \&'SSL' (smoothed segment length) specifies the 
distance between points used to draw the
smoothed contour lines.  It is expressed as a fraction of the width of the
window in the coordinate system in which the smoothing is being done.
.L2 "3-D Smoothing of Contour Lines"
The subroutine CPSPRS fits a smooth surface to a sparse array of data (using
.hw bicubic
bicubic splines under tension) and then samples that surface to get a dense
array of data, which is returned to you for use in subsequent
calls to produce the contour plot. 
This is quite expensive in terms of
computer time and space used, but it does produce very smooth contour lines
that are guaranteed not to cross each other.
.sp
The data in the dense array returned by CPSPRS may have a larger range than
the original data in the sparse array \(em the lows may be lower and the highs
higher.
If the data represent a physical quantity and its value must fall
inside a fixed range, the use of CPSPRS may be inappropriate.
In this case,
you should either perform your own interpolation to a dense grid or 
modify the contents of the dense array before continuing to draw the contour
plot.  If the latter is done, CPRECT must be the first subroutine called after
the array is modified.
.L2 "Labels"
Three 
different types of labels may be written by a call to the subroutine
CPLBDR:  an informational label, high and low labels, and contour-line labels.
Contour-line labels are written only if ABS('LLP') is greater than 1.
You can add
boxes surrounding these
labels to an area map by calling the subroutine CPLBAM.
Doing this
will prevent contour lines drawn by a subsequent call
to the subroutine CPCLDM from passing through the labels and prevent the label
boxes from being filled or colored by a subsequent call to the subroutine ARSCAM
(in the utility AREAS).
.sp
When a constant field is detected by the initial call to CPRECT or CPSPRS, a
fourth type of label may
be written by CONPACK subroutines.
In this case, a
call to CPLBDR will write a constant-field label, warning of the situation,
in place of the labels it would normally write.
A call to CPLBAM will add
the label box for the constant-field label to the area map, instead of
the label boxes for the other labels.  Calls to CPCLDR and
CPCLDM, which would
normally draw contour lines, will write the constant-field label instead.
.sp
.ne 3
You can control the appearance of all these labels by setting
internal
parameters, as listed here:
.sp
.ne 11
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
.ll 3.2in
.if \n(.l<\n(81 .ll \n(81u
.in 0
\&'ILA'  \&'ILB'  \&'ILC'  \&'ILL'  \&'ILP'  \&'ILS'  \&'ILT' 
\&'ILW'  \&'ILX'  \&'ILY'
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
.ll 3.2in
.if \n(.l<\n(81 .ll \n(81u
.in 0
\&'CFA'  \&'CFB'  \&'CFC'  \&'CFL'  \&'CFP'  \&'CFS'  
\&'CFT'  \&'CFW'  \&'CFX'  \&'CFY'
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
.ll 3.2in
.if \n(.l<\n(81 .ll \n(81u
.in 0
\&'HIC'  \&'HIT'  \&'HLA'  \&'HLB'  \&'HLC'  \&'HLL'  \&'HLO' 
\&'HLS'  \&'HLT'  \&'HLW'  \&'HLX'  \&'HLY'  \&'LOC'  \&'LOT'
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
.ll 3.2in
.if \n(.l<\n(81 .ll \n(81u
.in 0
\&'LLA'  \&'LLB'  \&'LLC'  \&'LLL'  \&'LLO'  \&'LLS'  \&'LLT'
\&'LLW'
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBType of Label
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wInformational
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wConstant-field
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wHigh and low
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wContour-line
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 1.25in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wAffected by\fR
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
.nr 38 3.2in
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
.if t .if \n(TW>\n(.li .tm Table at line 1178 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBType of Label\h'|\n(41u'Affected by\fR
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Informational\h'|\n(41u'
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
\&\h'|\n(40u'Constant-field\h'|\n(41u'
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
\&\h'|\n(40u'High and low\h'|\n(41u'
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
\&\h'|\n(40u'Contour-line\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-23
.sp
In the above internal parameter names,
the last letter has the following meanings:
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
.nr 80 0
.nr 38 \w\fBSuffix
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wA
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wB
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wC
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wS
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wW
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wX
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wY
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wO
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wStands for\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAngle
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wBox flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wColor index
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine width
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSize of characters
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wText of label
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wWhite space width
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wX coordinate
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wY coordinate
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wOverlap in \&'HLO' or Orientation in \&'LLO'
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 1197 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBSuffix\h'|\n(41u'Stands for\fR
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'A\h'|\n(41u'Angle
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'B\h'|\n(41u'Box flag
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'C\h'|\n(41u'Color index
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'L\h'|\n(41u'Line width
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'S\h'|\n(41u'Size of characters
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'T\h'|\n(41u'Text of label
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'W\h'|\n(41u'White space width
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'X\h'|\n(41u'X coordinate
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Y\h'|\n(41u'Y coordinate
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'O\h'|\n(41u'Overlap in \&'HLO' or Orientation in \&'LLO'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-14
.sp
All labels are written by means of calls to the character-plotting subroutine
PLCHHQ in the utility PLOTCHAR.
The internal parameters affect the labels as follows:
.BL
.LI
The angle, in degrees, at which a label is
written is determined by the value of the internal parameter \&'\fIxx\fRA' (and, if it is a
contour-line label, by the value of the internal parameter \&'LLO').  
.LI
The box flag \&'\fIxx\fRB'
determines whether or not, prior to writing the label, a box surrounding it
is filled, and whether or not, after writing the label, the edge of the box
is drawn.  
.LI
If the box is filled, it is done using the color index specified
by the internal parameter \&'LBC'.
.LI
If the edge of the box is drawn, it is done using the
color index, if any, chosen for the label itself, which is determined by the
value of the internal parameter \&'\fIxx\fRC'.
.LI
The line width to be used in drawing the box
is determined by the value of the internal parameter \&'\fIxx\fRL'.
.LI
The size (width) of the
characters is determined by the value of the internal parameter \&'\fIxx\fRS'.
.LI
The text of
the label is determined by the value of the internal parameter \&'\fIxx\fRT'. 
Usually, this
string may contain embedded substrings of the form \&'$\fIxxx\fR$', which are to be
replaced by the value of the quantity specified by the three-character
mnemonic \&'\fIxxx\fR'. 
.LI
The width of the white space to be left around the label
(which defines the dimensions of the box around it) is determined by the
value of the internal parameter \&'\fIxx\fRW'.
.LE
.L3 "Positioning Labels on Contour Lines"
CONPACK provides three different ways to position labels on contour lines.
Set
the internal parameter \&'LLP' to choose one of the three ways, 
which are described below:
.AL
.LI
When ABS('LLP') = 1, contour lines are labeled by CPCLDR or CPCLDM,
which call the utility DASHCHAR with a dash pattern.
The dash pattern includes the desired
label, so that the label will appear at intervals along the line.  You 
can call CPLBAM, 
but it will not add label boxes for contour line labels to the area
map; because of this, there is no way to keep other contour lines from
passing through the labels.  This is the fastest choice, but the labels are
poorly positioned \(em they may overlap each other, for example.
Also, there is no
way (short of modifying the utility DASHCHAR) to force all the labels
to be written at the same angle.
.LI
When ABS('LLP') = 2, a \fIregular scheme\fR is used to position the contour line
labels.  The \fIn\fRth label on each labeled contour line will be at a distance
\&'RC1' + \&'RC2' * (\fIn\fR-1) + \&'RC3' * R\fIn\fR 
units (in the fractional coordinate system) from
the beginning of the line, where \&'RC1', \&'RC2', and \&'RC3' are internal
parameters, and R\fIn\fR is a random number between -1 and 1.  Labels that would
overlap the edge of the viewport or each other are omitted.  The contour line
labels are drawn by a call to the label-drawing subroutine CPLBDR.  You 
can call CPLBAM
to add label boxes for contour-line labels to the area map, so
contour lines can be prevented from passing through the labels. 
This scheme is
better than the first one, but the contour-line labels are still not very
well positioned.  It is somewhat more expensive in terms of computer time and
space used.
.LI
When ABS('LLP') = 3, a \fIpenalty scheme\fR (based on one developed by Starley
Thompson and Phil Rasch) is used.  The contour lines are traced in order of
increasing contour level.  At each point along each line, several tests are
performed; if the point fails any of the tests, no label is put there.  At
the remaining points on the line, a penalty function is evaluated.  A label
is placed at the point where the penalty function has the smallest value.
This process is repeated for each line until it results in the placement of
no further labels.  The contour line labels are drawn by a call to the
label-drawing subroutine CPLBDR.
You can call CPLBAM to add label boxes for
contour-line labels to the area map, so contour lines can be prevented from
passing through the labels. 
This scheme gives by far the best results, but it is
also the most expensive in terms of computer time and space used.
.LE
.sp
When \&'LLP' is 2 or 3, smoothing (if any), implied by a non-zero value of
\&'T2D' is suspended during placement of contour line labels.  This saves a
good deal of computer time and space and provides almost as good a set of
labels.  To leave smoothing turned on during placement of contour line
labels, use \&'LLP' = -2 or -3.
.sp
.L3 "\fBPenalty Scheme for Positioning Contour Line Labels\fR"
The penalty scheme is controlled by the ten internal parameters:  \&'PC1', \&'PC2', \&'PC3',
\&'PC4', \&'PC5', \&'PC6', \&'PW1', \&'PW2', \&'PW3', and \&'PW4'.  (The rationale behind
the names is that the first six internal parameters are characterized as 
\fIconstants\fR
and the remaining four as \fIweights\fR for the four terms in the penalty
function.)
.sp
A point \fIP\fR on a contour line will be rejected as the center point of a 
label under any of the following conditions:
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
.ll 4.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
If the point \fIP\fR is too close to the center point of any other label on
the current line, where the meaning of \fItoo close\fR is defined by \&'PC6'.
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
.ll 4.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
If a label at the point \fIP\fR would extend outside the current viewport.
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
.ll 4.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
If a label at the point \fIP\fR would overlap any previous label.
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
.ll 4.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
If the estimated gradient of the 
field being contoured at the point \fIP\fR is
too large, where \fItoo large\fR is defined by 
the value of \&'PC1', and \&'PW1',
the weight of the first term in the penalty function, is non-zero.
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
.ll 4.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
If the estimated number of contour lines crossing a label at the point \fIP\fR
is too large, where \fItoo large\fR is defined by the value of \&'PC2', and
\&'PW2', the weight of the second term in the penalty function, is
non-zero.
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
.ll 4.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
If the cumulative change in direction of the contour line within a
circle covering a label at the point \fIP\fR is too large, where \fItoo large\fR
is defined by the value of \&'PC3', and \&'PW3', the weight of the third
term in the penalty function, is non-zero.
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
.nr 38 \wCondition 1:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCondition 2:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCondition 3:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCondition 4:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCondition 5:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCondition 6:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCondition 7:
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wIf there are fewer than three points on the line.
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
.nr 38 4.7in
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
.if t .if \n(TW>\n(.li .tm Table at line 1345 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'Condition 1:\h'|\n(41u'If there are fewer than three points on the line.
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Condition 2:\h'|\n(41u'
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
\&\h'|\n(40u'Condition 3:\h'|\n(41u'
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
\&\h'|\n(40u'Condition 4:\h'|\n(41u'
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
\&\h'|\n(40u'Condition 5:\h'|\n(41u'
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
\&\h'|\n(40u'Condition 6:\h'|\n(41u'
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
\&\h'|\n(40u'Condition 7:\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-39
.sp
.ne 6
The penalty function computed at each remaining point \fIP\fR has the form
.sp
./".sF
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
.nr 80 0
.nr 38 \wPFUN = PW1 * GRAD / (GRAV+PC1*GRSD)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w~~~+ PW2 * ENCB / PC2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w~~~+ PW3 * CDIR / PC3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w~~~+ PW4 * MIN (1-EXP(-((D(I)-PC4)/PC5)**2)
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wGradient term
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNumber-of-contours term
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wChange-in-direction term
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wOptimum-distance term
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 1358 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'PFUN = PW1 * GRAD / (GRAV+PC1*GRSD)\h'|\n(41u'Gradient term
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'~~~+ PW2 * ENCB / PC2\h'|\n(41u'Number-of-contours term
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'~~~+ PW3 * CDIR / PC3\h'|\n(41u'Change-in-direction term
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'~~~+ PW4 * MIN (1-EXP(-((D(I)-PC4)/PC5)**2)\h'|\n(41u'Optimum-distance term
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
./".eF
.sp
.S 12
The first term of the penalty function becomes larger in high-gradient
regions.  GRAD is the estimated gradient at the point \fIP\fR, GRAV is 
the average
gradient over the whole field being contoured, and GRSD is the standard
deviation of the estimated gradients over the whole field.  The internal
parameter
\&'PC1' specifies how far from the norm gradients are allowed to wander, as a
multiple of the standard deviation.  Condition 5 above implies that, for
points at which the penalty function is computed, either \&'PW1' is zero, or
GRAD is less than or equal to GRAV+PC1*GRSD.
.sp
.ne 2
The second term of the penalty function becomes larger as ENCB increases.
ENCB is the estimated
number of contour bands crossing a label at the point \fIP\fR.  The
internal parameter \&'PC2' specifies the largest number of crossing bands allowed.
Condition 6 implies that, for points at which the penalty function is
computed, either \&'PW2' is zero, or ENCB is less than or equal to \&'PC2'.
.sp
The third term of the penalty function becomes larger as CDIR increases.
CDIR is the cumulative
change in direction of the contour line in a circular region centered at the
point \fIP\fR and with a radius equal to half the larger dimension of the label.
The internal parameter \&'PC3' specifies the largest such cumulative change
allowed, in degrees.  Condition 7 implies that, for points at which the
penalty function is computed, either \&'PW3' is zero, or CDIR is less than or
equal to \&'PC3'.
.sp
The fourth term of the penalty function becomes larger as the distance of the
point \fIP\fR from the centers of all labels previously placed on other contour
lines deviates from an optimum value specified by you.  D(I) represents
the distance to the Ith such label center.  The minimum is taken over all
values of I.  The internal parameter \&'PC4' is the user-specified optimum distance,
specified as a fraction of the width of the current viewport.  If the 
point \fIP\fR
is exactly \&'PC4' units away from some previous label, then MIN(1-EXP(...))
will have the value 0; otherwise, it will be non-zero.  The internal
parameter \&'PC5'
is specified as a fraction of the width of the
current viewport; as its value decreases, the function "1-EXP(...)" develops
a sharper spike at D(I) = \&'PC4'.
.sp
Following are brief descriptions and default values of all
the user-settable internal parameters mentioned above:  
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
.ll \n(34u*1u/3u
.if \n(.l<\n(81 .ll \n(81u
.in 0
a fraction of the width of the viewport.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\&'PC1'=1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC2'=5.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC3'=60.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC4'=.05
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC5'=.15
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC6'=.30
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PW1'=2.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PW2'=0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PW3'=1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PW4'=1.
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wMultiplier of the standard deviation of the gradients.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMaximum number of crossing contour bands.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMaximum cumulative change in direction of the contour line.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wOptimum distance, as a fraction of the width of the viewport.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wFolding distance, as a fraction of the width of the viewport.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMinimum distance between labels on the same contour line, as
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wWeight of the gradient term.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wWeight of the number-of-contours term.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wWeight of the change-in-direction term.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wWeight of the optimum-distance term.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
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
.if t .if \n(TW>\n(.li .tm Table at line 1423 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\&'PC1'=1.\h'|\n(41u'Multiplier of the standard deviation of the gradients.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC2'=5.\h'|\n(41u'Maximum number of crossing contour bands.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC3'=60.\h'|\n(41u'Maximum cumulative change in direction of the contour line.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC4'=.05\h'|\n(41u'Optimum distance, as a fraction of the width of the viewport.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC5'=.15\h'|\n(41u'Folding distance, as a fraction of the width of the viewport.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC6'=.30\h'|\n(41u'Minimum distance between labels on the same contour line, as
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PW1'=2.\h'|\n(41u'Weight of the gradient term.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PW2'=0.\h'|\n(41u'Weight of the number-of-contours term.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PW3'=1.\h'|\n(41u'Weight of the change-in-direction term.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PW4'=1.\h'|\n(41u'Weight of the optimum-distance term.
.fc
.nr T. 1
.T# 1
.35
.rm a+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-16
.L3 "Numeric Labeling Controls"
Numeric labels are used in various ways in the annotation of a CONPACK plot.
Contour line labels created by CONPACK are purely numeric; each gives the
value of the contour level for the line.  The text of the informational label
is specified by the value of the character-string internal parameter \&'ILT', which may
contain substrings like \&'$CIU$', \&'$CMN$', \&'$CMX$', 
\&'$SFU$', \&'$ZMN$', and
\&'$ZMX$', for each of which is to be substituted a string representing the
numeric value of some quantity of interest.  Similarly, the labels for highs
and lows are specified by the character-string 
internal parameters \&'HIT', \&'LOT',
and/or \&'HLT', which may contain the substring \&'$ZDV$', for which is to be
substituted a string representing the numeric value of the high or low.
.sp
Seven CONPACK internal parameters have to do with the generation
of pleasing numeric
labels:  \&'NEL', \&'NET', \&'NEU', \&'NLS', \&'NLZ', \&'NOF', 
and \&'NSD'.
.L2 "Scale Factors"
It is possible to specify a scale factor by which field values are to be
divided before conversion to a character form for display as a numeric label.
.sp
The internal parameter \&'SFS' determines how the scale factor is to be selected.
If you assign \&'SFS' 
a value greater than zero, that value is the desired scale factor.  If
you assign \&'SFS' a value less than or equal to zero, CONPACK is directed to
choose a scale factor to use, in one of five different ways.  You can
retrieve the value of
the internal parameter \&'SFU'; it specifies the scale
factor which has been selected for use.
.sp
You can display
the value of the scale factor as a part of the informational
label.  This is done by embedding the substring \&'$SFU$' in the string that
gives the value of the internal parameter \&'ILT'.
.sp
The default value of \&'SFS' is 1., which essentially specifies that no scale
factor is to be used.
.L2 "Constant-field Detection"
The subroutines CPRECT and CPSPRS check for a field that is essentially
constant.  When such a field is found, the internal parameter \&'CFF' is set non-zero;
otherwise, \&'CFF' is set to zero.
You can retrieve
the value of \&'CFF'.
.sp
When \&'CFF' is non-zero, a call to one of the subroutines CPCLDM or CPCLDR will
not cause any contour lines to be drawn; instead, the constant-field label
will be written.  
You can still draw the edge of the grid, the edges of special-value areas, and
the edges of out-of-range areas.
.sp
Similarly, when \&'CFF' is non-zero, a call to the subroutine CPLBDR will write
the constant-field label instead of the labels that would normally be
written.
A call to the subroutine CPLBAM will put into the area map the label box 
for the
constant-field label instead of the label boxes for the
normal set of labels.
.L2 "Workspace Management"
Many of the subroutines in CONPACK require one or more workspace arrays, some
of type real and some of type integer.  Some subroutines require more workspace
under some conditions than under other conditions.  
For example, when the
contour lines at a given level are traced, an integer workspace is used to
keep track of where those contour lines were, so as to avoid tracing any of
them more than once.  The amount of space required for this is larger for a
complicated contour plot than it is for a simple one.
.sp
The workspace management scheme used in CONPACK is as follows:  You
define one workspace array of type real and another of type integer.  In the
call to CPRECT or CPSPRS, which initializes the drawing of a contour plot,
these arrays appear as arguments (called RWRK and IWRK), together with
arguments specifying their lengths (LRWK and LIWK).  In subsequent calls to
other CONPACK subroutines that require workspaces, the same arrays appear as
arguments, but the lengths do not.  The CONPACK subroutines cooperate in using
these arrays in such a way as not to interfere with one another.  Dynamic
enlargement of one workspace at the expense of another becomes possible and
the probability of running out of space is reduced.
.sp
In general, it is safest not to use the workspace arrays for other purposes
between one call to a CONPACK subroutine and the next (unless the next is to one
of the subroutines CPRECT or CPSPRS, which initialize the workspace pointers).
With this release of CONPACK,
there is only one case in which the contents of the arrays are
assumed to be preserved intact:  If labels are being positioned using either
the regular scheme or the penalty scheme, the list of label positions is
created in the workspace arrays when it is first required and is assumed
untouched thereafter.  
.sp
.ne 3
It is possible to find out how much space has been used in each of the
workspace arrays.  The internal parameters \&'IWU' and \&'RWU' are set to zero by a call to
CPRECT or CPSPRS and are updated thereafter to reflect maximum usage of space
in the arrays.  Thus, you might choose to
give the arrays large dimensions, create a
typical contour plot, retrieve the values of \&'IWU' and \&'RWU' to see how much
space was actually used, and then reduce the dimensions to more reasonable
values.
.sp
Workspace usage by some subroutines cannot be predicted exactly.  Upper bounds
can be computed, but they may be rather large.  For this reason, it may not
be possible to ensure that enough workspace will be available for
a given call.  Therefore, there is an internal parameter, called \&'WSO', which determines
what
to do when a workspace overflow occurs.  The three possibilities are as
follows:
.AL
.LI
Terminate after printing an error message.
.LI
Continue running
after printing an error message.  (This is the default.)
.LI
Continue running without printing anything.
.LE
.ne 5
.sp
Of course, in the latter two cases,
incomplete plots may result.  It is possible to find out whether or not a
workspace overflow has occurred during a given call to a CONPACK subroutine.
Do this by retrieving the values of the internal parameters \&'IWU'
and \&'RWU' and
comparing them with the dimensions of the workspace arrays IWRK and RWRK.
.L3 "Estimating Real and Integer Workspace Usage"
The following information will be of value as you attempt to estimate how much
real and integer workspace will be required by each of the user-callable
subroutines of CONPACK.
.sp
CPCLAM uses real workspace of length NWRC, where NWRC is as defined
below.  It uses integer workspace of length MAX (NWCL,NWES,NWEO), where
NWCL, NWES, and NWEO are as defined below.
.sp
CPCLDM uses real workspace of length 2*'RWM' + NWRC, where NWRC is as
defined below.  It uses integer workspace of length 2*'IWM' + MAX
(NWCL,NWES,NWEO), where NWCL, NWES, and NWEO are as defined below.
.sp
CPCLDR uses real workspace of length NWRC, where NWRC is as defined
below.  It uses integer workspace of length MAX (NWCL,NWES,NWEO), where
NWCL, NWES, and NWEO are as defined below.
.sp
CPGET\fIx\fR (where \fIx\fR stands for C, I, or R) uses no workspace.
.sp
.ne 2
CPLBAM uses real workspace of minimum length 10.  If contour-line labels
are positioned using either the regular scheme or the penalty scheme, and
the list of positions is not defined when CPLBAM is called, then more
real workspace is required, of 
length NWRC + NWL1 + NWL2 + NWGA, where
NWRC, NWL1, NWL2, and NWGA are as defined below.
.sp
CPLBDR uses no real workspace unless contour-line labels are positioned
using either the regular scheme or the penalty scheme and the list of
positions is not defined when CPLBDR is called, in which case more real
workspace is required, of length NWRC + NWL1 + NWL2 + NWGA, where NWRC,
NWL1, NWL2, and NWGA are as defined below.
.sp
CPSPRS uses real workspace of length \s11 100*((NSPV+99)/100)+3*MSPS*NSPS+MAX(MSPS+NSPS+NSPS,4*IZDM),\s12
where NSPV is the number of special values
in the sparse array, MSPS and NSPS are the dimensions of the sparse
array, and IZDM is the first dimension of the dense array.  It also
uses integer workspace of length 100*((NSPV+99)/100), where NSPV is the
number of special values in the sparse array.
.L4 "Definitions for Estimating Workspace"
NWRC = 2*'RWC' if 2D smoothing of contour lines is turned off and
7*'RWC' if 2D smoothing of contour lines is turned on.
.sp
NWCL = 100*((NHCL+99)/100), where NHCL is the number of places where a
contour line crosses a horizontal segment of the grid.  If contour lines
are not traced, then NWCL = 0.
.sp
NWES = 100*((NHES+99)/100), where NHES is the number of horizontal
segments in the edges of the special-value areas.  If these edges are
not traced, then 
.br
NWES = 0.
.sp
NWEO = 100*((NHEO+99)/100)), where NHEO is the number of horizontal
segments in the grid used to trace the edges of the out-of-range areas
that cross such an edge.  If these edges are not traced, then NWEO = 0.
.sp
NWL1 = 100*((4*NLBS+99)/100), where NLBS is the total number of labels
written (the informational label + high/low labels + line labels).
.sp
NWL2 = 100*((4*NIHL+99)/100), where NIHL is the number of informational
labels written (0 or 1), plus the number of high/low labels written.
.sp
NWGA is zero if the penalty scheme for positioning contour labels is not
being used (that is, if ABS('LLP') is not equal to 3) or if both the
weights \&'PW1' and \&'PW2' are zero; otherwise, it has the value \&'RWG'
(1000, by default).  In this space, an array of gradients
which are used in the execution of the penalty scheme is constructed.
.sp
Subroutines not mentioned above use no workspace, with the following exception:
In the subroutine CPCNRC,
a real workspace array of length 5000, an integer workspace array
of length 2000, and an area map array of length 12000 are defined.
CPCNRC enables you to produce a
complete contour plot with a single call 
(as was done by the subroutine CONREC in
Version 2.00 of NCAR Graphics.)
.L2 "Character-width Multiplier"
The internal parameter \&'CWM' is used as a multiplier for
all character widths and similar quantities.  This makes it
possible to scale all such quantities up and down
simultaneously.
.L2 "Searching for Highs and Lows"
The gridpoint (I,J) is defined to be the position of a high if and only if
.AL
.LI
I is neither 1 nor \&'ZDM'.
.LI
J is neither 1 nor \&'ZDN'.
.LI
ZDAT(I,J) is not a special value.
.LI
ZDAT(I,J) is greater than ZDAT(K,L) for all values of K and L such
that
.LE
.in +.5
.BL
.LI
K is greater than or equal to MAX(1,I-NX) and less than or
equal to MIN('ZDM',I+NX).
.LI
L is greater than or equal to MAX(1,J-NY) and less than or
equal to MIN('ZDN',J+NY).
.LI
Either K is not equal to I, or L is not equal to J.
.LI
ZDAT(K,L) is not a special value.
.LE
.in -.5
.sp
The values of NX and
NY are as specified by the values of the internal 
parameters \&'HLX' and \&'HLY'.
.sp
A similar definition is used for the position of a low.
.L2 "GKS Considerations"
Certain assumptions are made by CONPACK about the state of GKS, as follows:
.AL
.LI
Like all the utilities in NCAR Graphics, CONPACK assumes
that GKS has been opened and that the desired workstations have been
opened and activated.  The statement
.>>
.sF
CALL OPNGKS
.<<
.eF
calls the SPPS routine OPNGKS, the GKS equivalent of which is
.>>
.sF
CALL GOPKS (6,0)
CALL GOPWK (1,2,1)
CALL GACWK (1)
.<<
.eF
creating a single metafile workstation associated with Fortran unit 2.
.sp
Similarly, at the end of your program, the workstations must be
deactivated and closed and then GKS must be closed.  The statement
.ne 4
.>>
.sF
CALL CLSGKS
.<<
.eF
calls the SPPS routine CLSGKS, the GKS equivalent of which is
.>>
.sF
CALL GDAWK (1)
CALL GCLWK (1)
CALL GCLKS
.<<
.eF
.LI
It is assumed by CONPACK that clipping is turned
off.  To do this,
use the statement
.>>
.sF
CALL GSCLIP (0)
.<<
.eF
If this is not done, and if the informational label is drawn in its
default position, which is outside the viewport, it will be clipped
and fail to appear on the plot.
.LI
It is assumed that the aspect source flags for various quantities
are set to "individual."  (The NCAR GKS package does this by default,
but other packages may not.)
To make sure that all the aspect source
flags are set correctly, use the following code:
.>>
.ne 6
.sF
DIMENSION IASF(13)
\&...
DATA IASF / 13*1 /
\&...
CALL GSASF (IASF)
.<<
.eF
.LI
Color fill of label boxes is done by CONPACK using calls to the
GKS routine GFA;
you do color fill of contour bands by calling the same routine.
To get solid fill, rather than hollow fill,
you must call a GKS routine to set the "fill area interior style:"
.>>
.sF
CALL GSFAIS (1)
.<<
.eF
.LI
Color-setting by CONPACK is done by executing calls to the GKS
routines GSPLCI, GSTXCI, and GSFACI, with user-defined color indices
as arguments.
You must have previously defined the association of these color indices with 
colors on
the workstations. This
may be done by 
using calls to the GKS routine GSCR.  The statement
.>>
.sF
CALL GSCR (IW,IC,RC,GC,BC)
.<<
.eF
defines, for workstation IW, color index IC, with RGB components RC, GC,
and BC.  To be consistent with the SPPS routines OPNGKS and CLSGKS, use
IW = 1.  The value of IC may be any non-negative integer.  By default,
color index 0 is associated with the color black, which is defined by
(RC,GC,BC) = (0.,0.,0.) and is used as the background color, while color
index 1 is associated with the color white, which is defined by
(RC,GC,BC) = (1.,1.,1.).
.LE
.L2 "Color-setting Philosophy"
A number of the CONPACK internal parameters specify the color of
some object (like the
informational label) or class of objects (like all contour lines for a given
contour level).  The default value of each such parameter is -1, which states
that the color is to be determined by the current value of one of the GKS
color indices.  The polyline color index is used for lines, the text color
index for labels, and the fill area color index for filling label boxes.
.sp
If the value of the CONPACK color-setting parameter for a given object is
given a value greater than or equal to zero, it specifies the color index of
the color in which the object is to be drawn.  Before any object is drawn,
the values of the GKS color indices affected are saved; after the object is
drawn, the saved values are restored.
.sp
This structure allows the use of a tiered approach to color setting.  If no
color setting whatsoever is done, contour plots are drawn entirely in the
colors specified by the applicable default values of the GKS color indices.
If, on the other hand, prior to calling CONPACK, you define the color index
IC (in a manner described in "GKS Considerations", above) and then use
.>>
.sF
CALL GSPLCI (IC)
.<<
.eF
to change the GKS polyline color index, it will cause
all polylines drawn by CONPACK
to change color.  Similarly, you can use the statement
.>>
.sF
CALL GSTXCI (IC)
.<<
.eF
to change the GKS text color index, which will change the color of
the labels drawn by CONPACK.
You can use the statement
.>>
.sF
CALL GSFACI (IC)
.<<
.eF
to change the GKS fill area color index, which will change the color of
label boxes filled by
CONPACK.
.sp
If, in addition, you give CONPACK color-setting parameters values
greater than or equal to zero, the objects or classes of objects to which
those parameters apply are colored accordingly.  These colors are used in
preference to values preset by calls to GSPLCI, GSTXCI, or GSFACI.
.sp
.ne 3
A final opportunity to set color is provided by the user-supplied versions of
the change subroutines, with names of the form CPCH\fIxx\fR~; calls to GSPLCI, GSTXCI,
and GSFACI may occur in such a routine and take precedence over color setting
by any other means.  Note that, if color is being set for drawing a label,
then you may need to set either or both of the polyline 
color index and the text color index,
depending on whether the labels are being drawn by calls to
the GKS routine GPL (to draw polylines stroking out the characters) or by
calls to the GKS routine GTX (to draw text).  In particular
, you can direct the subroutine
PLCHHQ in the utility PLOTCHAR (which is called by CONPACK to draw labels)
to draw high-quality characters, which are
stroked, medium-quality characters, which are also stroked, or low-quality
characters, which are drawn by GTX.
.L1 "CONPACK Calls"
All of the CONPACK subroutines have names that are six characters
long, beginning with the
letters CP.  The summary table on the next page lists the CONPACK subroutines
in alphabetical order, gives a brief description
of each, and lists the number of the page where the detailed
documentation begins.
.sp
The detailed documentation for the subroutines begins after
the summary table. The subroutines are organized in the
following functional groups:
.sp
Initialization Subroutines
.in+.25i
CPRECT~~CPSPRS
.sp
.in-.25i
Contour Level and Label Selection Subroutines
.in+.25i
CPPKCL~~CPPKLB
.sp
.in-.25i
Action Subroutines
.in+.25i
CPCLDR~~CPCLDM~~CPCLAM~~CPLBDR~~CPLBAM~~CPBACK~~CPDRPL
.br
CPMPXY
.in-.25i
.sp
Transition Subroutines
.in+.25i
CPCNRC~~CPEZCT
.sp
.in-.25i
Change Subroutines
.in+.25i
CPCHCF~~CPCHCL~~CPCHHL~~CPCHIL~~CPCHLL~~
.sp
.in-.25i
Parameter Access Subroutines
.in+.25i
CPSETC~~CPSETI~~CPSETR~~CPGETC~~CPGETI~~CPGETR~~CPRSET
.sp
.in-.25i
.in 0
.ce
\fBAlphabetical Listing of Subroutines\fR
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
.rm 80 81 82
.nr 80 0
.nr 38 \w\fBSubroutine
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPBACK
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPCHCF*
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPCHCL*
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPCHHL*
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPCHIL*
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPCHLL*
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPCLAM
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPCLDM
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPCLDR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPCNRC
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPDRPL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPEZCT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPGETC
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPGETI
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPGETR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPLBAM
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPLBDR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPMPXY*
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPPKCL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPPKLB
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPRECT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPRSET
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPSETC
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPSETI
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPSETR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCPSPRS
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wBrief Description
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDraws a background
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wProvides user control as constant-field message is drawn
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wProvides user control as contour lines are drawn
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wProvides user control as high and low labels are drawn
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wProvides user control as informational label is drawn
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wProvides user control as line labels are drawn
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAdds contour lines to area map
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDraws contour lines masked by existing area map
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDraws contour lines
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSimulates old routine CONREC
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wProvides polyline-drawing for CPCLDM
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSimulates old subroutine EZCNTR (in CONREC)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wGets current value of a parameter of type character
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wGets current value of a parameter of type integer
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wGets current value of a parameter of type real
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAdds label boxes to area map
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDraws labels
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMaps CONPACK output from rectangular
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wcoordinate system to other coordinate system
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPicks a set of contour levels
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPicks a set of labels for labeled contour levels
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInitializes contouring of rectangular array of data
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wResets all parameters to initial default values
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSets value of a parameter of type character
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSets value of a parameter of type integer
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSets value of a parameter of type real
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInterpolates from an array of data on a
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wsparse rectangular grid to an array of data on a 
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdense rectangular grid and initializes contouring 
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wfrom the array on the dense grid
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wPage\fR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-53
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-65
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-66
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-67
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-69
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-70
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-48
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-44
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-42
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-58
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-54
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-63
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-73
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-74
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-74
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-51
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-50
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-55
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-39
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-41
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-34
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-72
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-72
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-72
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-72
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w3-36
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 1919 file man/conpack.l is too wide - \n(TW units
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
.sp .5
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBSubroutine\h'|\n(41u'Brief Description\h'|\n(42u'Page\fR
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPBACK\h'|\n(41u'Draws a background\h'|\n(42u'3-53
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPCHCF*\h'|\n(41u'Provides user control as constant-field message is drawn\h'|\n(42u'3-65
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPCHCL*\h'|\n(41u'Provides user control as contour lines are drawn\h'|\n(42u'3-66
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPCHHL*\h'|\n(41u'Provides user control as high and low labels are drawn\h'|\n(42u'3-67
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPCHIL*\h'|\n(41u'Provides user control as informational label is drawn\h'|\n(42u'3-69
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPCHLL*\h'|\n(41u'Provides user control as line labels are drawn\h'|\n(42u'3-70
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPCLAM\h'|\n(41u'Adds contour lines to area map\h'|\n(42u'3-48
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPCLDM\h'|\n(41u'Draws contour lines masked by existing area map\h'|\n(42u'3-44
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPCLDR\h'|\n(41u'Draws contour lines\h'|\n(42u'3-42
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPCNRC\h'|\n(41u'Simulates old routine CONREC\h'|\n(42u'3-58
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPDRPL\h'|\n(41u'Provides polyline-drawing for CPCLDM\h'|\n(42u'3-54
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPEZCT\h'|\n(41u'Simulates old subroutine EZCNTR (in CONREC)\h'|\n(42u'3-63
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPGETC\h'|\n(41u'Gets current value of a parameter of type character\h'|\n(42u'3-73
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPGETI\h'|\n(41u'Gets current value of a parameter of type integer\h'|\n(42u'3-74
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPGETR\h'|\n(41u'Gets current value of a parameter of type real\h'|\n(42u'3-74
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPLBAM\h'|\n(41u'Adds label boxes to area map\h'|\n(42u'3-51
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPLBDR\h'|\n(41u'Draws labels\h'|\n(42u'3-50
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPMPXY*\h'|\n(41u'Maps CONPACK output from rectangular\h'|\n(42u'3-55
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'coordinate system to other coordinate system\h'|\n(42u'
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPPKCL\h'|\n(41u'Picks a set of contour levels\h'|\n(42u'3-39
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPPKLB\h'|\n(41u'Picks a set of labels for labeled contour levels\h'|\n(42u'3-41
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPRECT\h'|\n(41u'Initializes contouring of rectangular array of data\h'|\n(42u'3-34
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPRSET\h'|\n(41u'Resets all parameters to initial default values\h'|\n(42u'3-72
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPSETC\h'|\n(41u'Sets value of a parameter of type character\h'|\n(42u'3-72
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPSETI\h'|\n(41u'Sets value of a parameter of type integer\h'|\n(42u'3-72
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPSETR\h'|\n(41u'Sets value of a parameter of type real\h'|\n(42u'3-72
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CPSPRS\h'|\n(41u'Interpolates from an array of data on a\h'|\n(42u'3-36
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'sparse rectangular grid to an array of data on a \h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'dense rectangular grid and initializes contouring \h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'from the array on the dense grid\h'|\n(42u'
.sp .5
.fc
.nr T. 1
.T# 1
.in \n(#Iu
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-45
.sp
*These subroutines are called by CONPACK; the default
versions do nothing.  You can replace the default version of any of these subroutines
with your own version of that subroutine.  See the listed pages for
more details.
.L2 "Initialization Subroutines"
.in 0
.ft B           
.S 14
CPRECT
.S 12
.L2 Purpose
CPRECT (CONPACK, rectangular array) initializes the contouring 
of a rectangular array of data.
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
.nr 38 \w\fB\s11CALL CPRECT\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~KZDT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~MZDT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~NZDT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~LRWK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~LIWK)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
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
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wKZDT by \fIn\fR
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wLRWK
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wLIWK
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
.if t .if \n(TW>\n(.li .tm Table at line 1949 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPRECT\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'KZDT by \fIn\fR
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~KZDT,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~MZDT,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~NZDT,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RWRK,\h'|\n(42u'Real array\h'|\n(43u'Input/output\h'|\n(44u'LRWK
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~LRWK,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IWRK,\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'LIWK
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~LIWK)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.in +.5i
.VL 1.2i
.LI "\fBZDAT\fR"
The array of data to be contoured.
Dimensioned KZDT by \fIn\fR, where \fIn\fR is greater than or equal to NZDT.
.LI \fBKZDT\fR
The first dimension of the array ZDAT.
.LI \fBMZDT\fR
The first dimension of the array of data in ZDAT.
MZDT must be less than or equal to KZDT.
.LI \fBNZDT\fR
The second dimension of the array of data in ZDAT.
NZDT must be less than or equal to the declared second dimension of the array
ZDAT.
.LI \fBRWRK\fR
The real work array.
.LI \fBLRWK\fR
The length of RWRK.
.LI \fBIWRK\fR
The integer work array.
.LI \fBLIWK\fR
The length of IWRK.
.LE
.L2 "Usage"
The dimensions of
all the arrays are transferred to variables in common,
so that, in calls to
other CONPACK routines, you can omit those dimensions.
CPRECT initializes the internal
pointers that are used to manage workspace use
and
decides what the ranges of X and Y coordinates used to draw contour lines
and to position labels ought to be.
.sp
.ne3
If CONPACK is to call SET, appropriate
arguments are determined and SET is called; otherwise, GETSET is called to
retrieve the arguments from your call to SET.
.sp
CPRECT discards the list of label positions (if any)
left over from previous calls to CONPACK.
If
contour levels are to be chosen by the utility,
the internal parameter \&'NCL' is set to zero so that the
contour levels will be chosen when required.
CPRECT locates the minimum and maximum values in the
data array and decides whether the data are essentially
constant \(em so nearly constant that no meaningful
contour levels can be chosen.
Numeric-label parameters that depend on the range of values in
the data array are initialized.  Under some conditions, CPRECT chooses
a scale factor.
.in 0
.ft B           
.S 14
CPSPRS
.S 12
.L2 Purpose
CPSPRS (CONPACK, sparse array) interpolates from an array of data on a
\fIsparse\fR rectangular grid to an array of data on a \fIdense\fR
rectangular grid
and then initializes contouring from the array on the dense grid.  The
phrase \fIsparse grid\fR is used here to refer to a grid with
such small dimensions that contour lines drawn directly on it
are composed of long straight segments.  The phrase \fIdense grid\fR is
used to refer to a grid with sufficiently large dimensions to avoid
this problem.  Whether a given grid is sparse or dense is a subjective
matter and depends on how you are using the contour plot.
Thus, CPSPRS functions as a data-smoothing routine.
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
.nr 38 \w\fB\s11CALL CPSPRS\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZSPS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~KSPS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~MSPS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~NSPS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~LRWK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~LIWK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~LZDT)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
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
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wKSPS by \fIn\fR
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wLRWK
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wLIWK
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wLZDT
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
.if t .if \n(TW>\n(.li .tm Table at line 2036 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPSPRS\fR\s12\h'|\n(41u'~~~(ZSPS,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'KSPS by \fIn\fR
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~KSPS,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~MSPS,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~NSPS,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RWRK,\h'|\n(42u'Real array\h'|\n(43u'Input/output\h'|\n(44u'LRWK
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~LRWK,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IWRK,\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'LIWK
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~LIWK,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Output\h'|\n(44u'LZDT
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~LZDT)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-15
.in +.5i
.VL 1.2i
.LI \fBZSPS\fR
The sparse array of data, from which the dense array is
to be generated.
Dimensioned KSPS by \fIn\fR, where \fIn\fR is greater than or equal to
NSPS.
.LI \fBKSPS\fR
The first dimension of the array ZSPS.
.LI \fBMSPS\fR
The first dimension of the sparse array of data in
ZSPS.  MSPS must be less than or equal to KSPS.
.LI \fBNSPS\fR
The second dimension of the sparse array of data
in ZSPS.  NSPS must be less than or equal 
to the declared second dimension of the array ZSPS.
.LI \fBRWRK\fR
The real work array.
.LI \fBLRWK\fR
The length of RWRK.
.LI \fBIWRK\fR
The integer work array.
.LI \fBLIWK\fR
The length of IWRK.
.LI \fBZDAT\fR
The array in which the interpolated dense array of data is to be returned.
You can either supply the dimensions of the
interpolated array or let them 
be determined by CONPACK,
depending on the value of the parameter \&'ZDS'.  Note that, if CONPACK determines the dimensions and if the size of
the dense array is not a product of the size of the sparse array and some
perfect square, the aspect ratio 
(the ratio of the height to the width) 
of the dense grid may be slightly different
from that of the sparse grid.
.LI \fBLZDT\fR
The length of ZDAT.
.LE
.L2 "Usage"
CPSPRS performs the same functions as CPRECT, but, in addition, it
interpolates from a sparse array of data to a dense array of data.
CPSPRS does this by using
the routines BSURF1 and BSURF2, from the utility FITPACK, by Alan K. Cline,
to fit bicubic splines under tension to the sparse array of data
and to compute the dense grid of data that is returned to you.
The tension on the spline surfaces is specified by the parameter \&'T3D'.  By
default, CPSPRS selects the dimensions of the dense array of data; if
desired, you can specify these dimensions by setting the
parameter \&'ZDS' non-zero and the
parameters \&'ZD1', \&'ZDM', and \&'ZDN' to the desired values. 
In either case, once \&'ZD1', \&'ZDM', and \&'ZDN' are set, they should
not be reset by you until the contour plot is complete and a different
contour plot is to be drawn.
.sp
Because the routines BSURF1 and BSURF2 do not have a built-in special-value
feature, if the special-value parameter \&'SPV' is set non-zero and 
the sparse array
contains occurrences of that value,
special action must be taken.  The indices
of the special values in the sparse array are saved in a part of the integer
workspace array; the special values are then replaced by values interpolated
from adjacent grid points and the resulting array is used to obtain the dense
array;  then, the special values in the sparse array are restored and the
corresponding elements of the dense array are also given the special value.
.L2 "Contour Level and Label Selection Subroutine"
.L3 "Strategies for Using CPPKCL and CPPKLB"
The default values of the relevant internal parameters are such that
CONPACK chooses the contour levels to be used, decides which of those levels
are to be labeled, and chooses the labels to be used.  If, before calling
CPRECT or CPSPRS, you set the internal parameter \&'CLS' (Contour Level
Selection) to zero, it declares your intention to pick the contour
levels yourself; you do this by setting the values of the internal
parameters \&'NCL' (Number of Contour Levels) and then, for values of
\&'PAI' (Parameter Array Index) from 1 to \&'NCL', the desired value
of the internal parameters \&'CLV', \&'CLU', \&'AIA', \&'AIB', \&'CLC',
\&'CLD', \&'CLL', \&'LLC', and \&'LLT', all of which are described
under the heading "CONPACK Internal Parameter Descriptions," later 
in this chapter.  You may leave the parameters that 
specify the labels for labeled levels
blank; CONPACK will then choose the labels for you.  In none of these
cases do you need to call the routines described in this section:  CPPKCL
(CONPACK, Pick Contour Levels), and CPPKLB (CONPACK, Pick Labels).  They
will be called for you as required, by one of the action subroutines
(CPCLDR, CPCLAM, CPLBAM, or CPLBDR).  These are described in
"Action Subroutines," which follows the
section you are now reading.
.sp
Sometimes, however, you will want to
call one or both of these routines, so that, after each
call, 
you can modify what the routine called has done in order to produce
desired effects.
For example, after CPPKCL is called,
you might set elements
in parameter arrays that will cause the labeled contour lines to be solid
and the unlabeled contour lines to be dashed.  Or, after
CPPKLB is called, 
you might check for the string "0" as a label and change it
to "0.0".
.L4 "Calling Order"
CPPKCL and/or CPPKLB must be called after one of the initialization routines
(CPRECT or CPSPRS)
and before a call to one of the action routines
(CPCLDR, CPCLDM, CPCLAM, CPLBAM, or CPLBDR).
If you call one of the action routines
before calling CPPKCL and/or CPPKLB, it will call them for you to
generate contour levels and labels,
and your direct calls to CPPKCL and/or CPPKLB will do nothing.
.sp
You may not need to call both of these subroutines.
If you do not wish to change the contour levels picked, but only the labels,
then you only need to call CPPKLB; it will call CPPKCL for you.
.sp
If you want to change
both the contour levels (after calling CPPKCL)
and the labels (after calling CPPKLB),
you must use that order:  call CPPKCL before calling CPPKLB.
If you call CPPKLB first, it will call CPPKCL to pick contour levels;
your call to CPPKCL will then do nothing.
.in 0
.ft B           
.S 14
CPPKCL
.S 12
.L2 Purpose
CPPKCL (CONPACK, pick contour levels)
picks a set of contour levels.
.sp
.in 0
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
.nr 34 \n(.lu
.eo
.am 84
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 84
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 84
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\s11CALL CPPKCL\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IWRK)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 \n(a-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(b-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(c-
.if \n(84<\n(38 .nr 84 \n(38
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
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 2180 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPPKCL\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RWRK,\h'|\n(42u'Real array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IWRK)\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.c+
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-14
.in +.5i
.sp
All three arguments are arrays used in the last call to CPRECT or CPSPRS, the
contents of which must not have been changed since that call.
.VL 1.2i
.LI \fBZDAT\fR
The data array.
.LI \fBRWRK\fR
The real workspace array.
.LI \fBIWRK\fR
The integer workspace array.
.LE
.L2 "Usage"
Normally, CPPKCL is called by CONPACK when the contour levels are needed.
However, you can call CPPKCL directly
(after the initialization call to CPRECT or CPSPRS)
if you want
to modify the resulting parameter arrays specifying contour
levels and associated quantities in some way.
.sp
If the contour-level-selection parameter \&'CLS' is zero, CPPKCL does nothing.
If the constant-field-flag \&'CFF' is non-zero, indicating that, on the last
call to CPRECT or CPSPRS, the data were found to be essentially constant,
CPPKCL does nothing.
.sp
If neither of the conditions mentioned in the last paragraph arises, contour
levels are picked.  For a detailed description of the ways
in which this might be done, see the description of the parameter \&'CLS'.  In
any case, 
the parameter \&'NCL' is set to the number of contour levels
picked and elements 1
through \&'NCL' of the parameter array \&'CLV' 
(contour level values) are set to the levels themselves.
The Ith elements of the associated parameter arrays are set as follows:
.sp
.ne 8
.sF
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
.nr 80 0
.nr 38 \w\&'CLU' = 1 or 3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'AIA' = I+1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'AIB' = I
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLT' = \&' \&' (single blank)
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLD' = \&'$$$$$$$$$$$$$$$$\&'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLC' = -1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLC' = -1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLL' = 0.
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wContour level use flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wArea identifier above the level
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wArea identifier below the level
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour line label text
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour line dash pattern
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour line color
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour line label color
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour line line width
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 2229 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\&'CLU' = 1 or 3\h'|\n(41u'Contour level use flag
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'AIA' = I+1\h'|\n(41u'Area identifier above the level
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'AIB' = I\h'|\n(41u'Area identifier below the level
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLT' = \&' \&' (single blank)\h'|\n(41u'Contour line label text
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLD' = \&'$$$$$$$$$$$$$$$$\&'\h'|\n(41u'Contour line dash pattern
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLC' = -1\h'|\n(41u'Contour line color
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLC' = -1\h'|\n(41u'Contour line label color
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLL' = 0.\h'|\n(41u'Contour line line width
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.sp
.eF
.S 12
Thus, after CPPKCL is called, the situation for each contour level is 
as follows:
.BL
.LI
Contour lines at the level are to be generated by calls to CPCLDM or CPLBDR.
.LI
The lines will be labeled if \&'CLU' = 3, but not labeled if \&'CLU' = 1.  
.LI
If CPCLAM is
called, the lines will be added to the area map.
.LI
The identifier used for
areas "above" the lines will be the contour level index plus
one, and the
identifier used for areas "below" the lines will be the contour level index.
.LI
The text of the label associated with the level is, as yet, unspecified (see
the description of the subroutine CPPKLB, below).  
.LI
The dash pattern for the
level is solid, and neither the color of the line, the color of the labels on
it, nor its width are to be set.  
.LE
.sp
By resetting elements in these parameter
arrays, you can change the situation in various ways.
.sp
.in 0
.ft B           
.S 14
CPPKLB
.S 12
.L2 Purpose
.hw levels
CPPKLB (CONPACK, pick labels)
picks a set of labels for labeled contour levels.
.sp .5
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
.nr 34 \n(.lu
.eo
.am 84
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 84
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 84
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\s11CALL CPPKLB\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IWRK)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array 
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 \n(a-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(b-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(c-
.if \n(84<\n(38 .nr 84 \n(38
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
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 2285 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPPKLB\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RWRK,\h'|\n(42u'Real array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IWRK)\h'|\n(42u'Integer array \h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.c+
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-14
.in +.5i
.sp
All three arguments are arrays used in the last call to CPRECT or CPSPRS, the
contents of which must not have been changed since that call.
.VL 1.2i
.LI \fBZDAT\fR
The data array.
.LI \fBRWRK\fR
The real workspace array.
.LI \fBIWRK\fR
The integer workspace array.
.LE
.L2 "Usage"
Normally, CPPKLB is called by CONPACK when labels for the contour levels are
needed.
You can call
CPPKLB directly (after the initialization call to CPRECT
or CPSPRS) 
when you want to modify the resulting parameter arrays
that specify the labels.
.sp
If the constant-field-flag \&'CFF' is non-zero, indicating that, on the last
call to CPRECT or CPSPRS, the data were found to be essentially constant,
CPPKLB does nothing.
Otherwise, CPPKLB examines the first \&'NCL' elements of the
parameter array \&'CLV', which defines the contour levels, and the associated
parameter arrays, looking for levels that are to be labeled ('CLU' = 2 or
3) for which no label is specified ('LLT' = \&' \&').  (The
value of \&'LLT' is a single blank.)  If any such levels are
found, labels are generated for them.
.sp
The scale factor \&'SFU' may be set as a byproduct of choosing the labels.  See
the description of the parameters \&'SFU' (scale factor used)
and \&'SFS' (scale factor selector) in the section entitled
"CONPACK Internal Parameter Descriptions."
After calling CPPKLB, a user program may examine the generated labels and
change them in various ways.
.L2 "Action Subroutines"
The subroutines documented in this section perform 
some action related to contour lines, contour labels,
or the contour background.
.sp
.in 0
.ft B           
.S 14
CPCLDR
.S 12
.L2 Purpose
CPCLDR (CONPACK, contour lines, draw) draws contour lines.
.sp
.in 0
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
.nr 34 \n(.lu
.eo
.am 84
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 84
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to
CPRECT or CPSPRS
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 84
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to
CPRECT or CPSPRS
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\s11CALL CPCLDR\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IWRK)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 \n(a-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(b-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(c-
.if \n(84<\n(38 .nr 84 \n(38
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
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 2353 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPCLDR\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RWRK,\h'|\n(42u'Real array\h'|\n(43u'Input/Output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IWRK)\h'|\n(42u'Integer array\h'|\n(43u'Input/Output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.c+
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-16
.in +.5i
.sp
All three arguments are arrays used in the last call to CPRECT or CPSPRS, the
contents of which must not have been changed since that call.
.VL 1.2i
.LI \fBZDAT\fR
The data array.
.LI \fBRWRK\fR
The real workspace array.
.LI \fBIWRK\fR
The integer workspace array.
.LE
.L2 "Usage"
The routine CPCLDR, which draws contour lines, may be called at 
any time after the initialization call
to CPRECT or CPSPRS.  The contour lines drawn are those specified 
by the first \&'NCL' elements of
the parameter arrays \&'CLV' and \&'CLU'.  If \&'NCL' is zero, CPPKCL is called
to generate these values; if \&'NCL' is still zero after the call to CPPKCL, a
fatal error exit results.  Each element of \&'CLV' specifies a contour level and
the corresponding element of \&'CLU' specifies whether or not contour lines are
to be drawn at that level and whether or not the lines are to be labeled.
If the parameter \&'T2D' has a non-zero value, the contour lines are smoothed,
using cubic splines under tension.
.sp
If the element of the parameter array \&'CLU' corresponding to \&'PAI' = -1 is
non-zero, the edge of the grid is also drawn.  If the element of \&'CLU'
corresponding to \&'PAI' = -2 is non-zero, the edges of special-value areas
(if any) are drawn.  If the element of 
\&'CLU' corresponding to \&'PAI' = -3 is
non-zero, the edges of out-of-range areas (if any) are drawn.  The default
values are such that none of these edges are drawn.
.sp
Groups of lines are drawn in the following order:
.AL
.LI
contour lines for each of
the specified levels, in ascending numeric order,
.LI
the edges of special-value
areas, if any,
.LI
the edges of out-of-range areas, if any, and
.LI
the edge of the grid.
.LE
.sp
The color, dash pattern, and line width to be used for the lines drawn may
be specified by elements of the parameter arrays \&'CLC', \&'CLD', and \&'CLL',
respectively.  Each of these contains elements corresponding to values 
of \&'PAI' from 1 to \&'NCL' and three special elements, corresponding 
to \&'PAI' = -1, -2, and -3.  Before and after each group of lines 
is drawn, the routine CPCHCL is called.
You can supply your own version of CPCHCL to override the
settings of color, dash pattern, and line width.
.sp
\fBDash-pattern-usage Parameter:\fR  
The dash-pattern-usage parameter
('DPU')
affects the pattern
used to draw the lines.
Set the value of \&'DPU' 
as follows:
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
.ll 4.75in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Lines are drawn by calling
the SPPS routine CURVE.  Lines are all solid and unlabeled;
specified dash patterns are not used.  
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
.ll 4.75in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Lines are drawn by calling the DASHCHAR routine CURVED.
Lines are
solid or dashed, depending on the dash pattern specified by the appropriate
element of \&'CLD'.  
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
.ll 4.75in
.if \n(.l<\n(81 .ll \n(81u
.in 0
If ABS('LLP') = 1, then the
dash pattern
for those lines that are to be labeled is constructed by replicating, \&'DPU'
times, the
dash pattern specified by the appropriate element of \&'CLD', 
and then 
.hw ap-pend-ing
appending to it
the characters specified by the appropriate element of
\&'LLT'.  
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
.ll 4.75in
.if \n(.l<\n(81 .ll \n(81u
.in 0
If ABS('LLP') is greater than 1, then the
lines
drawn will pass through any labels drawn by CPLBDR.
If this is undesirable,
you can call
CPLBAM to put the label boxes into an area map and then call
CPCLDM instead of CPCLDR
to draw only those portions of the contour lines which do not lie inside
the label boxes.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLE 0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0
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
.nr 38 4.75in
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
.if t .if \n(TW>\n(.li .tm Table at line 2460 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LE 0\h'|\n(41u'
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
\&\h'|\n(40u'>0\h'|\n(41u'
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
\&\h'|\n(40u'\h'|\n(41u'
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
\&\h'|\n(40u'\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-42
.sp
If, during the last call to CPRECT or CPSPRS, the data being contoured were
found to be essentially constant, then no contour lines are drawn; instead,
the constant-field label is written.  Other lines are still drawn.
.in 0
.ft B           
.S 14
CPCLDM
.S 12
.L2 Purpose
CPCLDM (CONPACK, contour lines, drawn masked) draws contour lines masked by an 
existing area map.  The object of this may
be simply to avoid drawing contour lines through label boxes, but the routine
may be used for more complicated tasks, like limiting the drawing of contour
lines to the ocean areas on an EZMAP background.
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
.nr 34 \n(.lu
.eo
.am 84
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 84
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 84
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 84
.br
.di d+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to ARINAM
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\s11CALL CPCLDM\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IAMA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RTPL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w(Subroutine)
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 \n(a-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(b-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(c-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(d-
.if \n(84<\n(38 .nr 84 \n(38
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
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 2496 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPCLDM\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RWRK,\h'|\n(42u'Real array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IWRK,\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IAMA,\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.d+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RTPL)\h'|\n(42u'(Subroutine)\h'|\n(43u'\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-18
.in +.5i
.sp
The first three arguments are arrays used in the last call to CPRECT or
CPSPRS, the contents of which must not have been changed since that call.
.VL 1.2i
.LI \fBZDAT\fR
The data array.
.LI \fBRWRK\fR
The real workspace array.
.LI \fBIWRK\fR
The integer workspace array.
.LI \fBIAMA\fR
The array containing the area map that
is to be used to mask the contour lines as they are drawn.
.LI \fBRTPL\fR
An external subroutine supplied by the user to
process the
polylines that result from masking the generated contour lines and other
edges against the area map.  RTPL must be declared 
external
in the routine
that calls CPCLDM.  RTPL will be called repeatedly and must have 
the following form:
.LE
.br
.ne 9
.>>
.sF
SUBROUTINE RTPL (XCS,YCS,NCS,IAI,IAG,NAI)
  DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
    ---
    (CODE TO PROCESS POLYLINE DEFINED BY ARGUMENTS)
    ---
  RETURN
END
.eF
.<<
The arrays XCS and YCS hold the X and Y coordinates of NCS points defining a
polyline that is to be considered for drawing.  For each I greater than or
equal to 1 and less than or equal to NAI, IAI(I) is the area identifier for
the area in which the polyline lies, relative to the area-identifier group
IAG(I).  The X and Y coordinates are all normalized device coordinates, and it
may be assumed that the appropriate SET call has been done.  If it is decided
to draw the line, it may be done with a call to the SPPS routine CURVE, to
the DASHCHAR routine CURVED, or to the GKS routine GPL.  
.sp
The value of \&'PAI'
will have been set to the appropriate value (1 through \&'NCL', -1, -2, or -3)
for the line of which the polyline is a part.  The color and line width will
have been set as implied by the values of the appropriate elements of the
parameter arrays \&'CLC' and \&'CLL'.  The dash pattern will have been defined as
implied by the value of the appropriate elements of the parameter arrays
\&'CLD' and \&'CLU' and the value of the parameter \&'DPU'.  If a dash pattern is
defined, it may be retrieved by a CALL CPGETC ('CTM',CVAL).
.sp
If the only object of using CPCLDM is to avoid drawing contour lines through
label boxes, then the CONPACK routine CPDRPL may be used for RTPL.  In the
subroutine that calls CPCLDM, insert the declaration
EXTERNAL CPDRPL
and then use CPDRPL for the last argument.
.sp
For more information, see the description of the argument LPR of the
subroutine ARDRLN, in the documentation of the utility AREAS
(in \fINCAR Graphics User's Guide, Version 2.00\fR).
.L2 "Usage"
The routine CPCLDM may be called at any time after the initialization call
to CPRECT or CPSPRS to draw contour lines masked by an existing area map.
Actually, CPCLDM does not draw the lines; it generates them, masks them
against a user-specified area map, and generates calls, one for each
polyline resulting from the masking process,
to the
user-specified routine, RTPL.
Each such polyline lies in precisely one of the areas defined by
the area map.  The routine RTPL may use the information provided by its
arguments, describing the area the polyline is in, to decide whether or not
to draw the polyline.
.sp
The contour lines generated are those specified by the first \&'NCL' elements
of the parameter arrays \&'CLV' and \&'CLU'.  If \&'NCL' is zero, CPPKCL is called
to generate these values; if \&'NCL' is still zero after the call to CPPKCL, a
fatal error exit results.  Each element of \&'CLV' specifies a contour level and
the corresponding element of \&'CLU' specifies whether or not contour lines are
to be generated at that level.  If the parameter \&'T2D' has a non-zero value,
the contour lines are smoothed, using cubic splines under tension.
.sp
If the element of the parameter array \&'CLU' corresponding to \&'PAI' = -1 is
non-zero, the edge of the grid is also generated.  If the element of \&'CLU'
corresponding to \&'PAI' = -2 is non-zero, the edges of special-value areas
(if any) are generated.  If the element of \&'CLU' corresponding to \&'PAI' =
-3 is non-zero, the edges of out-of-range areas (if any) are generated.  The
default values are such that none of these edges are generated.
.sp
Groups of lines are generated in the following order:
.AL
.LI
contour lines for
each of the specified levels, in ascending numeric order,
.LI
the edges of
special-value areas, if any,
.LI
the edges of out-of-range areas, if any, and
.LI
the edge of the grid.
.LE
.sp
The color, dash pattern, and line width to be used for the lines drawn may
be specified by elements of the parameter arrays \&'CLC', \&'CLD', and \&'CLL',
respectively.  Each of these contains elements corresponding to values of
\&'PAI' from 1 to \&'NCL' and three special elements, corresponding to \&'PAI' =
-1, -2, and -3.  Before and after each group of lines is generated, the
routine CPCHCL is called; a user-supplied version of this routine may
override the settings of color, dash pattern, and line width.  Also, of
course, the routine RTPL, which actually does the drawing, may override the
settings of these quantities.
.sp
\fBDash-pattern-usage Parameter:\fR  You can use 
the dash-pattern-usage parameter
('DPU')
to affect the pattern
used to draw the lines.
Set the value of \&'DPU' 
as follows:
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
.ll 4.75in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Lines are drawn by calling
the SPPS routine CURVE. Lines are all solid and unlabeled;
specified dash patterns are not used.  
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
.ll 4.75in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Lines are drawn by calling the DASHCHAR routine CURVED.
Lines are
solid or dashed, depending on the dash pattern specified by the appropriate
element of \&'CLD'.  
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
.ll 4.75in
.if \n(.l<\n(81 .ll \n(81u
.in 0
If ABS('LLP') = 1, then the
dash pattern
for those lines that are to be labeled is constructed by replicating, \&'DPU'
times, the
dash pattern specified by the appropriate element of \&'CLD', 
and then appending to it
the characters specified by the appropriate element of
\&'LLT'.  
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLE 0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0
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
.nr 38 4.75in
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
.if t .if \n(TW>\n(.li .tm Table at line 2649 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LE 0\h'|\n(41u'
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
\&\h'|\n(40u'>0\h'|\n(41u'
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
\&\h'|\n(40u'\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-28
.sp
If, during the last call to CPRECT or CPSPRS, the data being contoured were
found to be essentially constant, then no contour lines are generated;
instead, the constant-field label is written.  Other lines are still
generated.
.ft B           
.S 14
.in 0
CPCLAM
.S 12
.L2 Purpose
CPCLAM (CONPACK, contour lines to area map) adds contour lines to an area map.  
This is part of the process of drawing a solid-fill contour plot.
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
.nr 34 \n(.lu
.eo
.am 84
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 84
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 84
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 84
.br
.di d+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to ARINAM
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\s11CALL CPCLAM\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w~~~\fBArgument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IAMA)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 \n(a-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(b-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(c-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(d-
.if \n(84<\n(38 .nr 84 \n(38
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
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 2682 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'~~~\fBArgument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPCLAM\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RWRK,\h'|\n(42u'Real array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IWRK,\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IAMA)\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.d+
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-17
.in +.5i
.sp
The first three arguments are arrays used in the last call to CPRECT or
CPSPRS, the contents of which must not have been changed since that call.
.VL 1.2i
.LI \fBZDAT\fR
The data array.
.LI \fBRWRK\fR
The real workspace array.
.LI \fBIWRK\fR
The integer workspace array.
.LI \fBIAMA\fR
The array containing the area map to which contour
lines are to be added.  Dimensioned as specified in the last call to
ARINAM, in AREAS.
.LE
.L2 "Usage"
The routine CPCLAM, which adds 
contour lines generated from the data in the array ZDAT to the area map 
in the array IAMA,  
may be called at any time after the initialization call to
CPRECT or CPSPRS.
The area map must previously have
been initialized by a call to the routine ARINAM, in the utility AREAS.
.sp
The contour lines added to the area map are as specified by the first \&'NCL'
elements of the parameter arrays \&'CLV', \&'AIA', and \&'AIB'.  If \&'NCL' is zero,
CPPKCL is called to generate these values; if \&'NCL' is still zero after the
call to CPPKCL, a fatal error results.
.sp
.ne#2
If, for a given value of I between 1 and \&'NCL', inclusive, the Ith element of
either \&'AIA' or \&'AIB' is non-zero, then the contour lines specified by the
Ith element of \&'CLV' are added to the area map, with the Ith element of \&'AIA'
as the area identifier for the area "above" each line (where field values are
greater than they are along the line) and with the Ith element of \&'AIB' as
the area identifier for the area "below" each line (where field values are
less than they are along the line).  If the parameter \&'T2D' has a non-zero
value, the contour lines are smoothed, using cubic splines under tension.
.sp
Four other types of lines are added to the area map by CPCLAM:  (1)
the edge of the
current viewport and possibly a set of vertical lines within the viewport,
(2) the edge of the grid, (3) the edges of special-value areas, if any, and
(4) the edges of out-of-range areas, if any.  The area identifier for 
the outside of the
viewport is always -1.  You can use elements of the parameter array \&'AIA' 
for \&'PAI' = -1,
-2, and -3 to specify the area identifiers to be used for the
outside of the grid, the inside of a special-value area, and the inside of an
out-of-range area, respectively; the default values of all three are -1's.
Area identifiers for all other sides of these edges are determined from the
area-identifier information given for the contour levels.
.sp
Lines are added to the area map in the following order:  
.AL
.LI
the edge of the
viewport and the vertical lines within it, 
.LI
the edges of the out-of-range
areas, if any,
.LI
the edge of the grid,
.LI
the edges of the special-value areas, if any, and,
.LI
the contour lines, in order of increasing contour level.
.LE
.sp
The edge of the viewport may actually be added to the area map 
twice:
.AL
.LI
as part of the edge group with group identifier \&'GIC', and 
.LI
as part of the edge group with group identifier \&'GIS'.  
.LE
.sp
The object of number 1 above is to
prevent problems that arise when mapping is turned on and the mapping
function has a line of discontinuity (for example, when using EZMAP with
a cylindrical equidistant projection).  The object of number
2 above is to break
up the areas represented by the area map into smaller pieces. 
Whether this is done or not is under your control, by means of the internal
parameters \&'NVS'
and \&'GIS'.  For more information, see the descriptions of those internal
parameters.
.sp
If, during the last call to CPRECT or CPSPRS, the data being contoured were
found to be essentially constant, then no contour lines are added to the
area map; the other lines are added, however.
.in 0
.ft B           
.S 14
CPLBDR
.S 12
.L2 Purpose
CPLBDR (CONPACK, labels, draw) draws three kinds of labels:  informational
labels, high and low labels, and line labels.
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
.nr 34 \n(.lu
.eo
.am 84
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 84
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 84
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\s11CALL CPLBDR\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IWRK)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 \n(a-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(b-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(c-
.if \n(84<\n(38 .nr 84 \n(38
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
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 2800 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPLBDR\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RWRK,\h'|\n(42u'Real array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IWRK)\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.c+
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-14
.sp
.in +.5i
All three arguments are arrays used in the last call to CPRECT or CPSPRS, the
contents of which must not have been changed since that call.
.VL 1.2i
.LI \fBZDAT\fR
The data array.
.LI \fBRWRK\fR
The real workspace array.
.LI \fBIWRK\fR
The integer workspace array.
.LE
.L2 "Usage"
The routine CPLBDR, which draws labels, may be called at 
any time after the initialization call
to CPRECT or CPSPRS.  If, during the last call to CPRECT or
CPSPRS, the data being contoured were found to be essentially constant, then
the constant-field label is drawn.  Otherwise, the informational label, the
high and low labels, and/or the contour-line labels are drawn,
as follows:
.BL
.LI
The informational label is drawn only if the parameter \&'ILT', which specifies
the text of that label, is non-blank.
.LI
High labels are drawn only if the parameter \&'HIT', which specifies the text
of those labels, is non-blank.
.LI
Low labels are drawn only if the parameter \&'LOT', which specifies the text of
those labels, is non-blank.
.LI
Contour line labels are drawn only if the parameter \&'LLP', which specifies how
those labels are to be positioned, has an absolute value of 2 or 3, and if,
for some I between 1 and \&'NCL', inclusive, the Ith element of 
the parameter
array \&'CLU' has a value implying that contour lines at the contour level
specified by the Ith element of \&'CLV' are to be labeled.
.LE
.in 0
.ft B           
.S 14
CPLBAM
.S 12
.L2 Purpose
CPLBAM (CONPACK, label boxes to area map)
adds label boxes for the informational label, for high and low labels, and
for contour-line labels to an area map.  The ultimate object of this will
usually be to prevent contour lines drawn by CPCLDM from passing through
labels or to prevent fill of the label boxes as contour bands are filled.
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
.nr 34 \n(.lu
.eo
.am 84
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 84
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 84
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 84
.br
.di d+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to ARINAM
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\s11CALL CPLBAM\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IAMA)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 \n(a-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(b-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(c-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(d-
.if \n(84<\n(38 .nr 84 \n(38
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
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 2869 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPLBAM\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RWRK,\h'|\n(42u'Real array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IWRK,\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IAMA)\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.d+
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-17
.sp
.in +.5i
The first three arguments are arrays used in the last call to CPRECT or
CPSPRS, the contents of which must not have been changed since that call.
.VL 1.2i
.LI \fBZDAT\fR
The data array.
.LI \fBRWRK\fR
The real workspace array.
.LI \fBIWRK\fR
The integer workspace array.
.LI \fBIAMA\fR
The array holding the area map to which the label boxes are to be added.
Dimensioned as specified in the last call to ARINAM, which is part
of the utility AREAS.
.LE
.L2 "Usage"
The routine CPLBAM, which adds label boxes to an area map, may be 
called at any time after the initialization call
to CPRECT or CPSPRS.  If, during the last
call to CPRECT or CPSPRS, the data being contoured were found to be
essentially constant, then the box for the constant-field label is added to
the area map.  Otherwise, boxes for the informational label, the high and low
labels, and/or the contour-line labels are added, as
follows:
.BL
.LI
A box for the informational label is added only if the parameter \&'ILT', which
specifies the text of that label, is non-blank.
.LI
Boxes for the high labels are added only if the parameter \&'HIT', which
specifies the text of those labels, is non-blank.
.LI
Boxes for the low labels are added only if the parameter \&'LOT', which
specifies the text of those labels, is non-blank.
.LI
Boxes for the contour line labels are added only if the parameter \&'LLP',
which specifies how those labels are to be positioned, has an 
absolute value of 2
or 3, and if, for some I between 1 and \&'NCL', inclusive, the Ith element of
the parameter array \&'CLU' has a value implying that contour lines at the
contour level specified by the Ith element of \&'CLV' are to be labeled.
.LE
.in 0
.ft B           
.S 14
.in 0
CPBACK
.S 12
.L2 Purpose
CPBACK (CONPACK, background) draws a background for a contour plot.
This initial version of CPBACK only draws major tick marks on the 
horizontal and vertical sides of the perimeter.  If you have suggestions 
for other functions for CPBACK,
please fill out the Reader Comment form (the last page of this
manual). 
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
.nr 34 \n(.lu
.eo
.am 84
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 84
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 84
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*1u/6u
.if \n(.l<\n(84 .ll \n(84u
.in 0
By last call to CPRECT or CPSPRS
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\s11CALL CPBACK\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~RWRK,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IWRK)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/output
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 38 \n(a-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(b-
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \n(c-
.if \n(84<\n(38 .nr 84 \n(38
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
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
.if t .if \n(TW>\n(.li .tm Table at line 2942 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPBACK\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~RWRK,\h'|\n(42u'Real array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IWRK)\h'|\n(42u'Integer array\h'|\n(43u'Input/output\h'|\n(44u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(44u
.in +\n(37u
.c+
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-14
.sp
.in +.5i
All three arguments 
are arrays used in the last call to CPRECT or CPSPRS, the
contents of which must not have been changed since that call.
.VL 1.2i
.LI "\fBZDAT\fR" 
The data array.
.LI "\fBRWRK\fR"
The real workspace array.
.LI "\fBIWRK\fR" 
The integer workspace array.
.LE
.L2 "Usage"
You can call
CPBACK at any time after the initialization call to
CPRECT or CPSPRS.  What CPBACK does depends on the value of the internal
parameter \&'MAP', as follows:
.BL
.LI
If \&'MAP' is set to zero,
CPBACK draws the perimeter
of the current viewport by calling PERIM, in the utility GRIDAL,
requesting \&'ZDM'-1 major ticks on the horizontal sides of the perimeter and
\&'ZDN'-1 major ticks on the vertical sides.
No minor ticks are drawn. 
.LI
If \&'MAP' is set non-zero, CPBACK does nothing.
.sp
.LE
.in 0
.ft B           
.S 14
CPDRPL
.S 12
.L2 Purpose
CPDRPL (CONPACK, draw polyline) may be specified as the
subroutine to be called by CPCLDM.  If you are only using CPCLDM to avoid drawing contour
lines through labels,
then put the declaration
EXTERNAL CPDRPL
in the subroutine that calls CPCLDM and use CPDRPL for the
argument RTPL in the call to CPCLDM.
(See the description of the argument RTPL of CPCLDM.)
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
.nr 38 \w\fB\s11CALL CPDRPL\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(XCS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~YCS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~NCS,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IAI,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~IAG,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~NAI)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger array
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
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAt least NCS
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAt least NCS
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAt least NAI
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAt least NAI
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
.if t .if \n(TW>\n(.li .tm Table at line 3000 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPDRPL\fR\s12\h'|\n(41u'~~~(XCS,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'At least NCS
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~YCS,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'At least NCS
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~NCS,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IAI,\h'|\n(42u'Integer array\h'|\n(43u'Input\h'|\n(44u'At least NAI
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~IAG,\h'|\n(42u'Integer array\h'|\n(43u'Input\h'|\n(44u'At least NAI
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~NAI)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-11
.in +.5i
.VL 1.2i
.LI \fBXCS\fR
An array containing the X coordinates of NCS points defining a polyline.
.LI \fBYCS\fR
An array containing the Y coordinates of NCS points defining a polyline.
.LI \fBNCS\fR
The number of points defining the polyline.
.LI \fBIAI\fR
An array of area
identifiers for the area in which the polyline lies.  For each I from 1 to
NAI, IAI(I) is the area identifier of the area with respect to the edge group
IAG(I).
.LI \fBIAG\fR
An array of group identifiers.  See the description of IAI, above.
.LI \fBNAI\fR
The number of group identifiers in the array IAG.
.LE
.L2 "Usage"
If the only reason you are using CPCLDM instead of CPCLDR is to avoid
drawing contour lines through labels, follow these steps:
.AL
.LI
In the subroutine that calls CPCLDM, put the declaration
EXTERNAL CPDRPL.
.LI
In the call to CPCLDM, use CPDRPL for the argument RTPL.  
.LE
.sp
Each time
CPDRPL is called, it draws the polyline defined by its first three arguments
if, and only if, none of the area identifiers defined by the other three
arguments is negative.
.in 0
.ft B           
.S 14
CPMPXY
.S 12
.L2 Purpose
CPMPXY (CONPACK, map X and Y coordinates)
maps CONPACK output from a rectangular coordinate system superimposed on
the data grid to some other coordinate system.  The default
version handles the mapping of longitudes and latitudes into
rectangular coordinates on a given EZMAP projection.  It also
handles mapping polar coordinates into rectangular
coordinates.  You can supply your own version of CPMPXY to produce any
other desired mapping.  CPMPXY is not
called by you; it is called by CONPACK when the parameter \&'MAP' is non-zero.
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
.nr 38 \w\fB\s11CALL CPMPXY\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(IMAP,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~XINP,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~YINP,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~XOTP,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~YOTP)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal 
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal 
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal 
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal 
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
.nr 38 \wOutput
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
.if t .if \n(TW>\n(.li .tm Table at line 3061 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPMPXY\fR\s12\h'|\n(41u'~~~(IMAP,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~XINP,\h'|\n(42u'Real \h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~YINP,\h'|\n(42u'Real \h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~XOTP,\h'|\n(42u'Real \h'|\n(43u'Output\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~YOTP)\h'|\n(42u'Real \h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-10
.in +.5i
.VL 1.2i
.LI \fBIMAP\fR
The current (non-zero) value of the parameter \&'MAP'.
.LI \fBXINP\fR
The X coordinate of a point on the contour plot.  If
\&'XC1' = \&'XCM' (the default situation), then XINP will lie in the range from 1
to \fIm\fR, where \fIm\fR is the first dimension of the array being contoured (equal to
the value of the parameter \&'ZDM').  In this case, the X coordinate will have
the same range as the first index of the data array.  If you set \&'XC1'
unequal to \&'XCM', then XINP will lie in the range from \&'XC1' (corresponding
to an index value of 1) to \&'XCM' (corresponding to an index value of \fIm\fR).
.LI \fBYINP\fR
The Y coordinate of a point on the contour plot.  If \&'YCI' = \&'YCN' (the default situation), then YINP will lie in the range from
1 to \fIn\fR, where \fIn\fR is 
the second dimension of the array being contoured (equal to
the value of the parameter \&'ZDN').  In this case, the Y coordinate will have
the same range as the second index of the data array.  If you set \&'YC1'
unequal to \&'YCM', then YINP will lie in the range from \&'YC1' (corresponding
to an index value of 1) to \&'YCN' (corresponding to an index value of \fIn\fR).
.LI \fBXOTP\fR
The mapped X coordinate of a point on the contour
plot, in a coordinate system consistent with the current window, as specified
by arguments 5 through 8 of the last call to the SPPS subroutine SET or by the
equivalent GKS call.
.LI \fBYOTP\fR
The mapped Y coordinate of a point on the contour
plot, in a coordinate system consistent with the current window, as specified
by arguments 5 through 8 of the last call to the SPPS subroutine SET or by the
equivalent GKS call.
.LE
.L2 "Usage"
Each call to CPMPXY is intended to map the X and Y
coordinates of a single point, whose position is known relative to the data
grid, to X and Y coordinates in some other system.  The default version of
CPMPXY is as follows:
.sF
.>>
SUBROUTINE CPMPXY (IMAP,XINP,YINP,XOTP,YOTP)
 IF (IMAP.EQ.1) THEN
  CALL MAPTRN (YINP,XINP,XOTP,YOTP)
 ELSE IF (IMAP.EQ.2) THEN
  XOTP=XINP*COS(.017453292519943*YINP)
  YOTP=XINP*SIN(.017453292519943*YINP)
 ELSE
  XOTP=XINP
  YOTP=YINP
 END IF
 RETURN
END
.<<
.eF
.BL
.LI
When IMAP (which is the value of \&'MAP') = 1, the incoming X and Y coordinates
are assumed to represent longitude and latitude, respectively.  The EZMAP
subroutine MAPTRN is called to find the X and Y coordinates of the projection
of the specified point on the globe, and those coordinates are returned as
the outgoing X and Y coordinates.
.LI
When IMAP = 2, the incoming X and Y
coordinates are assumed to represent rho and theta (in degrees) in polar
coordinates.
The output X and Y coordinates are computed from these.
.LI
If
IMAP is anything else, the input X and Y are simply returned.
.LE
.ne 6
.in 0
.L2 "Transition Subroutines"
The subroutines in this section may be useful to two different groups of users:
.AL
.LI
If you have old programs that call the Version 2.00 subroutine CONREC, 
you can use CPCNRC (CONPACK, CONREC simulation) to simulate the behavior of CONREC.
Or, if your old programs call the CONREC subroutine EZCNTR, you can use
CPEZCT (CONPACK, EZCNTR simulation) to simulate EZCNTR.
.LI
If you just want to produce basic contour plots, you can use a single call to either CPCNRC or CPEZCT to do that.
.LE
.sp
The disadvantage of using CPCNRC or CPEZCT is that not all of the power of the 
CONPACK utility is available to you \(em you 
cannot do color fill, for example.
.in 0
.ft B           
.S 14
CPCNRC
.S 12
.L2 Purpose
CPCNRC (CONPACK, CONREC simulation) simulates the behavior of the old routine CONREC;
it has the same arguments and will produce similar output.
.sp
.in 0
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
.nr 38 \w\fB\s11CALL CPCNRC\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~KZDT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~MZDT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~NZDT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~FLOW,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~FHGH,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~FINC,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~NSET,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~NHGH,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~NDSH)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger 
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger 
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal 
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal 
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal 
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger 
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
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wKZDT by \fIn\fR
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
.if t .if \n(TW>\n(.li .tm Table at line 3172 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPCNRC\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'KZDT by \fIn\fR
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~KZDT,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~MZDT,\h'|\n(42u'Integer \h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~NZDT,\h'|\n(42u'Integer \h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~FLOW,\h'|\n(42u'Real \h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~FHGH,\h'|\n(42u'Real \h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~FINC,\h'|\n(42u'Real \h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~NSET,\h'|\n(42u'Integer \h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~NHGH,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~NDSH)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-15
.in +.5i
.VL 1.2i
.LI \fBZDAT\fR
The array of data to be contoured.
Dimensioned KZDT by \fIn\fR, where \fIn\fR is greater than or 
equal to NZDT.
.LI \fBKZDT\fR
The first dimension of the array ZDAT.
.LI \fBMZDT\fR
The first dimension of the array of data in ZDAT.
MZDT must be less than or equal to KZDT.
.LI \fBNZDT\fR
The second dimension of the array of data in ZDAT.
NZDT must be less than or equal to the declared second dimension of the array
ZDAT.
.LI \fBFLOW\fR
The desired lowest contour level.  If FLOW is equal to
or greater than FHGH, CONPACK will choose the set of contour levels.
.LI \fBFHGH\fR
The desired highest contour level.  If FHGH is equal to
or less than FLOW, CONPACK will choose the set of contour levels.
.LI \fBFINC\fR
Determines how contour levels are to be chosen.  Choose a
value as follows:
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
Specifies the desired contour interval
to be used.
.br
In this case, if FLOW is less than FHGH, the 
.br
intervals used
will be FLOW, FLOW+FINC, FLOW+2*FINC, \&... FLOW+\fIn\fR*FINC, where \fIn\fR
is the largest integer such that FLOW+\fIn\fR*FINC is less than or equal to 
FHGH.  If FLOW is greater than or equal to FHGH, the contour levels will be 
those integer multiples of FINC that fall between the minimum value in ZDAT
and the maximum value in ZDAT.
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
CONPACK will choose the contour
interval in such a way as to give at least 16 contour levels (if FINC is
zero) or MAX(1,INT(-FINC)) contour levels (if FINC is less than zero)
between the minimum and maximum values in ZDAT.  All the contour levels
will be integer multiples of the chosen interval.  If FLOW is less than
FHGH, no contour lines will be drawn for chosen contour levels that are
outside the range (FLOW,FHGH).
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
.if t .if \n(TW>\n(.li .tm Table at line 3226 file man/conpack.l is too wide - \n(TW units
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-28
.sp
.LI \fBNSET\fR
Determines how the contour plot is to be mapped onto the
plotter frame.  Choose a value as follows:
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
The standard configuration will be used.  CONPACK will
be directed to call SET.  The portion of the plotter frame bounded by the
lines "X=.05", "X=.95", "Y=.05", and "Y=.95" (in the fractional or
normalized-device-coordinate system) will be used.  The shape of the
plot will be determined by the values of the internal parameters \&'WDL\&',
\&'WDR\&', \&'WDB\&', and \&'WDT\&'; by default, the ratio of the plot's width to its
height will be MZDT/NZDT.  If the ratio of the width to the height is
less than 1/4 or greater than 4, the plot will be made square.  CPBACK
will be called to draw a perimeter.
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
The contour plot will fill the current
viewport.  CONPACK will be directed to call SET.  The portion of the
plotter frame used will be that bounded by the lines "X=xl", "X=xr",
\&"Y=yb", and "Y=yt", where "xl", "xr", "yl", and "yr" are obtained by
means of a
.>>
.sF
CALL GETSET (xl,xr,yl,yr,...)
.eF
.<<
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
The plot will fill this entire area.  CPBACK will not be called to draw
a perimeter.
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
CONPACK is directed not to call SET.
It will be assumed that you have done the appropriate call.  CPBACK
will not be called to draw a perimeter.  Use this option when overlaying
CPCNRC output on an existing background (one drawn by EZMAP, for example).
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0
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
.if t .if \n(TW>\n(.li .tm Table at line 3272 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'0\h'|\n(41u'
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
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
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
\&\h'|\n(40u'>0\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-40
.sp
.LI \fBNHGH\fR
Determines whether highs and lows or data points are to be
labeled.  Choose a value as follows:
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
Each high is marked with an H and each low is marked
with an L; the value is written as a subscript of the H or L.
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
Each data point is marked with the value
at that point.  No attempt is made to cull overlapping values, so using
this option when there are too many data points may result in a mess.
The values of the CONPACK internal parameters \&'HLA' and \&'HLS' are 
retrieved and
used to determine the angle at which the values are written and the size
of the characters used.  They may be used to reduce the clutter.
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
Neither of the above is done. 
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w~~~0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0
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
.nr 38 \n(c-
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
.if t .if \n(TW>\n(.li .tm Table at line 3301 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'~~~0\h'|\n(41u'
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
\&\h'|\n(40u'>0\h'|\n(41u'
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
\&\h'|\n(40u'<0\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-23
.sp
.LI \fBNDSH\fR
The absolute value of NDSH is a 10-bit dash pattern to be
used.  The 10-bit pattern is actually made into a 16-bit
pattern by prepending a copy of the low-order six bits.
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
The specified dash pattern is used for all contour lines.
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
The dash pattern is used only for negative-valued
contour lines.  
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
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0
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
.if t .if \n(TW>\n(.li .tm Table at line 3320 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.sp
If ABS(NDSH) is 0, 1, or 1023, all solid lines are used.
.LE
.L2 "Usage"
The appearance of the plot produced by CPCNRC may
vary, depending on the setting of internal parameters of CONPACK.  The
following should be noted:
.AL
.LI
By default, contour lines will be labeled using the old CONREC
scheme of appending the label for a line to the dash pattern for the
line.  To use one of the new schemes, place a 
.>>
.sF
CALL CPSETI (\&'LLP - LINE LABEL POSITIONING\&',\fIn\fR)
.eF
.<<
\fRwhere \fIn\fR has the value 2 or 3, before 
the call to CPCNRC.  CPCNRC retrieves the value of \&'LLP' and,
if the value is a 2 or a 3, modifies the calls that it performs so as to
properly shield labels from having contour lines drawn through them.
.LI
By default, high-quality characters will be used in the informational
label, in high/low labels, in point-value labels, and in contour-line
labels written when \&'LLP\&' is 2 or 3.  To use lower-quality characters,
place a 
.>>
.sF
CALL PCSETI (\&'QUALITY\&',1)
.<<
.eF
or a 
.>>
.sF
CALL PCSETI (\&'QUALITY\&',2)
.eF
.<<
prior to the call to CPCNRC.  
.sp
Note that these calls are to subroutines that start with the
letters PC, because the routine you are 
calling is in the utility PLOTCHAR.  (CONPACK uses PLOTCHAR
to plot characters.  See the "Text, Labels, and Legends" chapter for 
examples of the 
different quality levels of characters produced by PLOTCHAR.)
.LI
If the output from CPCNRC is to be overlaid on an EZMAP background and
longitude is a linear function of the first subscript of the data
array and latitude is a linear function of the second subscript of
the data array, you can 
.BL
.LI
Call EZMAP to draw the background.
.LI
Set the parameter \&'MAP\&' to 1.
.LI
Set the parameters \&'XC1\&', \&'XCM\&', \&'YC1\&',
and \&'YCN\&' to specify the longitude and latitude ranges.
.sp
Note that \&'XCM\&' must be numerically
larger than \&'XC1\&'.  If, for example, your data run from 175 degrees
east of Greenwich at the left edge to 160 degrees west of Greenwich
at the right edge (which is -160 degrees east), then you should set
\&'XC1\&' = 175 and \&'XCM\&' = 200 to achieve the proper mapping.
.LI
Call CPCNRC to draw the contour plot.  
.LE
.LI
To smooth the contour lines in the same way that they would have been
smoothed by the "smooth" or "super" versions of CONREC, insert
.>> 
.sF
CALL CPSETR (\&'T2D - TENSION ON THE 2D SPLINES\&',\fIt\fR),
.eF
.<<
\fRwhere \fIt\fR is the desired tension, to turn on the smoother.
.LI
By default, no scale factor is used.  (Because CONPACK can write
contour line labels in scientific notation, it was considered less
important to use a scale factor.)  If you want to use a scale
factor, the value of the internal parameter \&'SFS\&' should be set prior
to calling CPCNRC.
.LI
CPCNRC contains a real workspace array, an integer workspace array,
and an area map, which is used to prevent contour lines from passing through
labels when \&'LLP\&' has the value 2 or 3.  The lengths of these arrays
are 5000, 2000, and 12000, respectively.  If this proves insufficient,
you must obtain a copy of the code 
for CPCNRC, adjust the dimensions
as required, and compile it in place of the default version.
.LI
Certain internal parameters are used by CPCNRC; user calls to reset them prior
to the call to CPCNRC will therefore have no effect.  These parameters
are as follows:  
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
.rm 80 81 82 83 84 85
.nr 80 0
.nr 38 \w\&'AIA\&'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLL\&'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLT\&'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'SET\&'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\&'AIB\&'
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w\&'CLS\&'
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w\&'HIT\&'
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w\&'VPB\&'
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \w\&'CIS\&'
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w\&'CLV\&'
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w\&'LOT\&'
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w\&'VPL\&'
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \w\&'CIU\&'
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w\&'CLU\&'
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w\&'LLC\&'
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w\&'VPR\&'
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \w\&'CLC\&'
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w\&'CMN\&'
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w\&'LLT\&'
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w\&'VPS\&'
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 85 0
.nr 38 \w\&'CLD\&'
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \w\&'CMX\&'
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \w\&'NCL\&'
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \w\&'VPT\&'
.if \n(85<\n(38 .nr 85 \n(38
.85
.rm 85
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
.nr 45 \n(84+(3*\n(38)
.nr 85 +\n(45
.nr TW \n(85
.if t .if \n(TW>\n(.li .tm Table at line 3423 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'AIA\&'\h'|\n(41u'\&'AIB\&'\h'|\n(42u'\&'CIS\&'\h'|\n(43u'\&'CIU\&'\h'|\n(44u'\&'CLC\&'\h'|\n(45u'\&'CLD\&'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLL\&'\h'|\n(41u'\&'CLS\&'\h'|\n(42u'\&'CLV\&'\h'|\n(43u'\&'CLU\&'\h'|\n(44u'\&'CMN\&'\h'|\n(45u'\&'CMX\&'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLT\&'\h'|\n(41u'\&'HIT\&'\h'|\n(42u'\&'LOT\&'\h'|\n(43u'\&'LLC\&'\h'|\n(44u'\&'LLT\&'\h'|\n(45u'\&'NCL\&'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'SET\&'\h'|\n(41u'\&'VPB\&'\h'|\n(42u'\&'VPL\&'\h'|\n(43u'\&'VPR\&'\h'|\n(44u'\&'VPS\&'\h'|\n(45u'\&'VPT\&'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.LI
Subroutines with names of the form CPCH\fIxx\fR (the change
subroutines) will be of
more value when used in conjunction with CPCNRC than when used in
conjunction with the more basic 
CONPACK subroutines.  For example,
the only way to change the color of the zero contour line drawn by
a call to CPCNRC is to supply your own version of the routine
CPCHCL; there is no way, short of modifying the code of CPCNRC, to
use the internal parameter that controls the color of the zero contour line.
.LI
Like CONREC, CPCNRC does no call to FRAME.  To advance the frame, put
a CALL FRAME after the call to CPCNRC.
.LE
.in 0
.ft B           
.S 14
CPEZCT
.S 12
.L2 Purpose
CPEZCT (CONPACK, EZCNTR simulation)
simulates the behavior of the old subroutine EZCNTR in CONREC; it has
the same arguments
and will produce similar output.
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
.nr 38 \w\fB\s11CALL CPEZCT\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(ZDAT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~MZDT,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~NZDT)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal array
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
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wMZDT by NZDT
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
.if t .if \n(TW>\n(.li .tm Table at line 3457 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPEZCT\fR\s12\h'|\n(41u'~~~(ZDAT,\h'|\n(42u'Real array\h'|\n(43u'Input\h'|\n(44u'MZDT by NZDT
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~MZDT,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~NZDT)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-8
.in +.5i
.VL 1.2i
.LI \fBZDAT\fR
The array containing the data to be contoured.
.LI \fBMZDT\fR
The first dimension of the array ZDAT and of
the array of data stored in it.
.LI \fBNZDT\fR
The second dimension of the array ZDAT and of
the array of data stored in it.
.LE
.L2 "Usage"
The effect of calling CPEZCT will be exactly the same as if you 
had executed the statements
.sF
.>>
CALL CPCNRC (ZDAT,MZDT,MZDT,NZDT,0.,0.,0.,0,0,-682)
CALL FRAME
.eF
.<<
See the description of CPCNRC on the previous pages.
.in 0
.L2 "Change Subroutines"
Each subroutine documented in this section allows you to regain
control during execution of a CONPACK subroutine you have called, in order
to change what it does.
This is accomplished by your supplying your own version of the subroutine
to replace the default version.
The default versions of these subroutines do nothing.  
The names of these subroutines are all of the form "CPCH\fIxx\fR,"
which stands for "CONPACK, change \fIxx\fR."
.sp
Each change subroutine is called by CONPACK at a crucial point during 
execution 
\(em a point at which you might want some code of your own to be executed.  For
example, just before (and again just after) the contour lines at a given
level are drawn, the subroutine CPCHCL is called, with arguments detailing
what is about to happen (or has just happened).  The default version of
this subroutine does nothing; 
you may supply your own version to change color, dash
pattern, line width, and so on.
.sp
.in 0
.ft B           
.S 14
CPCHCF
.S 12
.L2 Purpose
CPCHCF (CONPACK, change constant field label) provides user control as a 
constant-field label is drawn.
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
.nr 38 \w\fB\s11CALL CPCHCF\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(IFLG)
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
.if t .if \n(TW>\n(.li .tm Table at line 3516 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPCHCF\fR\s12\h'|\n(41u'~~~(IFLG)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.VL 1.2i
.LI \fBIFLG\fR
IFLG is positive if an action is about to be taken, negative
if an action has just been completed.  The action in question is defined by
the absolute value of IFLG, as follows:
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
Implies the determination of the size of the label (by means of a 
call to PLCHHQ, in the 
utility PLOTCHAR, with ANGD=360.).
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
Implies the filling of the box around the label, which is done 
before the drawing of the label itself.
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
Implies the drawing of the label (by means of a call to PLCHHQ, in 
the utility PLOTCHAR).
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
Implies the drawing of the box around the label, which is done after 
the drawing of the 
label itself.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w4
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
.if t .if \n(TW>\n(.li .tm Table at line 3550 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
\&\h'|\n(40u'3\h'|\n(41u'
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
\&\h'|\n(40u'4\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-26
.L2 Usage
A version of CPCHCF supplied by you
can change attributes such as
color and line width (by calling 
the appropriate GKS routines.)
.sp
You can retrieve the text of the label being written 
by a CALL CPGETC (\&'CTM\&',CVAL).
You can change the text of the label
by a CALL CPSETC (\&'CTM\&',CVAL).
This latter call should only be
done during a call with IFLG = 1 or 3 and, if it is done for one of those
two values, it should also be done for the other.
.sp
When CPCHCF is called, the parameter \&'ZDV' will have been
set to the value of the field; its value may be
retrieved and used by CPCHCF.
.in 0
.ft B           
.S 14
CPCHCL
.S 12
.L2 Purpose
CPCHCL (CONPACK, change contour lines) provides user control as contour 
lines are drawn.
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
.nr 38 \w\fB\s11CALL CPCHCL\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(IFLG)
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
.if t .if \n(TW>\n(.li .tm Table at line 3584 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPCHCL\fR\s12\h'|\n(41u'~~~(IFLG)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.VL 1.2i
.LI \fBIFLG\fR
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
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w+1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine is about to be drawn.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine has just been drawn.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 3597 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+1\h'|\n(41u'Line is about to be drawn.
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1\h'|\n(41u'Line has just been drawn.
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-8
.LE
.L2 "Usage"
A version of CPCHCL supplied by you can
change attributes such as color and line
width (by calling the appropriate GKS routines).
.sp
If the element of the internal parameter array \&'CLU' corresponding 
to \&'PAI' = -1 has
been set non-zero to request the drawing of the edge of the grid, then CPCHCL
will be called before and after that is done.  Similarly, if the element of
\&'CLU' corresponding to \&'PAI' = -2 has been set non-zero, then CPCHCL will be
called before and after the drawing of the edges of the special-value areas,
and, if the element of \&'CLU' corresponding to \&'PAI' = -3 has been set
non-zero, then CPCHCL will be called before and after the drawing of the
edges of the out-of-range areas.
.sp
When CPCHCL is called, the internal parameter \&'PAI' will have been set 
to the index of
the appropriate contour level (between 1 and \&'NCL') or to one of the values
-1, -2, or -3.  By retrieving the value of \&'PAI', CPCHCL can find out what
line is being drawn; also, a CPGET\fIx\fR call to retrieve an element of an
internal parameter
array like \&'CLD' will automatically get the correct one for the line being
drawn.
.in 0
.ft B           
.S 14
CPCHHL
.S 12
.L2 Purpose
CPCHHL (CONPACK, change high/low labels) provides user control as high and low 
labels are drawn.
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
.nr 38 \w\fB\s11CALL CPCHHL\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(IFLG)
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
.if t .if \n(TW>\n(.li .tm Table at line 3638 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPCHHL\fR\s12\h'|\n(41u'~~~(IFLG)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.VL 1.2i
.LI \fBIFLG\fR
IFLG is positive if an action is about to be taken, negative
if an action has just been completed.  The action in question is defined by
the absolute value of IFLG, as follows:
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
Implies the determination of the size of the label for a
high (by means of a call to PLCHHQ, in the utility PLOTCHAR, with ANGD
= 360.).
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
Implies the filling of the box around the label for a high,
which is done before the drawing of the label itself.
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
Implies the drawing of the label for a high (by means of a
call to PLCHHQ, in the utility PLOTCHAR).
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
Implies the drawing of the box around the label for a high,
which is done after the drawing of the label itself.
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
Implies the determination of the size of the label for a
low (by means of a call to PLCHHQ, in the utility PLOTCHAR, with ANGD
= 360.).
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
Implies the filling of the box around the label for a low,
which is done before the drawing of the label itself.
.br
.di
.nr f| \n(dn
.nr f- \n(dl
..
.ec \
.eo
.am 81
.br
.di g+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Implies the drawing of the label for a low (by means of a
call to PLCHHQ, in the utility PLOTCHAR).
.br
.di
.nr g| \n(dn
.nr g- \n(dl
..
.ec \
.eo
.am 81
.br
.di h+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Implies the drawing of the box around the label for a low,
which is done after the drawing of the label itself.
.br
.di
.nr h| \n(dn
.nr h- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w4
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w5
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w6
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w7
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w8
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
.nr 38 \n(g-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(h-
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
.if t .if \n(TW>\n(.li .tm Table at line 3692 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
\&\h'|\n(40u'3\h'|\n(41u'
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
\&\h'|\n(40u'4\h'|\n(41u'
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
\&\h'|\n(40u'5\h'|\n(41u'
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
\&\h'|\n(40u'6\h'|\n(41u'
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
.sp
.ne \n(g|u+\n(.Vu
.if (\n(g|+\n(#^-1v)>\n(#- .nr #- +(\n(g|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'7\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.g+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(h|u+\n(.Vu
.if (\n(h|+\n(#^-1v)>\n(#- .nr #- +(\n(h|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'8\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.h+
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
.rm g+
.rm h+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-46
.LE
.L2 "Usage"
A version of CPCHHL supplied by you can change attributes such 
as color and line width (by calling
the appropriate GKS routines).  
.sp
The text of the
label may be retrieved by means of a CALL CPGETC ('CTM',CVAL).  The text
of the label may be changed by means of a CALL CPSETC ('CTM',CVAL).
This should only be done during a call with IFLG = 1, 3, 5, or 7.
If it is done
for one of the two values 1 and 3, it should also be done for the other;
if it is done for one of the two values 5 and 7, it should also be done for
the other.  
.sp
When CPCHHL is called, the parameter \&'ZDV' will have been set to
the value of the high or low being labeled; its value may be retrieved and
used by CPCHHL.
.in 0
.ft B           
.S 14
CPCHIL
.S 12
.L2 Purpose
CPCHIL (CONPACK, change informational label) provides user control as the 
informational label is drawn.
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
.nr 38 \w\fB\s11CALL CPCHIL\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(IFLG)
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
.if t .if \n(TW>\n(.li .tm Table at line 3727 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPCHIL\fR\s12\h'|\n(41u'~~~(IFLG)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.VL 1.2i
.LI \fBIFLG\fR
IFLG is positive if an action is about to be taken, negative
if an action has just been completed.  The action in question is defined by
the absolute value of IFLG, as follows:
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
Implies the
determination of the size of the label (by means
of a call to PLCHHQ, in the utility PLOTCHAR, with ANGD=360.).
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
Implies the
filling of the box around the label, which is
done before the drawing of the label itself.
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
Implies the
drawing of the label (by means of a call to
PLCHHQ, in the utility PLOTCHAR).
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
Implies the
drawing of the box around the label, which is
done after the drawing of the label itself.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w4
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
.if t .if \n(TW>\n(.li .tm Table at line 3763 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
\&\h'|\n(40u'3\h'|\n(41u'
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
\&\h'|\n(40u'4\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-28
.LE
.L2 "Usage"
A version of CPCHIL supplied by you can change attributes such 
as color and line width (by calling
the appropriate GKS routines).  
.sp
The text of the
label may be retrieved by means of a CALL CPGETC ('CTM',CVAL).  The text
of the label may be changed by means of a CALL CPSETC ('CTM',CVAL). 
This
should only be done during a call with IFLG = 1 or IFLG = 3, and, if it is
done for one of those values, it should be done for the other.
.in 0
.ft B           
.S 14
CPCHLL
.S 12
.L2 Purpose
CPCHLL (CONPACK, change line labels) provides user control as contour line 
labels are drawn.
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
.nr 38 \w\fB\s11CALL CPCHLL\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~(IFLG)
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
.if t .if \n(TW>\n(.li .tm Table at line 3792 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\h'|\n(41u'\fB~~~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL CPCHLL\fR\s12\h'|\n(41u'~~~(IFLG)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.VL 1.2i
.LI \fBIFLG\fR
IFLG is positive if an action is about to be taken, negative
if an action has just been completed.  The action in question is defined by
the absolute value of IFLG, as follows:
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
Implies the
determination of the size of the label (by means
of a call to PLCHHQ, in the utility PLOTCHAR, with ANGD=360.).
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
Implies the
filling of the box around the label, which is
done before the drawing of the label itself.
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
Implies the
drawing of the label (by means of a call to
PLCHHQ, in the utility PLOTCHAR).
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
Implies the
drawing of the box around the label, which is
done after the drawing of the label itself.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w4
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
.if t .if \n(TW>\n(.li .tm Table at line 3828 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
\&\h'|\n(40u'3\h'|\n(41u'
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
\&\h'|\n(40u'4\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-28
.LE
.L2 "Usage"
A version of CPCHLL supplied by you can change attributes such as 
color and line width (by calling
the appropriate GKS routines).  
.sp
The text of the
label may be retrieved by means of a CALL CPGETC ('CTM',CVAL).  The text
of the label may be changed by means of a CALL CPSETC ('CTM',CVAL).
This
should only be done during a call with IFLG = 1 or 3 and, if it is done for
one of those values, it should be done for the other.

When CPCHLL is called, the internal parameter \&'PAI' will have been 
set to the index of
the appropriate contour level.  Thus, parameters associated with that level
may easily be retrieved by calls to CPGET\fIx\fR.
.L1 "CONPACK Parameter Access Subroutines"
If you are satisfied to use all the CONPACK defaults, you do
not need to set any internal parameters.  To assume more control, 
however, you must set internal
parameters.  There are four subroutines to set 
the values of internal parameters \(em
one to reset all internal parameters to their initial default values, and
one for each type of parameter: character, integer, and real.
There are three subroutines to retrieve the current values of
internal parameters.  These seven subroutines are described below.
The internal parameters are described under "CONPACK Internal Parameter
Descriptions," later in this chapter.
.L2 "Setting Internal Parameter Values"
The value of an internal parameter must be set
prior to calling any of the subroutines whose behavior is
affected by that value.  Generally, such setting can be
done prior to calling one of the initialization subroutines.
Because most internal parameters retain the values given
them from that point on, they generally need to be set only
once; there are a few exceptions, which are noted in the
internal parameter descriptions.  Some internal parameters 
are not set by the user, but by
CONPACK; it may be necessary to retrieve the value of these
parameters for use in the calling program.
.sp
It is recommended that the rest of the character string be 
used to improve the readability of your code.  For example, 
instead of just \&'ILT', use \&'ILT - INFORMATIONAL LABEL TEXT'.
.sp
The subroutine CPSETI is passed an integer value \fIi\fR.
If you are setting an integer parameter, it receives the value \fIi\fR.
If you are setting a real parameter, it receives the value
REAL(\fIi\fR).
.sp
The subroutine CPSETR is passed a real value \fIr\fR.  If you are setting
a real parameter, it receives the value \fIr\fR.
If you are setting an integer parameter,
it receives the value INT(\fIr\fR).
Note that the Fortran intrinsic INT does truncation rather than rounding.
.sp
Thus, the subroutine CPSETR is the more general subroutine;
it provides access to all real and integer parameters.
CPSETI allows for more natural access to integer parameters and to
those real parameters whose values have no fractional part.
.sp
The subroutine CPRSET
resets all parameters to their initial default values.
.sp
.ne 13
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
.nr 38 \w\fB\s11CALL CPSETC\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~~CVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
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
.if t .if \n(TW>\n(.li .tm Table at line 3903 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB\s11CALL CPSETC\fR\s12\h'|\n(41u'~~~~~(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~~CVAL)\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI \fBPNAM\fR
The internal parameter name, of type character (for example, \&'ILT').  If 
the parameter is an array, the element specified
by the current value of \&'PAI' will be the one retrieved.
.LI \fBCVAL\fR
The character value you select for the parameter.
.LE
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
.nr 38 \w\fB\s11CALL CPSETI\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~(PNAM,
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
.if t .if \n(TW>\n(.li .tm Table at line 3923 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB\s11CALL CPSETI\fR\s12\h'|\n(41u'~~~~~(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
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
.LI \fBPNAM\fR
The internal parameter name, of type character (for example, \&'ILA').  If 
the parameter is an array, the element specified
by the current value of \&'PAI' will be the one retrieved.
.LI \fBIVAL\fR
The integer value you select for the parameter.
.LE
.ne 4
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
.nr 38 \w\fB\s11CALL CPSETR\fR\s12
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
.if t .if \n(TW>\n(.li .tm Table at line 3944 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB\s11CALL CPSETR\fR\s12\h'|\n(41u'~~~~~(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
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
.sp
.in +.5i
.VL 1.2i
.LI \fBPNAM\fR
The internal parameter name, of type character (for example, \&'CIS').  If 
the parameter is an array, the element specified by the
current value of  \&'PAI' will be the one retrieved.
.LI \fBRVAL\fR
The real value you select for the parameter.
.LE
.sp 2
.in 0
\fB\s11CALL CPRSET\fR\s12~~~~~~~~~Resets all parameters 
to their initial default values.
CPRSET has 
.br
.ti 1.65i
 no arguments.
.sp
.in 0
.L2 "Retrieving Current Internal Parameter Values"
The subroutines with names of the form CPGET\fIx\fR 
retrieve the current values of CONPACK's internal parameters.
.sp
The subroutine CPGETI returns an integer value.
If you are getting the value of an integer parameter, CPGETI
returns that integer value.  If you are getting the value of a
real parameter with value \fIr\fR,
then the value returned is INT(\fIr\fR).  Note
that the Fortran intrinsic INT does truncation rather than
rounding.
.sp
The subroutine CPGETR returns a real value.  If
you are getting the value of a real parameter, CPGETR returns that real
value.  If you are getting the value of an integer parameter with
value \fIi\fR,
then the value returned is REAL(\fIi\fR).
.sp
Thus, the subroutine CPGETR is the more general routine; it
provides access to all real and integer parameters.  CPGETI 
allows for more natural access to integer parameters
and to those real parameters whose values have no fractional
part.
.sp
If you need to recover the current value of one of the internal parameters, 
use one of these calls:
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
.nr 38 \w\fB\s11CALL CPGETC\fR\s12
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~~~~~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~~~~~CVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
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
.if t .if \n(TW>\n(.li .tm Table at line 4000 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB\s11CALL CPGETC\fR\s12\h'|\n(41u'~~~~~(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~~CVAL)\h'|\n(42u'Character\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI \fBPNAM\fR
The internal parameter name, of type character (for example, \&'LLT').  If 
the parameter is an array, the array element specified by the current 
value of the parameter
\&'PAI' will be the one retrieved.
.LI \fBCVAL\fR
A character variable in which the current value of the parameter
is to be returned.
.LE
.sp 
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
.nr 38 \w\fB\s11CALL CPGETI\fR\s12
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
.if t .if \n(TW>\n(.li .tm Table at line 4022 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB\s11CALL CPGETI\fR\s12\h'|\n(41u'~~~~~(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
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
.LI \fBPNAM\fR
The internal parameter name, of type character (for example, \&'LLP').  If 
the parameter is an array, the element specified
by the current value of  \&'PAI' will be the one retrieved.
.LI \fBIVAL\fR
An integer variable in which the current value of the parameter
is to be returned.
.LE
.sp 1.5
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
.nr 38 \w\fB\s11CALL CPGETR\fR\s12
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
.if t .if \n(TW>\n(.li .tm Table at line 4043 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB\s11CALL CPGETR\fR\s12\h'|\n(41u'~~~~~(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~~~~~~RVAL)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI \fBPNAM\fR
The internal parameter name, of type character (for example, \&'SFS').  If 
the parameter is an array, the array element specified by the current 
value of the parameter \&'PAI' will be the one retrieved.
.LI \fBRVAL\fR
A real variable in which the current value of the parameter
is to be returned.
.LE
.L1 "CONPACK Internal Parameters Overview"
The behavior of CONPACK is controlled by a large number of internal 
parameters in
internal common blocks to which all of the CONPACK subroutines have access.
The table on the following pages lists the
internal parameters
and gives a phrase that briefly describes each one.
Detailed descriptions of the internal parameters
(also arranged in alphabetical order)
appear under the heading "CONPACK Internal Parameter Descriptions," which 
appears later in this chapter.
.L2 "Names of Internal Parameters"
Each of the internal parameters of CONPACK is identified by a three-character mnemonic
name.  For example, \&'NCL' is the name of the internal parameter specifying the number
of contour levels.
.L2 "Types of Internal Parameters"
Each internal parameter is intrinsically of type character,
integer, or real, but the
type of the internal parameter is not implied by the form of its name.
Read
the description of the internal parameter to determine its type,
or refer to the table on the following pages.
.L2 "Defaults for Internal Parameters"
Each internal parameter has a default value that is intended to yield a
useful behavior.
You can
modify the behavior of CONPACK subroutines by
changing the values of the appropriate internal parameters at the appropriate time.
.B
.ce
Alphabetical Listing of Internal Parameters
.R
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
.rm 80 81 82
.nr 80 0
.nr 38 \w\fBInternal
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wParameter
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'AIA'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'AIB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFA'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFP'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFT'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFW'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFX'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFY'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CIS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CIT'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CIU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLD'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLV'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CMN'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CMX'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CTM'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CWM'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'DPS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'DPU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'DPV'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'GIC'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 1.in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wBrief Description
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wArea Identifier Above
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wArea Identifier Below
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field Label Angle
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field Label Box Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field Label Color Index
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field-found Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field Label Line Width
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field Label Positioning Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field Label Size
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field Label Text String
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field Label White Space Width
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field Label X Coordinate
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wConstant-field Label Y Coordinate
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Interval Specifier
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Interval Table
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Interval Used
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Line Color Index
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Line Dash Pattern
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Line Line Width
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Level Selection Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Level Use Flags
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Level Values
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Minimum
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wContour Maximum
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCharacter Temporary
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCharacter-width Multiplier
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDash Pattern Size
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDash Pattern Use Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDash Pattern Vector Length
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wGroup Identifier for Contour Lines
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 3.25in
.if \n(81<\n(38 .nr 81 \n(38
.nr 82 0
.nr 38 \wType\fR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 38 1.25in
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 4130 file man/conpack.l is too wide - \n(TW units
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
.sp .5
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBInternal\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Parameter\h'|\n(41u'Brief Description\h'|\n(42u'Type\fR
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'AIA'\h'|\n(41u'Area Identifier Above\h'|\n(42u'Integer Array
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'AIB'\h'|\n(41u'Area Identifier Below\h'|\n(42u'Integer Array
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFA'\h'|\n(41u'Constant-field Label Angle\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFB'\h'|\n(41u'Constant-field Label Box Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFC'\h'|\n(41u'Constant-field Label Color Index\h'|\n(42u'Integer
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFF'\h'|\n(41u'Constant-field-found Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFL'\h'|\n(41u'Constant-field Label Line Width\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFP'\h'|\n(41u'Constant-field Label Positioning Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFS'\h'|\n(41u'Constant-field Label Size\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFT'\h'|\n(41u'Constant-field Label Text String\h'|\n(42u'Character
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFW'\h'|\n(41u'Constant-field Label White Space Width\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFX'\h'|\n(41u'Constant-field Label X Coordinate\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFY'\h'|\n(41u'Constant-field Label Y Coordinate\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CIS'\h'|\n(41u'Contour Interval Specifier\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CIT'\h'|\n(41u'Contour Interval Table\h'|\n(42u'Real Array
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CIU'\h'|\n(41u'Contour Interval Used\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLC'\h'|\n(41u'Contour Line Color Index\h'|\n(42u'Integer Array
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLD'\h'|\n(41u'Contour Line Dash Pattern\h'|\n(42u'Character Array
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLL'\h'|\n(41u'Contour Line Line Width\h'|\n(42u'Real Array
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLS'\h'|\n(41u'Contour Level Selection Flag\h'|\n(42u'Integer
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLU'\h'|\n(41u'Contour Level Use Flags\h'|\n(42u'Integer Array
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLV'\h'|\n(41u'Contour Level Values\h'|\n(42u'Real Array
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CMN'\h'|\n(41u'Contour Minimum\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CMX'\h'|\n(41u'Contour Maximum\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CTM'\h'|\n(41u'Character Temporary\h'|\n(42u'Character
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CWM'\h'|\n(41u'Character-width Multiplier\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'DPS'\h'|\n(41u'Dash Pattern Size\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'DPU'\h'|\n(41u'Dash Pattern Use Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'DPV'\h'|\n(41u'Dash Pattern Vector Length\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'GIC'\h'|\n(41u'Group Identifier for Contour Lines\h'|\n(42u'Integer
.sp
.fc
.nr T. 1
.T# 1
.in \n(#Iu
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-43
.in 0
.*i "continued on next page"
\s10~~~~(continued on next page)\s12
.in 0
.in +.5
.sp
.B
.ce
Alphabetical Listing of Internal Parameters
.R
.sp
.in 0
.in +.5
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
.rm 80 81 82
.nr 80 0
.nr 38 \w\fBInternal
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wParameter
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'GIL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'GIS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HIC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HIT'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLA'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLO'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLT'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLW'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLX'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HLY'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ILA'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ILB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ILC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ILL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ILP'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ILS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ILT'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ILW'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ILX'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ILY'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'IWM'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'IWU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LBC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LBX'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LBY'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LIS'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 1.in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wBrief Description
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wGroup Identifier for Label Boxes
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wGroup Identifier for Strips
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh Label Color Index
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh Label Text String
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh/Low Label Angle
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh/Low Label Box Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh/Low Label Color Index
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh/Low Label Line Width
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh/Low Label Overlap Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh/Low Label Size
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh/Low Label Text Strings
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh/Low Label White Space Width
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh/Low Search Radius in X
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh/Low Search Radius in Y
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational Label Angle
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational Label Box Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational Label Color Index
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational Label Line Width
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational Label Positioning Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational Label Size
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational Label Text String
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational Label White Space Width
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational Label X Coordinate
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational Label Y Coordinate
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger Workspace for Masking
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInteger Workspace Usage
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLabel Box Color Index
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLabel Box X Coordinate
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLabel Box Y Coordinate
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLabel Interval Specifier
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 3.25in
.if \n(81<\n(38 .nr 81 \n(38
.nr 82 0
.nr 38 \wType\fR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
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
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 38 1.25in
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 4187 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBInternal\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Parameter\h'|\n(41u'Brief Description\h'|\n(42u'Type\fR
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'GIL'\h'|\n(41u'Group Identifier for Label Boxes\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'GIS'\h'|\n(41u'Group Identifier for Strips\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HIC'\h'|\n(41u'High Label Color Index\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HIT'\h'|\n(41u'High Label Text String\h'|\n(42u'Character
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLA'\h'|\n(41u'High/Low Label Angle\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLB'\h'|\n(41u'High/Low Label Box Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLC'\h'|\n(41u'High/Low Label Color Index\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLL'\h'|\n(41u'High/Low Label Line Width\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLO'\h'|\n(41u'High/Low Label Overlap Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLS'\h'|\n(41u'High/Low Label Size\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLT'\h'|\n(41u'High/Low Label Text Strings\h'|\n(42u'Character
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLW'\h'|\n(41u'High/Low Label White Space Width\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLX'\h'|\n(41u'High/Low Search Radius in X\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HLY'\h'|\n(41u'High/Low Search Radius in Y\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ILA'\h'|\n(41u'Informational Label Angle\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ILB'\h'|\n(41u'Informational Label Box Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ILC'\h'|\n(41u'Informational Label Color Index\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ILL'\h'|\n(41u'Informational Label Line Width\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ILP'\h'|\n(41u'Informational Label Positioning Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ILS'\h'|\n(41u'Informational Label Size\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ILT'\h'|\n(41u'Informational Label Text String\h'|\n(42u'Character
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ILW'\h'|\n(41u'Informational Label White Space Width\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ILX'\h'|\n(41u'Informational Label X Coordinate\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ILY'\h'|\n(41u'Informational Label Y Coordinate\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'IWM'\h'|\n(41u'Integer Workspace for Masking\h'|\n(42u'Integer
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'IWU'\h'|\n(41u'Integer Workspace Usage\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LBC'\h'|\n(41u'Label Box Color Index\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LBX'\h'|\n(41u'Label Box X Coordinate\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LBY'\h'|\n(41u'Label Box Y Coordinate\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LIS'\h'|\n(41u'Label Interval Specifier\h'|\n(42u'Integer
.sp
.fc
.nr T. 1
.T# 1
.in \n(#Iu
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-43
.*i "continued on next page"
\s10(continued on next page)\s12
.sp
.in 0
.in +.5
.B
.ce
Alphabetical Listing of Internal Parameters
.R
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
.rm 80 81 82
.nr 80 0
.nr 38 \w\fBInternal
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wParameter
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LIT'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LIU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLA'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLO'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLP'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLT'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLW'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LOC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LOT'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'MAP'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NCL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NEL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NET'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NEU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NLS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NLZ'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NOF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NSD'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'NVS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ORV'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PAI'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC1'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC2'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC3'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC4'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC5'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 1.in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wBrief Description
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLabel Interval Table
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLabel Interval Used
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine Label Angle
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine Label Box Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine Label Color Index
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine Label Line Width
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine Label Orientation
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine Label Positioning
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine Label Size
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine Label Text String
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLine Label White Space
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLow Label Color Index
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLow Label Text String
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMapping Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNumber of Contour Levels
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNumeric Exponent Length
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNumeric Exponent Type
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNumeric Exponent Use Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNumeric Leftmost Significant Digit Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNumeric Leading Zero Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNumeric Omission Flags
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNumber of Significant Digits
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNumber of Vertical Strips
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wOut-of-range Value
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wParameter Array Index
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPenalty Scheme Constant 1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPenalty Scheme Constant 2
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPenalty Scheme Constant 3
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPenalty Scheme Constant 4
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPenalty Scheme Constant 5
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 3.25in
.if \n(81<\n(38 .nr 81 \n(38
.nr 82 0
.nr 38 \wType\fR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
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
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 38 1.25in
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 4241 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBInternal\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Parameter\h'|\n(41u'Brief Description\h'|\n(42u'Type\fR
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LIT'\h'|\n(41u'Label Interval Table\h'|\n(42u'Integer Array
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LIU'\h'|\n(41u'Label Interval Used\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLA'\h'|\n(41u'Line Label Angle\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLB'\h'|\n(41u'Line Label Box Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLC'\h'|\n(41u'Line Label Color Index\h'|\n(42u'Integer Array
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLL'\h'|\n(41u'Line Label Line Width\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLO'\h'|\n(41u'Line Label Orientation\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLP'\h'|\n(41u'Line Label Positioning\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLS'\h'|\n(41u'Line Label Size\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLT'\h'|\n(41u'Line Label Text String\h'|\n(42u'Character Array
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLW'\h'|\n(41u'Line Label White Space\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LOC'\h'|\n(41u'Low Label Color Index\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LOT'\h'|\n(41u'Low Label Text String\h'|\n(42u'Character
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'MAP'\h'|\n(41u'Mapping Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NCL'\h'|\n(41u'Number of Contour Levels\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NEL'\h'|\n(41u'Numeric Exponent Length\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NET'\h'|\n(41u'Numeric Exponent Type\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NEU'\h'|\n(41u'Numeric Exponent Use Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NLS'\h'|\n(41u'Numeric Leftmost Significant Digit Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NLZ'\h'|\n(41u'Numeric Leading Zero Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NOF'\h'|\n(41u'Numeric Omission Flags\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NSD'\h'|\n(41u'Number of Significant Digits\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'NVS'\h'|\n(41u'Number of Vertical Strips\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ORV'\h'|\n(41u'Out-of-range Value\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PAI'\h'|\n(41u'Parameter Array Index\h'|\n(42u'Integer
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC1'\h'|\n(41u'Penalty Scheme Constant 1\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC2'\h'|\n(41u'Penalty Scheme Constant 2\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC3'\h'|\n(41u'Penalty Scheme Constant 3\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC4'\h'|\n(41u'Penalty Scheme Constant 4\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC5'\h'|\n(41u'Penalty Scheme Constant 5\h'|\n(42u'Real
.sp
.fc
.nr T. 1
.T# 1
.in \n(#Iu
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-43
.*i "continued on next page"
\s10(continued on next page)\s12
.sp
.in 0
.in +.5
.ne 40
.B
.ce
Alphabetical Listing of Internal Parameters
.R
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
.rm 80 81 82
.nr 80 0
.nr 38 \w\fBInternal
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wParameter
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PC6'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PIC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PIE'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PW1'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PW2'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PW3'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PW4'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'RC1'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'RC2'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'RC3'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'RWC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'RWG'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'RWM'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'RWU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'SET'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'SFS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'SFU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'SPV'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'SSL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'T2D'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'T3D'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'VPB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'VPL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'VPR'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'VPS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'VPT'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'WDB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'WDL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'WDR'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'WDT'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 1.in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wBrief Description
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPenalty Scheme Constant 6
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPoint Interpolation Flag for Contours
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPoint Interpolation Flag for Edges
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPenalty Scheme Weight 1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPenalty Scheme Weight 2
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPenalty Scheme Weight 3
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPenalty Scheme Weight 4
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wRegular Scheme Constant 1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wRegular Scheme Constant 2
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wRegular Scheme Constant 3
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal Workspace for Contours
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal Workspace for Gradients
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal Workspace for Masking
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wReal Workspace Usage
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wDo-SET-call Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wScale Factor Selector
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wScale Factor Used
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSpecial Value
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSmoothed Segment Length
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wTension on 2-Dimensional Splines
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wTension on 3-Dimensional Splines
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wViewport Bottom
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wViewport Left
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wViewport Right
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wViewport Shape
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wViewport Top
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wWindow Bottom
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wWindow Left
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wWindow Right
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wWindow Top
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 3.25in
.if \n(81<\n(38 .nr 81 \n(38
.nr 82 0
.nr 38 \wType\fR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
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
.nr 38 \wInteger
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
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 38 1.25in
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 4296 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBInternal\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Parameter\h'|\n(41u'Brief Description\h'|\n(42u'Type\fR
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PC6'\h'|\n(41u'Penalty Scheme Constant 6\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PIC'\h'|\n(41u'Point Interpolation Flag for Contours\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PIE'\h'|\n(41u'Point Interpolation Flag for Edges\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PW1'\h'|\n(41u'Penalty Scheme Weight 1\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PW2'\h'|\n(41u'Penalty Scheme Weight 2\h'|\n(42u'Real
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PW3'\h'|\n(41u'Penalty Scheme Weight 3\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PW4'\h'|\n(41u'Penalty Scheme Weight 4\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'RC1'\h'|\n(41u'Regular Scheme Constant 1\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'RC2'\h'|\n(41u'Regular Scheme Constant 2\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'RC3'\h'|\n(41u'Regular Scheme Constant 3\h'|\n(42u'Real
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'RWC'\h'|\n(41u'Real Workspace for Contours\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'RWG'\h'|\n(41u'Real Workspace for Gradients\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'RWM'\h'|\n(41u'Real Workspace for Masking\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'RWU'\h'|\n(41u'Real Workspace Usage\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'SET'\h'|\n(41u'Do-SET-call Flag\h'|\n(42u'Integer
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'SFS'\h'|\n(41u'Scale Factor Selector\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'SFU'\h'|\n(41u'Scale Factor Used\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'SPV'\h'|\n(41u'Special Value\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'SSL'\h'|\n(41u'Smoothed Segment Length\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'T2D'\h'|\n(41u'Tension on 2-Dimensional Splines\h'|\n(42u'Real
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'T3D'\h'|\n(41u'Tension on 3-Dimensional Splines\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'VPB'\h'|\n(41u'Viewport Bottom\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'VPL'\h'|\n(41u'Viewport Left\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'VPR'\h'|\n(41u'Viewport Right\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'VPS'\h'|\n(41u'Viewport Shape\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'VPT'\h'|\n(41u'Viewport Top\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'WDB'\h'|\n(41u'Window Bottom\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'WDL'\h'|\n(41u'Window Left\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'WDR'\h'|\n(41u'Window Right\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'WDT'\h'|\n(41u'Window Top\h'|\n(42u'Real
.sp
.fc
.nr T. 1
.T# 1
.in \n(#Iu
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-43
.*i "continued on next page"
\s10(continued on next page)\s12
.sp
.in 0
.in +.5
.ne 40
.B
.ce
Alphabetical Listing of Internal Parameters
.R
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
.rm 80 81 82
.nr 80 0
.nr 38 \w\fBInternal
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wParameter
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'WSO'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'XC1'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'XCM'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'YC1'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'YCN'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ZD1'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ZDM'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ZDN'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ZDS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ZDU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ZDV'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ZMN'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'ZMX'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 1.in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wBrief Description
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wWorkspace Overflow Flag
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wX Coordinate at Index 1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wX Coordinate at Index M
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wY Coordinate at Index 1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wY Coordinate at Index N
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wZDAT First Dimension
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wZ Data Array Dimension M
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wZ Data Array Dimension N
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wZDAT Dimension Selector
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wZ Data Value, Unscaled
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wZ Data Value
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wZ Minimum Value
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wZ Maximum Value
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 3.25in
.if \n(81<\n(38 .nr 81 \n(38
.nr 82 0
.nr 38 \wType\fR
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
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 38 1.25in
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 4331 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBInternal\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Parameter\h'|\n(41u'Brief Description\h'|\n(42u'Type\fR
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'WSO'\h'|\n(41u'Workspace Overflow Flag\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'XC1'\h'|\n(41u'X Coordinate at Index 1\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'XCM'\h'|\n(41u'X Coordinate at Index M\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'YC1'\h'|\n(41u'Y Coordinate at Index 1\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'YCN'\h'|\n(41u'Y Coordinate at Index N\h'|\n(42u'Real
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ZD1'\h'|\n(41u'ZDAT First Dimension\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ZDM'\h'|\n(41u'Z Data Array Dimension M\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ZDN'\h'|\n(41u'Z Data Array Dimension N\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ZDS'\h'|\n(41u'ZDAT Dimension Selector\h'|\n(42u'Integer
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ZDU'\h'|\n(41u'Z Data Value, Unscaled\h'|\n(42u'Real
.sp
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ZDV'\h'|\n(41u'Z Data Value\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ZMN'\h'|\n(41u'Z Minimum Value\h'|\n(42u'Real
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'ZMX'\h'|\n(41u'Z Maximum Value\h'|\n(42u'Real
.sp
.fc
.nr T. 1
.T# 1
.in \n(#Iu
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-23
.sp
.L2 "Access to Internal Parameters"
To set
the value of an internal parameter, use one of the three subroutines CPSETC,
CPSETI, or CPSETR.
Similarly, 
to retrieve the current value of an internal parameter, use
one of the three subroutines CPGETC, CPGETI, and CPGETR.
.sp
In general, once an internal parameter is given a particular value, it retains that
value until it is given a new value.
With only a few exceptions (which are noted in the
documentation for individual internal parameters), there
is no automatic resetting of internal parameters at any time.
For details on using CPSETC, CPSETI, CPSETR, CPGETC, CPGETI and CPGETR, 
see "CONPACK Parameter Access Subroutines," earlier
in this chapter.
.L2 "Use of Internal Parameter Names as Arguments"
Only the first three characters of the argument specifying the internal
parameter name are examined by
the internal parameter access subroutines.
We strongly recommend that
you include
additional characters in order to make the code more readable.  For
example, a call to set the number of contour levels might read
.>>
.sF
CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',13)
.eF
.<<
The additional characters make it much easier to remember what the
call is
intended to do.
.L2 "Automatic Type Conversion of Internal Parameter Values"
Normally, you would use CPSETI to set internal parameters of type integer and CPSETR
to set internal parameters of type real.  However, since automatic conversion is done
as necessary, this need not be the case.  You could, for example, use either
of the two statements
.>>
.sF
CALL CPSETI ('ILA - INFORMATIONAL LABEL ANGLE',30)
CALL CPSETR ('ILA - INFORMATIONAL LABEL ANGLE',30.)
.eF
.<<
to set the informational-label angle (which is intrinsically real) to 30
degrees.
Similarly,
you could use either of the two statements
.>>
.sF
CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',18)
CALL CPSETR ('NCL - NUMBER OF CONTOUR LEVELS',18.)
.<<
.eF
to set the number of contour levels (which is intrinsically integer) to 18.
In this case, the first of the two statements seems to make
better sense.  If
you are giving a real internal parameter a non-integral value
(like 3.14159), then you must use CPSETR.
.sp
Certain parameters of type character, representing dash patterns, may be set
using either a call to CPSETC or CPSETI.
In CPSETI, the low-order
sixteen bits of the integer are interpreted and used to construct a character
dash pattern (by mapping 0's into apostrophes and 1's into dollar signs).
.L2 "Automatic Restriction of Internal Parameter Values"
Some internal parameters may take on any possible value of the proper type.  For
example, \&'CFA', which specifies the angle at which the constant-field label
is to be written, may be given any real value.  Other internal parameters may only
be given values in a restricted range.  For example, \&'CFB', which specifies
whether the constant-field label is to be boxed, and, if so, in what manner,
is restricted to the integer values 0, 1, 2, and 3.  The 
internal-parameter-setting
routines attempt to enforce such restrictions.  Attempting to set \&'CFB' to
a non-integral value will result in truncation of the fractional part;
attempting to give it a value less than 0 will result in giving it the value
0 and attempting to give it a value greater than 3 will result in giving it
the value 3.  Other internal parameters are treated similarly.
.L2 "Internal Parameter Arrays"
Some of the internal parameters are actually arrays.  For example, the
internal parameter \&'CLV',
which specifies contour levels, is actually a 256-element array.  In order to
access the Ith element of such an internal parameter array, you must specify
the value of the index I.  This is done by having one internal parameter, named \&'PAI',
serve as an internal parameter array index.  Thus, 
to set the 10th contour level to
196.3, you would use the following code:
.>>
.sF
CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',10)
CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',196.3)
.eF
.<<
In a few cases, negative values of \&'PAI' must be used.  These 
are pointed out 
in the descriptions of individual parameters.
.L1 "CONPACK Internal Parameter Descriptions"
The internal parameter descriptions follow, in alphabetical order.
.sp
.VL 1.2i
.LI "\fB'AIA'\fR"
\fBArea Identifier Above.\fR  Each element of the internal parameter array \&'AIA' is an area identifier for the area
above the contour level specified by the corresponding element of the
internal parameter array \&'CLV'.  The corresponding element of the internal parameter array
\&'AIB' is an area identifier for the area below that level.  If, for a
particular contour level, both \&'AIA' and \&'AIB' are zero, that level is
ignored by the subroutine CPCLAM; otherwise, contour lines at that level are
added to the area map and the given values of \&'AIA' and \&'AIB' are used as
left and right area identifiers.
.sp
There are three special elements in the internal parameter array \&'AIA', corresponding
to \&'PAI' values of -1, -2, and -3:
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
Specifies an area identifier for
the area outside the edge of the grid.
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
Specifies an area identifier for any
area filled with special values.
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
Specifies an area identifier for any
area in which the mapping subroutine CPMPXY returns the out-of-range value
\&'ORV'.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB'PAI'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-3
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \w'AIA'\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
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
.if t .if \n(TW>\n(.li .tm Table at line 4465 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB'PAI'\h'|\n(41u''AIA'\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1\h'|\n(41u'
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
\&\h'|\n(40u'-2\h'|\n(41u'
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
\&\h'|\n(40u'-3\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-20
.sp
If contour levels are chosen automatically, rather than being supplied by
you, the value supplied for the Ith element of \&'AIA' is I+1 and the value
supplied for the Ith element of \&'AIB' is I.
.tA "Integer Array" "See description above."
.LI "\fB'AIB'\fR"
\fBArea Identifier Below.\fR  See the preceding description of \&'AIA'.
.tA "Integer Array" "See description of \&'AIA'."
.ne 6
.LI "\fB'CFA'\fR"
\fBConstant-field Label Angle.\fR  \&'CFA' specifies the
angle (in degrees counterclockwise from a
vector pointing to the right) at which a constant-field label is to be
written.
.tA "Real" "0."
.br
.ne 15
.LI "\fB'CFB'\fR"
\fBConstant-field Label Box Flag.\fR
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
Constant-field
label is not boxed at all.
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
Perimeter of the box is drawn (in the same
color as the label) after the label is drawn.
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
Box is filled (in the color specified by \&'LBC')
before the label is
drawn.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wBoth of the above.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
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
.if t .if \n(TW>\n(.li .tm Table at line 4508 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3\h'|\n(41u'Both of the above.
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.tA "Integer" "0"
.LI "\fB'CFC'\fR"
\fBConstant-field Label Color Index.\fR
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
Constant-field label and the box
around it (if any) are drawn in the color specified by the
current text color
index.
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
Specifies the
desired color index for the label and the box.  If a box is drawn around the
label, it is made the same color as the label itself.
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
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLE 0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGE 0.
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
.if t .if \n(TW>\n(.li .tm Table at line 4530 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LE 0.\h'|\n(41u'
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
\&\h'|\n(40u'GE 0.\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-17
.tA "Integer" "-1"
.LI "\fB'CFF'\fR"
\fBConstant-field-found Flag.\fR
\&'CFF' may not be set by you; its retrieved value will be
non-zero if and only if CPRECT or CPSPRS detected a constant field.
.tA "Integer" "0 (prior to any call to"
.ti +2.6i
CPRECT or CPSPRS)
.br
.ne 12
.LI "\fB'CFL'\fR"
\fBConstant-field Label Line Width.\fR
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
Line width is not set before drawing a box
around the constant-field label.
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
Specifies
the desired width, as a multiple of the normal line width.
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
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLE 0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0.
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
.if t .if \n(TW>\n(.li .tm Table at line 4558 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LE 0.\h'|\n(41u'
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
\&\h'|\n(40u'>0.\h'|\n(41u'
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
.tA "Real" "0."
.LI "\fB'CFP'\fR"
\fBConstant-field Label Positioning Flag.\fR  \&'CFP'
determines how the constant-field label is to be positioned.
There are nine possible values, each of which specifies a point of the label
box that is to lie on the point defined by \&'CFX' and \&'CFY':  
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
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w-4
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w-3
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w-2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w-1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w+1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w+2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w+3
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w+4
.if \n(31<\n(38 .nr 31 \n(38
.80
.rm 80
.nr 60 \n(31
.nr 38 \n(60+\n(32
.if \n(38>\n(80 .nr 80 \n(38
.if \n(38<\n(80 .nr 60 +(\n(80-\n(38)/2
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLower left-hand corner of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCenter of bottom of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLower right-hand corner of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCenter of left edge of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCenter of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCenter of right edge of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wUpper left-hand corner of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCenter of top edge of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wUpper right-hand corner of label box.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 60 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 4588 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ta \n(60u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-4\h'|\n(41u'Lower left-hand corner of label box.
.sp
.ta \n(60u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-3\h'|\n(41u'Center of bottom of label box.
.sp
.ta \n(60u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-2\h'|\n(41u'Lower right-hand corner of label box.
.sp
.ta \n(60u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1\h'|\n(41u'Center of left edge of label box.
.sp
.ta \n(60u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'Center of label box.
.sp
.ta \n(60u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+1\h'|\n(41u'Center of right edge of label box.
.sp
.ta \n(60u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+2\h'|\n(41u'Upper left-hand corner of label box.
.sp
.ta \n(60u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+3\h'|\n(41u'Center of top edge of label box.
.sp
.ta \n(60u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+4\h'|\n(41u'Upper right-hand corner of label box.
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.sp
Left, right, bottom, and top are defined from the viewpoint of someone
reading the label right side up.
.tA "Integer" "0"
.ne6
.LI "\fB'CFS'\fR"
\fBConstant-field Label Size.\fR
\&'CFS' specifies the nominal size (width) of a character in the
constant-field label, as a fraction of the width of the viewport.  This
nominal size is multiplied by \&'CWM'.
.tA "Real" ".012"
.LI "\fB'CFT'\fR"
\fBConstant-field Label Text String.\fR
\&'CFT' specifies the text of the constant-field label, which is
written when a constant data field is detected; it is a character string of
40 characters or less.  The embedded string \&'$ZDV$' will be replaced by the
numeric value of the field.
.sp
If \&'CFT' is given the value \&' \&', (a single blank), the constant-field
label will not be written.
.tA "Character" "'CONSTANT FIELD -"
.ti +2.6i
VALUE IS $ZDV$'
.LI "\fB'CFW'\fR"
\fBConstant-field Label White Space Width.\fR
\&'CFW' specifies the nominal width of white space to be left
around the constant-field label, as a fraction of the width of the viewport.
This nominal width is multiplied by \&'CWM'.
.tA "Real" ".005"
.LI "\fB'CFX'\fR"
\fBConstant-field Label X Coordinate.\fR
\&'CFX' specifies the X coordinate of the basepoint of the
constant-field label.  The given value is mapped linearly onto the viewport:
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
Beyond the left edge of the viewport.
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
Left edge of the viewport.
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
Between the left edge and the right edge of the viewport.
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
Right edge of the
viewport.  
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
Beyond the right edge of the viewport.
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.<'CFX'<1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>1.
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
.if t .if \n(TW>\n(.li .tm Table at line 4648 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'<0.\h'|\n(41u'
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
\&\h'|\n(40u'0.\h'|\n(41u'
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
\&\h'|\n(40u'0.<'CFX'<1.\h'|\n(41u'
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
\&\h'|\n(40u'1.\h'|\n(41u'
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
\&\h'|\n(40u'>1.\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-25
.sp
.tA "Real" ".5"
.LI "\fB'CFY'\fR"
\fBConstant-field Label Y Coordinate.\fR
\&'CFY' specifies the Y coordinate of the basepoint of the
constant-field label.  The given value is mapped linearly onto the viewport:
.sp
.ne9
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
Below the bottom edge of the viewport.
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
Bottom edge of the viewport.
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
Between the bottom edge and the top edge of the viewport.
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
Top edge of the
viewport.  
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
Above the top edge of the viewport.
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.LE  'CFY'LE 1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>1.
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
.if t .if \n(TW>\n(.li .tm Table at line 4683 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'<0.\h'|\n(41u'
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
\&\h'|\n(40u'0.\h'|\n(41u'
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
\&\h'|\n(40u'0.LE  'CFY'LE 1.\h'|\n(41u'
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
\&\h'|\n(40u'1.\h'|\n(41u'
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
\&\h'|\n(40u'>1.\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-26
.tA "Real" ".5"
.LI "\fB'CIS'\fR"
\fBContour Interval Specifier.\fR
See the description of \&'CLS', below.  When \&'CLS' is greater than zero, \&'CIS'
is used:
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
is to choose the contour interval. (See also the description of the internal
parameter arrays
\&'CIT' and \&'LIT').
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
The actual
contour interval to be used; in this case, \&'LIS' may be given a non-zero
value \fIn\fR to specify that every \fIn\fRth contour level should be labeled.
See also the descriptions of \&'CMN' and \&'CMX'.
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
.nr 38 \wLE 0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0.
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
.if t .if \n(TW>\n(.li .tm Table at line 4707 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'LE 0.\h'|\n(41u'
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
\&\h'|\n(40u'>0.\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-17
.tA "Real" "0."
.LI "\fB'CIT'\fR"
\fBContour Interval Table.\fR
Each non-zero element of the ten-element internal parameter array \&'CIT' is a real
number greater than or equal to 1. and less than 10.  When CONPACK picks the
contour interval, it chooses a number of the form \fIc\fR*10**\fIk\fR, where \fIc\fR is one
of the elements of \&'CIT' and \fIk\fR is an integer.  The non-zero elements of
\&'CIT' must be sorted in ascending order and appear at the beginning of the
array.
.sp
\&'CIT' and \&'LIT' are used together.
Each element of \&'CIT' specifies a set of contour levels that
may be used and the corresponding element of \&'LIT'
specifies which of those levels are to be labeled.
A contour at \fIn\fR*\fIc\fR*10**\fIk\fR is labeled
if \fIn\fR is a multiple
of \fIl\fR, where \fIl\fR is the element of \&'LIT' corresponding to the
element \fIc\fR of
\&'CIT'.  For example, if the first element of \&'CIT' is 1.\fR and the
first
element of \&'LIT' is 5, then CONPACK is allowed to use contour levels 1.,
2., 3., 4., etc., with labels at 5., 10., 15., etc.

The default contents of \&'CIT' and \&'LIT' are as follows:
.sp
.ne 12
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
.rm 80 81 82
.nr 80 0
.nr 38 \w\fB'PAI'
.if \n(80<\n(38 .nr 80 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w3
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w4
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w6
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w7
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w8
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w9
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w10
.if \n(31<\n(38 .nr 31 \n(38
.80
.rm 80
.nr 60 \n(31
.nr 38 \n(60+\n(32
.if \n(38>\n(80 .nr 80 \n(38
.if \n(38<\n(80 .nr 60 +(\n(80-\n(38)/2
.nr 81 0
.nr 38 \w'CIT'
.if \n(81<\n(38 .nr 81 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.0
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.0
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.5
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w4
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.0
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.0
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.0
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.0
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.0
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.0
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.0
.if \n(32<\n(38 .nr 32 \n(38
.81
.rm 81
.nr 61 \n(31
.nr 38 \n(61+\n(32
.if \n(38>\n(81 .nr 81 \n(38
.if \n(38<\n(81 .nr 61 +(\n(81-\n(38)/2
.nr 82 0
.nr 38 \w'LIT'\fR
.if \n(82<\n(38 .nr 82 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w4
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.82
.rm 82
.nr 62 \n(31
.nr 38 \n(62+\n(32
.if \n(38>\n(82 .nr 82 \n(38
.if \n(38<\n(82 .nr 62 +(\n(82-\n(38)/2
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 60 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 61 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 62 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 4749 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB'PAI'\h'|\n(41u''CIT'\h'|\n(42u''LIT'\fR
.ta \n(60u \n(61u \n(81u \n(62u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'1.0\h'|\n(42u'5
.ta \n(60u \n(61u \n(81u \n(62u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2\h'|\n(41u'2.0\h'|\n(42u'5
.ta \n(60u \n(61u \n(81u \n(62u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3\h'|\n(41u'2.5\h'|\n(42u'4
.ta \n(60u \n(61u \n(81u \n(62u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'4\h'|\n(41u'4.0\h'|\n(42u'5
.ta \n(60u \n(61u \n(81u \n(62u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'5\h'|\n(41u'5.0\h'|\n(42u'5
.ta \n(60u \n(61u \n(81u \n(62u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'6\h'|\n(41u'0.0\h'|\n(42u'0
.ta \n(60u \n(61u \n(81u \n(62u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'7\h'|\n(41u'0.0\h'|\n(42u'0
.ta \n(60u \n(61u \n(81u \n(62u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'8\h'|\n(41u'0.0\h'|\n(42u'0
.ta \n(60u \n(61u \n(81u \n(62u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'9\h'|\n(41u'0.0\h'|\n(42u'0
.ta \n(60u \n(61u \n(81u \n(62u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'10\h'|\n(41u'0.0\h'|\n(42u'0
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-15
.tA "Real Array" "See description above."
.LI "\fB'CIU'\fR"
\fBContour Interval Used.\fR
\&'CIU' is normally intended for output only.  When the selection
of the contour interval is left up to CONPACK, \&'CIU' is the value
it chooses.  When contour levels are completely set by you, the value of
\&'CIU' should be set as well, for two reasons:
.AL
.LI
to make the desired value
appear in an informational label (in place of the embedded string \&'$CIU$'),
and
.LI
so that it may be used by the penalty scheme for positioning labels.
.LE
.ne 12
.sp
The setting of \&'CIU' must be done after setting the contour levels.
(As a side effect of the setting of element 1 of \&'CLV', \&'CIU' is set to zero.)
If
you supply contour levels, but do not supply the value of \&'CIU', and the
penalty scheme is used to position labels, the required contour interval is
estimated.
In certain situations, this can lead to problems (if, for example,
the same contour level appears twice in \&'CLV', once to force lines at that
level to be drawn and once to force that level to be used as the boundary for
a shaded area).
.tA "Real" "0."
.LI "\fB'CLC'\fR"
\fBContour Line Color Index.\fR
Each element of the internal parameter array \&'CLC', if greater than or equal to zero,
is a color index for contour lines at the level specified by the associated
element of \&'CLV'.  A value less than zero implies that the lines will be
drawn in the color specified by the current polyline color index.
.sp
There are three special elements in the internal parameter array \&'CLC', 
corresponding to \&'PAI' values of -1, -2, and -3:
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
Specifies a color index for the
edge of the grid.
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
Specifies a color index for the edge of any area filled
with special values.
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
Specifies a color index for the edge of any area in
which the mapping subroutine CPMPXY returns the out-of-range value
\&'ORV'.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB'PAI'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-3
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \w'CLC'\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
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
.if t .if \n(TW>\n(.li .tm Table at line 4808 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB'PAI'\h'|\n(41u''CLC'\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1\h'|\n(41u'
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
\&\h'|\n(40u'-2\h'|\n(41u'
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
\&\h'|\n(40u'-3\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-20
.tA "Integer Array" "See description above."
.LI "\fB'CLD'\fR"
\fBContour Line Dash Pattern.\fR
Each element of the internal parameter array \&'CLD' is a dash 
pattern (as expected by
the utility DASHCHAR) to be used, when \&'DPU' is non-zero, to draw contour
lines at the level specified by the associated element of the contour level array \&'CLV'.
Elements of \&'CLD' may be set using a call to CPSETI, with a sixteen-bit
integer as the second argument, or using a call to CPSETC, with a character
string of 32 or fewer characters as the second argument.  In either case, the
result will be a character string internally.
.sp
There are three special elements in the internal parameter array \&'CLD', corresponding
to \&'PAI' values of -1, -2, and -3:
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
Specifies a dash pattern for the
edge of the grid.
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
Specifies a dash pattern for the edge of any area filled
with special values.
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
Specifies a dash pattern for the edge of any area in
which the mapping subroutine CPMPXY returns the out-of-range value
\&'ORV'.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\&'PAI'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-3
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \w\&'CLD'\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
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
.if t .if \n(TW>\n(.li .tm Table at line 4844 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB\&'PAI'\h'|\n(41u'\&'CLD'\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1\h'|\n(41u'
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
\&\h'|\n(40u'-2\h'|\n(41u'
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
\&\h'|\n(40u'-3\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-20
.sp
.ne 2
When CONPACK picks the contour levels, the default value supplied for each
associated dash pattern is the character constant \&'$$$$$$$$$$$$$$$$'.  This
is also the default value for each of the three special elements.
.tA "Character Array" "'$$$$$$$$$$$$$$$$'"
.sp
.LI "\fB'CLL'\fR"
\fBContour Line Line Width.\fR
Each element of the internal parameter array \&'CLL' specifies the line
width used to 
draw contour lines at the level specified by the associated element of
the contour level array \&'CLV'.  Each is expressed as a multiple of the 
normal line width; values less than or equal to zero imply that line width should not
be set.
.sp
There are three special elements in the internal parameter array \&'CLL', corresponding
to \&'PAI' values of -1, -2, and -3:
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
Specifies a line width for the
edge of the grid.
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
Specifies a line width for the edge of any area filled
with special values.
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
Specifies a line width for the edge of any area in
which the mapping subroutine CPMPXY returns the out-of-range value.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\&'PAI'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-3
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \w\&'CLL'\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
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
.if t .if \n(TW>\n(.li .tm Table at line 4883 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB\&'PAI'\h'|\n(41u'\&'CLL'\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1\h'|\n(41u'
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
\&\h'|\n(40u'-2\h'|\n(41u'
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
\&\h'|\n(40u'-3\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-19
.sp
When CONPACK picks the contour levels, the default value supplied for each
associated line width is 0.  This is also the default value for each of the
three special elements.
.tA "Real Array" "See description above."
.LI "\fB'CLS'\fR"
\fBContour Level Selection Flag.\fR  \&'CLS' specifies how contour
levels are to be selected.  Set the value of \&'CLS' before
calling CPRECT or CPSPRS, as follows:
.sp
.ne 8
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
CONPACK will not pick contour levels at all; the
current values of the internal parameters \&'NCL', \&'CLV', and
associated arrays will not
be changed.  They will thus retain the values chosen by CONPACK during a
previous call or the values supplied by you.
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
CONPACK will generate \fIn\fR
contour levels, splitting the range from the minimum field value to the
maximum field value into \fIn\fR+1 equal intervals.
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
If \&'CIS' is less than or equal to zero,
CONPACK will use values of the form \fIb\fR*\fIk\fR,
where \fIb\fR is a
base value chosen by CONPACK and \fIk\fR is an integer.  The base value
\fIb\fR
will be a \fInice value\fR (as defined by the contents of
the internal parameter array \&'CIT'), chosen in such a way as to give 
at least \fIn\fR
contour
levels (with the default contents of the array \&'CIT', you may get as many
as 2\fIn\fR levels).  
.sp
If \&'CIS' is greater than zero and \&'CMN' is greater than \&'CMX',
CONPACK will use values of the form \&'CIS'*\fIk\fR,
where \fIk\fR is an integer.
.sp
If \&'CIS' is greater than zero and \&'CMN' is less than or equal to \&'CMX',
CONPACK will use values of the form \&'CMN'+'CIS'*\fIk\fR
which are greater than or equal to \&'CMN' and less than or equal to \&'CMX',
where \fIk\fR is an integer.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fI-n\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fI+n\fR
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
.if t .if \n(TW>\n(.li .tm Table at line 4936 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'\fI-n\fR\h'|\n(41u'
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
\&\h'|\n(40u'\fI+n\fR\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-41
.tA "Integer" "16"
.LI "\fB'CLU'\fR"
\fBContour Level Use Flags.\fR
Each element of the internal parameter array \&'CLU' indicates how
the associated contour
level, in the internal parameter array \&'CLV', is to be used.
.sp .5
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
No contour line is to be drawn at the associated level.
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
The line is to be drawn without labels.
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
The labels are to be
drawn, but not the line.
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
Both the line and the labels
are to be drawn.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3
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
.if t .if \n(TW>\n(.li .tm Table at line 4966 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp .5
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
\&\h'|\n(40u'3\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.sp
There are three special elements in the internal parameter array \&'CLU', corresponding
to \&'PAI' values of -1, -2, and -3:
.sp
.ne 12
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
Specifies a flag for the edge of
the grid.
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
Specifies a flag for the edge of any area filled with special
values.
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
Specifies a flag for the edge of any area in which the mapping
routine CPMPXY returns the out-of-range value.  
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fB\&'PAI'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-3
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \w\&'CLU'\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
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
.if t .if \n(TW>\n(.li .tm Table at line 4991 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fB\&'PAI'\h'|\n(41u'\&'CLU'\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1\h'|\n(41u'
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
\&\h'|\n(40u'-2\h'|\n(41u'
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
\&\h'|\n(40u'-3\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-19
.sp
In each case, if the flag
is zero, the associated edge is not drawn; otherwise, the associated edge is
drawn.
.sp
When CONPACK chooses the contour levels, the associated elements of \&'CLU'
are given one of the values 1 or 3, depending on whether the line is
to be labeled or not.  The default values of the special elements are all
zeros.
.tA "Integer Array" "See description above."
.LI "\fB'CLV'\fR"
\fBContour Level Values.\fR  Each of the first \&'NCL'
elements of the internal parameter array \&'CLV' is a contour
level for which something is to be done, such as the drawing of contour lines, the
drawing of contour labels, and/or the addition of contour lines to an area
map.
.sp
Only elements 1 through \&'NCL' may be accessed via the internal parameter-setting
routines.  Thus, code to set the contour levels and associated quantities
must begin with a call to set \&'NCL'.
.sp
A side effect of setting the element numbered \&'PAI' of \&'CLV' is that the
associated element number \&'PAI' in each of the internal parameter arrays \&'AIA', \&'AIB',
\&'CLC', \&'CLD', \&'CLL', \&'CLU', \&'LLC', and \&'LLT' is also given a default value.
These values are as follows:
.sp
.in +.5i
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
.nr 80 0
.nr 38 \w\fBParameter
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'AIA'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'AIB'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLD'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLL'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLU'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLC'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LLT'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wDefault Value\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w'PAI'+1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w'PAI'
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w-1.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w'$$$$$$$$$$$$$$$$'
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w0.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w-1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w' \&' (a single blank)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 5032 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBParameter\h'|\n(41u'Default Value\fR
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'AIA'\h'|\n(41u''PAI'+1
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'AIB'\h'|\n(41u''PAI'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLC'\h'|\n(41u'-1.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLD'\h'|\n(41u''$$$$$$$$$$$$$$$$'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLL'\h'|\n(41u'0.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLU'\h'|\n(41u'~1
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLC'\h'|\n(41u'-1
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LLT'\h'|\n(41u'' \&' (a single blank)
.sp.5
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.in -.5i
.sp
Thus, in code to set contour levels and associated quantities, each contour
level must be set before the quantities associated with it.
.sp
A side effect of setting element number 1 of \&'CLV' is that the internal parameter
\&'CIU', which indicates what contour interval was used, is set to zero.  It is
assumed
that this will only happen when you are providing all the
contour levels, in which case the concept of the \fIcontour interval\fR may or
may not be well defined.  See the description of \&'CIU' for more information.
.tA "Real Array" "See description above."
.LI "\fB'CMN'\fR"
\fBContour Minimum.\fR
When \&'CLS' is greater than zero and \&'CIS' is also greater than zero, if \&'CMN'
is less than or equal to \&'CMX', then the contour levels used will be 
\&'CMN', \&'CMN'+'CIS', \&'CMN'+2*'CIS', ... , \&'CMN'+\fIn\fR*'CIS', where
\fIn\fR is the
largest integer such that \&'CMN'+\fIn\fR*'CIS' is less than or equal 
to \&'CMX'.  The
labeled levels will be those for which \fIn\fR is a multiple of \&'LIS'.
.tA "Real" "1."
.LI "\fB'CMX'\fR"
\fBContour Maximum.\fR
See the description of \&'CMN', above.
.tA "Real" "0."
.LI "\fB'CTM'\fR"
\fBCharacter Temporary.\fR
The internal parameter name \&'CTM' refers to a temporary character buffer in CONPACK;
it
may be used in the subroutine CPCHCL to get the dash pattern for the
current line and in the subroutines CPCHHL, CPCHIL, and CPCHLL to
get the text
of the label being written or to change it.
.tA "Character" "None"
.LI "\fB'CWM'\fR"
\fBCharacter-width Multiplier.\fR  All character size internal parameters
are multiplied by \&'CWM'; this makes it easy to
scale the sizes up or down simultaneously.  Internal parameters affected
by this are as follows:
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
.nr 38 \w\&'CFS'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CFW'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w'DPS'
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w'DPV'
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \w'HLS'
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w'HLW'
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \w'ILS'
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w'ILW'
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \w'LLS'
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w'LLW'
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
.if t .if \n(TW>\n(.li .tm Table at line 5079 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\&'CFS'\h'|\n(41u''DPS'\h'|\n(42u''HLS'\h'|\n(43u''ILS'\h'|\n(44u''LLS'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CFW'\h'|\n(41u''DPV'\h'|\n(42u''HLW'\h'|\n(43u''ILW'\h'|\n(44u''LLW'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-5
.tA "Real" "1."
.LI "\fB'DPS'\fR"
\fBDash Pattern Size.\fR  \&'DPS' specifies the nominal size 
(width) of a character in a
dash pattern, as a fraction of the width of the viewport.  This nominal size
is multiplied by \&'CWM'.
.tA "Real" ".010"
.LI "\fB'DPU'\fR"
\fBDash Pattern Use Flag.\fR
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
No dash patterns are used.  Contour lines are 
drawn using calls to CURVE.
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
Dash patterns are used.  Contour lines are drawn using 
calls to CURVED.  When the label positioning flag
ABS('LLP') = 1, contour lines are caused to be labeled by using a dash
pattern formed by concatenating \fIn\fR repetitions of 
the appropriate element of
\&'CLD' (the nominal dash pattern for the line) and 
the appropriate element of
\&'LLT' (the numeric label for the line); in 
this case, \&'DPU' specifies the
value of \fIn\fR.
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
.nr 38 \wLE 0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fIn\fR>0
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
.if t .if \n(TW>\n(.li .tm Table at line 5112 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'LE 0\h'|\n(41u'
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
\&\h'|\n(40u'\fIn\fR>0\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.tA "Integer" "3"
.LI "\fB'DPV'\fR"
\fBDash Pattern Vector Length.\fR
\&'DPV' specifies the nominal length of the solid vector generated
by a dollar sign, or the gap vector generated by an apostrophe, in a dash
pattern, as a fraction of the width of the viewport.  This nominal length is
multiplied by \&'CWM'.
.tA "Real" ".005"
.ne 7
.LI "\fB'GIC'\fR"
\fBGroup Identifier for Contour Lines.\fR
\&'GIC' specifies the group identifier for contour lines added to
an area map by the subroutine CPCLAM.
.tA "Integer" "3"
.LI "\fB'GIL'\fR"
\fBGroup Identifier for Label Boxes.\fR
\&'GIL' specifies the group identifier for label boxes added to an
area map by the subroutine CPLBAM.
.tA "Integer" "3"
.LI "\fB'GIS'"
Group Identifier for Strips.\fR
\&'GIS' specifies the group identifier for a group of edges added
to an area map by the subroutine CPCLAM in order
to create a set of vertical strips.
This is done only if the internal parameter \&'NVS'
is non-zero.
See \&'NVS', below.
.tA "Integer" "4"
.LI "\fB'HIC'\fR"
\fBHigh Label Color Index.\fR
\&'HIC' is used in determining the color index for high labels.
See the description of \&'HLC', below.
.tA "Integer" "-1"
.LI "\fB'HIT'\fR"
\fBHigh Label Text String.\fR
\&'HIT' specifies the text string to be used in labeling a high.
See the description of \&'HLT', below.
.tA "Character" "'H:B:$ZDV$:E' 
.LI "\fB'HLA'\fR"
\fBHigh/Low Label Angle.\fR
\&'HLA' specifies the angle (in degrees counterclockwise from a
vector pointing to the right) at which high and low labels are to be
written.
.tA "Real" "0."
.LI "\fB'HLB'\fR"
\fBHigh/Low Label Box Flag.\fR
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
Perimeter of the box is drawn (in the same color as
the label) after the label is drawn.
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
Box is filled (in the color specified by \&'LBC') before the label is drawn.
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
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHigh and low labels are not boxed at all.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wBoth 1 and 2 above.
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
.if t .if \n(TW>\n(.li .tm Table at line 5177 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'High and low labels are not boxed at all.
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3\h'|\n(41u'Both 1 and 2 above.
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-17
.tA "Integer" "0"
.LI "\fB'HLC'\fR"
\fBHigh/Low Label Color Index.\fR
\&'HLC' is used in determining the color index for high and low
labels.
.sp
The color index for high labels is determined in this manner:  
.BL
.LI
If \&'HIC'
is greater than or equal to zero,
\&'HIC' is used as the color index.
.LI
If \&'HIC'
is less than zero, but \&'HLC' is greater than or equal to zero, \&'HLC' is used as
the color index.  
.LI
If both \&'HIC' and \&'HLC' are less than zero, the current
text color index is used.  
.LE
.sp
The color index for low labels is determined similarly:  
.BL
.LI
If \&'LOC' is greater than or equal to zero,
\&'LOC' is used as the color index.
.LI
If \&'LOC' is less than zero,
but \&'HLC' is greater than or equal to zero, \&'HLC' is used as the
color index.  
.LI
If both \&'LOC' and \&'HLC' are less than zero, the current text
color index is used.  
.LE
.sp
If a box is drawn around a label, it is made the same color as
the label itself.
.sp
To set the color index of all high and low labels, simply supply the
desired value for \&'HLC'.  To have high and low labels that are colored
differently, set \&'HIC' and \&'LOC'.
.sp
The default values of \&'HLC', \&'HIC', and \&'LOC' are all -1's.
.tA "Integer" "-1"
.ne 7
.LI "\fB'HLL'\fR"
\fBHigh/Low Label Line Width.\fR
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
Line width is not set before drawing
boxes around high and low labels.
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
The desired width, as a multiple of the normal line width.
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
.nr 38 \wLE 0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0.
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
.if t .if \n(TW>\n(.li .tm Table at line 5239 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'LE 0.\h'|\n(41u'
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
\&\h'|\n(40u'>0.\h'|\n(41u'
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
.LI "\fB'HLO'\fR"
\fBHigh/Low Label Overlap Flag.\fR
The value of \&'HLO' determines what is to be done about the problem of high and
low labels overlapping other objects.  Add one or more of
the following values and use the resulting sum to obtain the
combination of effects you want:
.sp
.ne 19
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
The problem
will be ignored.
High and low labels will not be checked for overlap with
anything else.
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
Omit high and low labels that overlap the informational label.
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
Omit high and low labels that overlap other high and low labels
found before them.
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
Omit high and low labels that
overlap the edges of the viewport.
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
High and low
labels that overlap the edges of the viewport should be moved inward by just
enough to take care of the problem.
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w4
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w8
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wEffect\fR
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
.if t .if \n(TW>\n(.li .tm Table at line 5280 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Effect\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
\&\h'|\n(40u'4\h'|\n(41u'
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
\&\h'|\n(40u'8\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-31
.sp
The action implied by the value 4 takes precedence over that
implied by the value 8;  if you add both 4 and 8, the effect
will be as if you had added 4 alone.
.TE
.tA "Integer" "3 (the sum of 1 and 2)"
.ne 5
.LI "\fB'HLS'\fR"
\fBHigh/Low Label Size.\fR
\&'HLS' specifies the nominal size (width) of a character in a
high or low label, as a fraction of the width of the viewport.  This nominal
size is multiplied by \&'CWM'.
.tA "Real" ".012"
.LI "\fB'HLT'\fR"
\fBHigh/Low Label Text Strings.\fR
You can specify
the character strings used to label highs and lows 
individually, by setting \&'HIT' and \&'LOT', or together, by setting \&'HLT'.
If \&'HLT' is set, and there are no apostrophes in the
given character string, then
both \&'HIT' and \&'LOT' will be set equal to \&'HLT' and it will therefore be used as
the label for both highs and lows.  If there are apostrophes in the string,
what precedes the first apostrophe
will be used as the value of \&'HIT' (the label for
a high) and what follows it will be used as the value of \&'LOT' (the label for
a low).
.sp
Remember that, in Fortran, an apostrophe in a string that is delimited by
apostrophes is represented by two apostrophes.
.sp
The substring $ZDV$ may be used to represent the numeric value of the high or
the low, divided by the current scale factor.  The substring $ZDVU$ may be
used to represent the unscaled value.
.sp
.ne 8
\fBExamples\fR
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
.rm 80 81 82
.nr 80 0
.nr 38 \w\fBFortran String
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'H''L'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HI''LO'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'$ZDV$'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'H($ZDV$)''L($ZDV$)'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'H:B:$ZDV$:E:''L:B:$ZDV$:E:'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 2.25in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wHigh Label
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wH
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHI
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w1.362
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wH(1.362)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wH sub 1.362
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1in
.if \n(81<\n(38 .nr 81 \n(38
.nr 82 0
.nr 38 \wLow Label~~~\fR
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wL
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wLO
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w.764
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wL(.764)
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wL sub .764
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 38 1in
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
.if t .if \n(TW>\n(.li .tm Table at line 5330 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBFortran String\h'|\n(41u'High Label\h'|\n(42u'Low Label~~~\fR
.sF
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'H''L'\h'|\n(41u'H\h'|\n(42u'L
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HI''LO'\h'|\n(41u'HI\h'|\n(42u'LO
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'$ZDV$'\h'|\n(41u'1.362\h'|\n(42u'.764
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'H($ZDV$)''L($ZDV$)'\h'|\n(41u'H(1.362)\h'|\n(42u'L(.764)
./"\&'H:B:$ZDV$:E:''L:B:$ZDV$:E:';H\s-3\d1.362\s+3\u;L\s-3\d.764\s+3\u
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'H:B:$ZDV$:E:''L:B:$ZDV$:E:'\h'|\n(41u'H sub 1.362\h'|\n(42u'L sub .764
.eF
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-12
.sp
Note that, in the final example above, the subscripting capability
of the utility
PLOTCHAR is used.  The terminating function code E ensures that the H or
the L will be centered on the high or low.
To center the whole thing,
either remove the E's or change them to N's.
.sp
Neither of the character strings \&'HIT' and \&'LOT' may contain more than 20
characters.
If \&'HIT' is blank, highs will not be labeled.  If \&'LOT' is blank, lows will
not be labeled.
.sp
There is no default value for \&'HLT'.
The default value for \&'HIT' is \&'H:B:$ZDV$:E:' and the default value of \&'LOT'
is \&'H:B:$ZDV$:E:', as shown in the final example above.
.tA "Character" "None"
.ne 6
.LI "\fB'HLW'\fR"
\fBHigh/Low Label White Space Width.\fR
\&'HLW' specifies the nominal width of white space to be left
around a high or low label, as a fraction of the width of the viewport.  This
nominal width is multiplied by \&'CWM'.
.tA "Real" ".005"
.br
.ne 8
.LI "\fB'HLX'\fR"
\fBHigh/Low Search Radius in X.\fR
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
Specifies the half-width of the index-value
neighborhood used in searching the contour field for highs and lows.  The 
\fIneighborhood\fR is the area around the index value.  See
the Example below.
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
CONPACK picks a reasonable value to use
(approximately 1/8 of \&'ZDM', but not less than 2 nor greater than 15).
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
.if t .if \n(TW>\n(.li .tm Table at line 5376 file man/conpack.l is too wide - \n(TW units
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-16
.sp
\fBExample\fR
.sp
If \&'HLX' = 3 and \&'HLY' = 4, then the values in ZDAT examined
to determine if (I,J) is a high or a low are those having indices (K,L),
where
.BL
.LI
Either K not equal to I, or L not equal to J,
.LI
K is between
MAX(1,I-3) and MIN('ZDM',I+3), inclusive, and
.LI
L is between MAX(1,J-4) and
MIN('ZDN',J+4), inclusive.
.LE
.tA "Integer" "0"
.LI "\fB'HLY'\fR"
\fBHigh/Low Search Radius in Y.\fR
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
Specifies the half-height of the index-value
neighborhood used in searching the contour field for highs and lows.  The 
\fIneighborhood\fR is the area around the index value.
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
CONPACK picks a reasonable value to use
(approximately 1/8 of \&'ZDN', but not less than 2 nor greater than 15).
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
.if t .if \n(TW>\n(.li .tm Table at line 5412 file man/conpack.l is too wide - \n(TW units
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-15
.sp
.ne 3
For an example, see the description of \&'HLX', above.
.tA "Integer" "0"
.LI "\fB'ILA'\fR"
\fBInformational Label Angle.\fR
\&'ILA' specifies the angle (in degrees counterclockwise from a
vector pointing to the right) at which the informational label is 
written.
.tA "Real" "0."
.LI "\fB'ILB'\fR"
\fBInformational Label Box Flag.\fR
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
Perimeter of the box is drawn (in the same
color as the label) after the label is drawn.
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
Box is filled (in the color specified by \&'LBC') before the label is
drawn.
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
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wInformational label is not boxed at all.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wBoth 1 and 2 above.
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
.if t .if \n(TW>\n(.li .tm Table at line 5444 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'Informational label is not boxed at all.
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3\h'|\n(41u'Both 1 and 2 above.
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-18
.tA "Integer" "0"
.LI "\fB'ILC'\fR"
\fBInformational Label Color Index.\fR
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
Informational label and the box around it (if any),
are drawn in the color specified by the current text color
index.
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
Specifies the
desired color index for the label and the box.  If a box is drawn around the
label, it is made the same color as the label itself.
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
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGE 0
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
.if t .if \n(TW>\n(.li .tm Table at line 5465 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'<0\h'|\n(41u'
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
\&\h'|\n(40u'GE 0\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-16
.tA "Integer" "-1"
.ne 7
.LI "\fB'ILL'\fR"
\fBInformational Label Line Width.\fR
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
Line width is not set before drawing a
box around the informational label.
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
Specifies the desired width, as a multiple of the normal line width.
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
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLE 0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0.
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
.if t .if \n(TW>\n(.li .tm Table at line 5484 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LE 0.\h'|\n(41u'
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
\&\h'|\n(40u'>0.\h'|\n(41u'
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
.LI "\fB'ILP'\fR"
\fBInformational Label Positioning Flag.\fR
\&'ILP' determines how the informational label is to be positioned.
There are nine possible values, each of which specifies a point of the label
box that is to lie on the point defined by \&'ILX' and \&'ILY', as shown
below:
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
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-4
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w+1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w+2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w+3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w+4
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLower left-hand corner of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCenter of bottom of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLower right-hand corner of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCenter of left edge of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCenter of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCenter of right edge of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wUpper left-hand corner of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCenter of top edge of label box.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wUpper right-hand corner of label box.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 5515 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-4\h'|\n(41u'Lower left-hand corner of label box.
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-3\h'|\n(41u'Center of bottom of label box.
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-2\h'|\n(41u'Lower right-hand corner of label box.
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1\h'|\n(41u'Center of left edge of label box.
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'Center of label box.
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+1\h'|\n(41u'Center of right edge of label box.
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+2\h'|\n(41u'Upper left-hand corner of label box.
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+3\h'|\n(41u'Center of top edge of label box.
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'+4\h'|\n(41u'Upper right-hand corner of label box.
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.sp
Left, right, bottom, and top are defined from the viewpoint of someone
reading the label right side up.
.tA "Integer" "4"
.ne 5
.LI "\fB'ILS'\fR"
\fBInformational Label Size.\fR
\&'ILS' specifies the nominal size (width) of a character in the
informational label, as a fraction of the width of the viewport.  This nominal
size is multiplied by \&'CWM'.
.tA "Real" ".012"
.ne 6
.LI "\fB'ILT'\fR"
\fBInformational Label Text String.\fR
\&'ILT' is a string of 100 or fewer characters, specifying the
text of the informational label.  The following substrings will be replaced
by a numeric value:
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
.nr 80 0
.nr 38 \w$CIU$
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w$CMN$
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w$CMX$
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w$SFU$
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w$ZMN$
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w$ZMX$
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wContour interval used
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMinimum contour level
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMaximum contour level
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCurrent scale factor
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMinimum value in the data array
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMaximum value in the data array
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 5543 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'$CIU$\h'|\n(41u'Contour interval used
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'$CMN$\h'|\n(41u'Minimum contour level
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'$CMX$\h'|\n(41u'Maximum contour level
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'$SFU$\h'|\n(41u'Current scale factor
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'$ZMN$\h'|\n(41u'Minimum value in the data array
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'$ZMX$\h'|\n(41u'Maximum value in the data array
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-9
.sp
In each case except $SFU$, the given value will have been divided by the
current scale factor.  You can insert U just before the final $ (as in
\&'$CIUU$' or \&'$CMNU$') to request the use of an unscaled value.
.sp
The value that replaces $CIU$ is only correct if CONPACK
itself has chosen the contour levels; otherwise, it may be necessary for
you to set the value of \&'CIU', which is described above.
.sp
If \&'ILT' is given the value \&' \&' (a single blank), there is no 
informational label.
.tA "Character" " 'CONTOUR FROM $CMN$"
.ti +2.7i
TO $CMX$ BY $CIU$'
.LI "\fB'ILW'\fR"
\fBInformational Label White Space Width.\fR
\&'ILW' specifies the nominal width of white space to be left
around the informational label, as a fraction of the width of the viewport.
This nominal width is multiplied by \&'CWM'.
.tA "Real" ".005"
.ne 17
.LI "\fB'ILX'\fR"
\fBInformational Label X Coordinate.\fR
\&'ILX' specifies the X coordinate of the basepoint of the
informational label.  The given value is mapped linearly onto the viewport.
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
Outside the viewport on the left side.
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
Between the left and right edges of the viewport.
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
Right edge of the
viewport.  
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
Beyond the right edge of the viewport.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.<'ILX'<1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>1.
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLeft edge of the viewport.
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
.if t .if \n(TW>\n(.li .tm Table at line 5593 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'<0.\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.\h'|\n(41u'Left edge of the viewport.
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.<'ILX'<1.\h'|\n(41u'
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
\&\h'|\n(40u'1.\h'|\n(41u'
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
\&\h'|\n(40u'>1.\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-23
.tA "Real" ".98"
.ne 6
.LI "\fB'ILY'\fR"
\fBInformational Label Y Coordinate.\fR
\&'ILY' specifies the Y coordinate of the basepoint of the
informational label.  The given value is mapped linearly onto the viewport.
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
Outside the viewport on the bottom edge.
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
Between the bottom and top edges of the viewport.
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
Top edge of the
viewport.  
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
Beyond the top edge of the viewport.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.<'ILY'<1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>1.
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wBottom edge of the viewport.
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
.if t .if \n(TW>\n(.li .tm Table at line 5624 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'<0.\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.\h'|\n(41u'Bottom edge of the viewport.
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.<'ILY'<1.\h'|\n(41u'
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
\&\h'|\n(40u'1.\h'|\n(41u'
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
\&\h'|\n(40u'>1.\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-23
.sp
.tA "Real" "-.02"
.ne 10
.LI "\fB'IWM'\fR"
\fBInteger Workspace for Masking.\fR
\&'IWM' specifies the amount of integer workspace to be allotted
for use by CPCLDM, which draws contour lines masked by an area map, in calls
to the subroutine ARDRLN in the utility AREAS.  Assume an internal
parameter 
value \fIn\fR;
the space used will be 2\fIn\fR (\fIn\fR for the array IAI and 
\fIn\fR for the array IAG,
in calls to ARDRLN).  The value \fIn\fR must be greater than or equal to the
number of group identifiers used in generating the area map.
.tA "Integer" "10"
.ne 9
.LI "\fB'IWU'\fR"
\fBInteger Workspace Usage.\fR
\&'IWU' is intended for retrieval only.  It is 
set to zero by the call
to CPRECT or CPSPRS.  As CONPACK subroutines are called, the value
of \&'IWU' is updated to reflect the largest number of words of integer
workspace needed at any one time.  Therefore, by retrieving its value after
an entire plot has been constructed, you can find out how large an integer
workspace was actually required.
.tA "Integer" "None"
.ne 10
.LI "\fB'LBC'\fR"
\fBLabel Box Color Index.\fR
If label boxes (of whatever type) are filled, the filling is done using the
color index specified by \&'LBC'.  
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
The current fill area color index is used.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGE 0
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSpecifies the color index to be used.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
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
.if t .if \n(TW>\n(.li .tm Table at line 5667 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'<0\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'GE 0\h'|\n(41u'Specifies the color index to be used.
.fc
.nr T. 1
.T# 1
.35
.rm a+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-10
.tA "Integer" "0"
.LI "\fB'LBX'\fR"
\fBLabel Box X Coordinate.\fR
Not to be set by the user.  The value may be retrieved in one of the routines
CPCHCF, CPCHHL, CPCHIL, or CPCHLL.  
\&'LBX' specifies the X coordinate (in the
current world coordinate system) of the center of the box surrounding the
label that has caused the routine to be called.
.tA "Real" "0."
.ne 7
.LI "\fB'LBY'\fR"
\fBLabel Box Y Coordinate.\fR
Not to be set by the user.  The value may be retrieved in one of the routines
CPCHCF, CPCHHL, CPCHIL, or CPCHLL.  
\&'LBY' specifies the Y coordinate (in the
current world coordinate system) of the center of the box surrounding the
label that has caused the routine to be called.
.tA "Real" "0."
.LI "\fB'LIS'\fR"
\fBLabel Interval Specifier.\fR
When you give \&'CLS' a positive value, indicating that CONPACK is to choose
contour levels at intervals of the form \fIb*k\fR, where \fIb\fR is a 
base value and
\fIk\fR is an integer, and you give \&'CIS' a positive value, indicating that 
it is
the desired value of \fIb\fR, then you must set \&'LIS' to specify the interval
between labeled contour levels.
See the descriptions of the internal parameters \&'CLS' and \&'CIS',
above.
.sp
\fBExample\fR
.sp
If you specify \&'CLS'=1, \&'CIS'=1/3, and \&'LIS'=3 you will
get contours at values like 1/3, 2/3, 3/3, 4/3..., with labels at 
values like 1, 2, 3...
.tA "Integer" "5"
.LI "\fB'LIT'\fR"
\fBLabel Interval Table.\fR
See the description of the internal parameter \&'CIT', above.
.tA "Integer Array" "See \&'CIT'"
.LI "\fB'LIU'\fR"
\fBLabel Interval Used.\fR
\&'LIU' is for retrieval only.  When CONPACK chooses the contour
interval and decides that every \fInth\fR one should be labeled, it sets \&'LIU'
to \fIn\fR.
.tA "Integer" "None"
.LI "\fB'LLA'\fR"
\fBLine Label Angle.\fR
\&'LLA' specifies the angle (in degrees counterclockwise from a
vector pointing to the right) at which contour line labels are to be
written when ABS ('LLP') is greater than or equal to 2 and \&'LLO' is 0.
.tA "Real" "0."
.ne 4
.LI "\fB'LLB'\fR"
\fBLine Label Box Flag.\fR
You may assign \&'LLB' any of the following values:
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
Contour line labels
drawn by CPLBDR are not boxed at all.  
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
Perimeter of the box is drawn (in
the same color as the label) after the label is drawn.
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
Box is filled (in the color specified by \&'LBC') before the
label is drawn.  
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wBoth 1 and 2 above.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
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
.if t .if \n(TW>\n(.li .tm Table at line 5746 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3\h'|\n(41u'Both 1 and 2 above.
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-21
.tA "Integer" "0"
.LI "\fB'LLC'\fR"
\fBLine Label Color Index.\fR
An array of color indices for labels associated with the contour levels
specified by \&'CLV'.
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
The color index for labels on contour lines at the level specified by 
the associated element of \&'CLV'.  
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
The current text color index is to be used.
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
.if t .if \n(TW>\n(.li .tm Table at line 5766 file man/conpack.l is too wide - \n(TW units
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.sp
This internal parameter only affects line labels when ABS('LLP') = 2 or 3 and the
labels are therefore drawn by CPLBDR.  It does not affect line labels when
ABS('LLP') = 1 and the line labels are therefore drawn by the dash utility
(DASHCHAR), as called by CPCLDM or CPCLDR.
.sp
The default values of the elements of \&'LLU' are all -1's.
.tA "Integer Array" "See description above."
.ne 12
.LI "\fB'LLL'\fR"
\fBLine Label Line Width.\fR
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
Line width is not set before drawing
boxes around contour line labels.
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
The desired width, as a multiple of the normal line width.
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
.nr 38 \wLE 0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\>0.
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
.if t .if \n(TW>\n(.li .tm Table at line 5792 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'LE 0.\h'|\n(41u'
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
\&\h'|\n(40u'\>0.\h'|\n(41u'
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
.LI "\fB'LLO'\fR"
\fBLine Label Orientation.\fR
\&'LLO' only has effect when ABS('LLP') is greater than or equal
to 2, specifying the use of either the regular scheme or the penalty scheme for
positioning labels on contour lines.  
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
All labels are written at the angle specified by \&'LLA'.  
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
Each label is written in the \fIlocal direction\fR of the contour line.
The \fIlocal direction\fR is the direction of the tangent to the contour line at
the label position.
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
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNE  0
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
.if t .if \n(TW>\n(.li .tm Table at line 5814 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'NE  0\h'|\n(41u'
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
.tA "Integer" "0"
.LI "\fB'LLP'\fR"
\fBLine Label Positioning.\fR  If \&'LLP' is 0, it specifies that 
no line labels should be produced.
.sp
If ABS('LLP') is 1, it specifies that labels should be positioned along contour
lines by using the old CONREC scheme of setting up a character dash pattern
including the label and using the software dash utility to draw the labels
(which requires having \&'DPU' set non-zero).  This scheme has the disadvantages
that you cannot control the orientation or shield the labels from
having contour lines drawn through them.
.sp
If ABS('LLP') is 2, the labels are positioned at regular
intervals along the line.  See the descriptions of the internal
parameters \&'RC1',
\&'RC2', and \&'RC3'.
.sp
If ABS('LLP') is 3, the labels are positioned by using a penalty
scheme, which gives much better results than either of the other values.
See the descriptions of the internal parameters with names in the form of
\&'PC\fIn\fR' and \&'PW\fIn\fR'.
.sp
When \&'LLP' is 2 or 3, the 2-D smoothing, if any, implied by the value of \&'T2D'
is suspended during label positioning, so that fewer label positions will be
considered. 
This is quite a bit faster and the results are nearly as good as
if the smoothing were done.  To force smoothing, use \&'LLP' = -2 or -3.
.tA "Integer" "1"
.LI "\fB'LLS'\fR"
\fBLine Label Size.\fR  \&'LLS' specifies the nominal 
size (width) of a character in a
contour line label, as a fraction of the width of the viewport.  This nominal
size is multiplied by \&'CWM'.
.tA "Real" ".010"
.LI "\fB'LLT'\fR"
\fBLine Label Text String.\fR
For each I from 1 to \&'NCL', element I of the internal parameter array \&'LLT' is a
string of 20 or fewer characters, to be used as a label for the contour
level specified by the Ith element of \&'CLV'.  Since the character string will
be plotted using the subroutine PLCHHQ in the utility PLOTCHAR, it may contain
colon-enclosed function codes to perform functions such as creating
superscripts.
.sp
If you do not supply the elements of this array, the elements will be
filled in by CONPACK. 
.tA "Character Array" "None"
.LI "\fB'LLW'\fR"
\fBLine Label White Space.\fR
\&'LLW' specifies the nominal width of white space to be left
around a contour line label, as a fraction of the width of the viewport.
This nominal width is multiplied by \&'CWM'.
.tA "Real" ".005"
.LI "\fB'LOC'\fR"
\fBLow Label Color Index.\fR
\&'LOC' is used in determining the color index for low labels.
See the description of \&'HLC', above.
.tA "Integer" "-1"
.LI "\fB'LOT'\fR"
\fBLow Label Text String.\fR
\&'LOT' specifies the text string to be used in labeling a low.
See \&'HLT', above.
.tA "Character" "'L:B:$ZDV$:E:'"
.ne 12
.LI "\fB'MAP'\fR"
\fBMapping Flag.\fR
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
Specifies that the X and Y coordinates used to create the
contour map are not to be transformed by the user-replaceable subroutine
CPMPXY.  
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
Specifies that X and Y coordinates are 
to be transformed by the user-replaceable subroutine CPMPXY.
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
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNE  0
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
.if t .if \n(TW>\n(.li .tm Table at line 5896 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'NE  0\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-15
.sp
The default version of 
CPMPXY provides two useful mappings:
.sp
.AL
.LI
If the first subscript of the data array is a linear function of the
longitude and the second is a linear function of the latitude, then you
can transform all graphics output onto a map background created by calls
to subroutines in the utility EZMAP just by setting \&'MAP' = 1,
\&'XC1' = minimum longitude, \&'XCM' = maximum longitude, \&'YC1' = minimum
latitude, and \&'YCN' = maximum latitude.  Also, the internal parameter \&'SET' 
must be given the value 0 in order to prevent CONPACK from calling SET and
thereby overriding the call done by EZMAP.
.LI
If the first subscript of the data array is a linear function of rho
and the second is a linear function of theta, where rho and theta are
polar coordinates, then to map all graphics output properly, you can set
\&'MAP' = 2, \&'XC1' = minimum rho, \&'XCM' = maximum rho, \&'YC1' = minimum
theta, and \&'YCN' = maximum theta.  In this case, you must either use
\&'SET' = 0 and do an appropriate SET call or use \&'SET' = 1 and give the
internal parameters \&'WDB', \&'WDL', \&'WDR', and \&'WDT' values consistent with the
mapped values of X and Y, which will all be of the form "rho*cos(theta)"
and "rho*sin(theta)", respectively.
.LE
.sp
.EQ
.EN
Using any other non-zero value of \&'MAP' will result in the identity mapping
($X -> X,~Y -> Y,~a ->~a,~1 -> 1,$~and so on.)
.EQ
.EN
.sp
You can supply your own version of
the subroutine CPMPXY and build as many different
mappings into it as desired.  See the description of CPMPXY.
.tA "Integer" "See description above."
.ne 9
.LI "\fB'NCL'\fR"
\fBNumber of Contour Levels.\fR
If CONPACK is to pick contour levels (see the description of \&'CLS'), then 
the initial call to CPRECT or CPSPRS causes \&'NCL' to be set to zero.
Subsequently, during the first call to a CONPACK subroutine requiring contour
levels to have been chosen, \&'NCL' will be set as part of the process of
choosing them.  If you choose the contour levels, the first
internal parameter that you must set is \&'NCL'.
.tA "Integer" "None"
.LI "\fB'NEL'\fR"
\fBNumeric Exponent Length.\fR
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
Exponents in
numeric labels should be written in the shortest possible form; plus signs
are omitted and the exponent magnitude is written with no leading zeros.
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
Exponents should be
written with a sign (+ or -) and the exponent magnitude should be padded
with leading zeros to a length of \fIn\fR characters.
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
.nr 38 \wLE 0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fIn\fR>0
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
.if t .if \n(TW>\n(.li .tm Table at line 5963 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'LE 0\h'|\n(41u'
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
\&\h'|\n(40u'\fIn\fR>0\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-16
.tA "Integer" "0"
.LI "\fB'NET'\fR"
\fBNumeric Exponent Type.\fR  \&'NET' determines
what characters are to be used between the mantissa
of a numeric label and the exponent.  
.sp
.EQ
.EN
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
Specifies the use of an E, as
in Fortran E format.
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
Specifies the use of function codes, as
expected by the utility PLOTCHAR, to generate "\s-2x\s+2 $10 sup n$", where
\fIn\fR is a superscript exponent.
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
Implies the use of "\s-2x\s+2 10**".
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
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
.if t .if \n(TW>\n(.li .tm Table at line 5991 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-19
.EQ
.EN
.tA "Integer" "1"
.ne 13
.LI "\fB'NEU'\fR"
\fBNumeric Exponent Use Flag.\fR
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
Forces the use of the
exponential form in all numeric labels.
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
Specifies use of the form without an exponent as long as it requires no more
than \fIn\fR characters; otherwise the form requiring the fewest 
characters should be used.
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
.nr 38 \wLE 0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fIn\fR>0
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
.if t .if \n(TW>\n(.li .tm Table at line 6014 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'LE 0\h'|\n(41u'
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
\&\h'|\n(40u'\fIn\fR>0\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-15
.tA "Integer" "5"
.LI "\fB'NLS'\fR"
\fBNumeric Leftmost Significant Digit Flag.\fR
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
Leftmost non-zero digit of a number
represented by a numeric label is to be considered its first significant
digit.
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
Digit in the same digit position as
the leftmost non-zero digit of the largest number (in absolute value) in the
data field being contoured is to be considered the leftmost significant digit.
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
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNE  0
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
.if t .if \n(TW>\n(.li .tm Table at line 6035 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'NE  0\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-16
.sp
Using \&'NLS' NE  0 tends to make the numeric labels more nearly consistent with one another.
.sp
\fBExample\fR
.sp
Assume the use of three significant digits.
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
.rm 80 81 82 83 84 85 86 87
.nr 80 0
.nr 38 \wUsing \&'NLS'=0:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wUsing \&'NLS'=1:
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 31 0
.nr 32 0
.nr 38 \w.500
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w.5
.if \n(32<\n(38 .nr 32 \n(38
.81
.rm 81
.nr 61 \n(31
.nr 38 \n(61+\n(32
.if \n(38>\n(81 .nr 81 \n(38
.if \n(38<\n(81 .nr 61 +(\n(81-\n(38)/2
.nr 82 0
.nr 31 0
.nr 32 0
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.00
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.0
.if \n(32<\n(38 .nr 32 \n(38
.82
.rm 82
.nr 62 \n(31
.nr 38 \n(62+\n(32
.if \n(38>\n(82 .nr 82 \n(38
.if \n(38<\n(82 .nr 62 +(\n(82-\n(38)/2
.nr 83 0
.nr 31 0
.nr 32 0
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.50
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.5
.if \n(32<\n(38 .nr 32 \n(38
.83
.rm 83
.nr 63 \n(31
.nr 38 \n(63+\n(32
.if \n(38>\n(83 .nr 83 \n(38
.if \n(38<\n(83 .nr 63 +(\n(83-\n(38)/2
.nr 84 0
.nr 38 \w...
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w...
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 85 0
.nr 31 0
.nr 32 0
.nr 38 \w9
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.50
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w9
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.5
.if \n(32<\n(38 .nr 32 \n(38
.85
.rm 85
.nr 65 \n(31
.nr 38 \n(65+\n(32
.if \n(38>\n(85 .nr 85 \n(38
.if \n(38<\n(85 .nr 65 +(\n(85-\n(38)/2
.nr 86 0
.nr 31 0
.nr 32 0
.nr 38 \w10
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.5
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w10
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.5
.if \n(32<\n(38 .nr 32 \n(38
.86
.rm 86
.nr 66 \n(31
.nr 38 \n(66+\n(32
.if \n(38>\n(86 .nr 86 \n(38
.if \n(38<\n(86 .nr 66 +(\n(86-\n(38)/2
.nr 87 0
.nr 38 \w...
.if \n(87<\n(38 .nr 87 \n(38
.nr 38 \w...
.if \n(87<\n(38 .nr 87 \n(38
.87
.rm 87
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 61 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 62 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 63 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr 45 \n(84+(3*\n(38)
.nr 85 +\n(45
.nr 65 +\n(45
.nr 46 \n(85+(3*\n(38)
.nr 86 +\n(46
.nr 66 +\n(46
.nr 47 \n(86+(3*\n(38)
.nr 87 +\n(47
.nr TW \n(87
.if t .if \n(TW>\n(.li .tm Table at line 6048 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(62u \n(82u \n(63u \n(83u \n(84u \n(65u \n(85u \n(66u \n(86u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Using \&'NLS'=0:\h'|\n(61u'.500\h'|\n(42u'1.00\h'|\n(43u'1.50\h'|\n(44u'...\h'|\n(45u'9.50\h'|\n(46u'10.5\h'|\n(47u'...
.ta \n(80u \n(81u \n(62u \n(82u \n(63u \n(83u \n(84u \n(65u \n(85u \n(66u \n(86u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Using \&'NLS'=1:\h'|\n(61u'.5\h'|\n(42u'1.0\h'|\n(43u'1.5\h'|\n(44u'...\h'|\n(45u'9.5\h'|\n(46u'10.5\h'|\n(47u'...
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-5
.tA "Integer" "1"
.ne 10
.LI "\fB'NLZ'\fR"
\fBNumeric Leading Zero Flag.\fR
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
A zero is placed before any
numeric label that would otherwise begin with a decimal point (0.345,
rather than .345).
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNE  0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNo zero is added to numeric labels.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
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
.if t .if \n(TW>\n(.li .tm Table at line 6066 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NE  0\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'No zero is added to numeric labels.
.fc
.nr T. 1
.T# 1
.35
.rm a+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-12
.tA "Integer" "0"
.LI "\fB'NOF'\fR"
.EQ
.EN
\fBNumeric Omission Flags.\fR
Set \&'NOF' to specify
what parts of a numeric label may be omitted.  
Add one or more of the following values and use the resulting sum to
obtain the combination of effects you want:
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
Specifies that no part may be omitted.  
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
Indicates that trailing zeros (as in 46.200) may be omitted.
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
Indicates that a trailing decimal point (as in 13.) may be omitted.
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
Indicates that an unnecessary leading "1" or "1." 
(as in 1 x $10 sup 13$) may be omitted.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w4
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wEffect\fR
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
.if t .if \n(TW>\n(.li .tm Table at line 6099 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Effect\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
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
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
\&\h'|\n(40u'4\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.EQ
.EN
.sp
Contour line labels generated by CONPACK and values in the informational
label that are known to have been rounded to nice values (like \&'$CIU$',
\&'$CMN$', and \&'$CMX$') will have trailing zeros trimmed in any case, no
matter what the value of \&'NOF' is.
.tA "Integer" "6 (the sum of 4 and 2)"
.ne 14
.LI "\fB'NSD'\fR"
\fBNumber of Significant Digits.\fR
\&'NSD' specifies the maximum number of significant digits
used in numeric labels representing contour field values.
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
Specifies that ABS(\fIn\fR) significant digits should be used.
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
Specifies that \fIm+n\fR digits should be used, where \fIm\fR is the number of digits
that are the same for all values in the contour field.
For example, if the
minimum value is 1123.6 and the maximum value is 1125.9, then the value
of \fIm\fR is 3.
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
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fIn\fR<0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fIn\fRGE  0
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
.if t .if \n(TW>\n(.li .tm Table at line 6130 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fIn\fR<0\h'|\n(41u'
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
\&\h'|\n(40u'\fIn\fRGE  0\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-16
.tA "Integer" "4"
.LI "\fB'NVS'\fR"
\fBNumber of Vertical Strips.\fR
When \&'NVS' is non-zero, an
extra group of edges, with group
identifier \&'GIS', is added to the area map by the subroutine CPCLAM.  These
edges include the boundary of the viewport and enough vertical lines to
break the area occupied by the viewport up into \&'NVS' vertical strips.  The
object of this is to break up the contour bands that are to be filled into
smaller and simpler pieces; this may be necessary if the graphics device in
use limits the number of points that may be used to define a polygon to be
filled.  The area identifier for the outside of the viewport is -1; all
other area identifiers used are 0's.
.tA "Integer" "1"
.LI "\fB'ORV'\fR"
\fBOut-of-range Value.\fR
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
Specifies an out-of-range value, to be used as the
value of X and Y coordinates returned by the mapping subroutine CPMPXY
to say
that a point is out-of-range (invisible) under the current mapping.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNE  0.
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNo out-of-range value specified.
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
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
.if t .if \n(TW>\n(.li .tm Table at line 6161 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0.\h'|\n(41u'No out-of-range value specified.
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NE  0.\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.tA "Real" "0."
.ne 5
.LI "\fB'PAI'\fR"
\fBParameter Array Index.\fR
The value of \&'PAI' must be set before accessing any internal parameter that is an
array, to indicate which element of the array is meant.
.tA "Integer" "0"
.LI "\fB'PC1'\fR"
\fBPenalty Scheme Constant 1.\fR
\&'PC1' is one of the constants used in the penalty scheme for
positioning labels.  The largest gradient allowed at the position of a label
will be
.>>
GRAV + \&'PC1' * GRSD
.<<
where GRAV is the average gradient and GRSD is
the standard deviation of the gradients.
.tA "Real" "1."
.LI "\fB'PC2'\fR"
\fBPenalty Scheme Constant 2.\fR
\&'PC2' is one of the constants used in the penalty scheme for
positioning labels.  It specifies the maximum (estimated) number of contour
bands allowed to cross a label.
.tA "Real" "5."
.LI "\fB'PC3'\fR"
\fBPenalty Scheme Constant 3.\fR
\&'PC3' is one of the constants used in the penalty scheme for
positioning labels.  It specifies, in degrees, the maximum cumulative change
in direction to be allowed along that portion of the contour line covered by
a circle centered on a label and having a radius equal to half the width of
the label.
.tA "Real" "60."
.LI "\fB'PC4'\fR"
\fBPenalty Scheme Constant 4.\fR
\&'PC4' is one of the constants used in the penalty scheme for
positioning labels.  It specifies the optimal distance in the term in the
penalty function that attempts to force labels to be at an optimal distance
from each other.
.tA "Real" ".05"
.LI "\fB'PC5'\fR"
\fBPenalty Scheme Constant 5.\fR
\&'PC5' is one of the constants used in the penalty scheme for
positioning labels,
in the term that attempts to force labels to be at an optimal distance
from each other.
.tA "Real" ".15"
.ne 6
.LI "\fB'PC6'\fR"
\fBPenalty Scheme Constant 6.\fR
\&'PC6' is one of the constants used in the penalty scheme for
positioning labels.  It specifies the minimum distance to be allowed between
any two labels on the same contour line, as a fraction of the width of the
viewport.
.tA "Real" ".30"
.LI "\fB'PIC'\fR"
\fBPoint Interpolation Flag for Contours.\fR
\&'PIC' specifies the number of points to interpolate between
each pair of points defining a segment of a contour line, prior to any
mapping implied by the internal parameter \&'MAP'.  
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
Should normally be used only if \&'MAP' is non-zero, which turns mapping on,
and \&'T2D' is zero, which turns the 2-D smoother off.  The intent is to map
straight-line segments of contour lines more nearly correctly into curved-line
segments on a background (one drawn by EZMAP, for example).  If the 2-D
smoother is turned on, the additional points will be used and the smoothed
curve will be constrained to pass through them.
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
Causes ABS(\&'PIC') points to be interpolated, but
the interpolated points are not, in general, used to draw the line segment;
the object,
in this case, is simply to do a finer search for changes in
visibility (out-of-range state, as defined by values of \&'ORV' returned by the
subroutine CPMPXY) along the segment.
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
.nr 38 \w=~0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNE ~0
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
.nr 38 \wNo points are interpolated.
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
.if t .if \n(TW>\n(.li .tm Table at line 6246 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'=~0\h'|\n(41u'No points are interpolated.
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NE ~0\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-24
.tA "Integer" "0"
.LI "\fB'PIE'\fR"
\fBPoint Interpolation Flag for Edges.\fR
\&'PIE' specifies the number of points to interpolate between
each pair of points defining a segment of an edge (the edge of the grid,
the edge of a special-value area, or the edge of an out-of-range area).
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
Should normally be used only if \&'MAP'
is non-zero, which turns mapping on.
The intent is to map straight-line
segments of edge lines more nearly correctly into curved-line segments on a
background (one drawn by EZMAP, for example).
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
Causes ABS(\&'PIE') points to be interpolated, but
the interpolated points are not, in general, used to draw the line segment.
The object, in this case, is simply to do a finer search for changes in
out-of-range state (visibility) along the segment.  (The edges of out-of-range
areas, however, are drawn using all such interpolated points.)
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
.nr 38 \w=~0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNE  0
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
.nr 38 \wNo points are interpolated.
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
.if t .if \n(TW>\n(.li .tm Table at line 6276 file man/conpack.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'=~0\h'|\n(41u'No points are interpolated.
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NE  0\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.sp
Using too large an absolute value of \&'PIE' will cause the tracing of the
edges of out-of-range areas to be very time-consuming, because the number of
points to be examined is \&'ZDM'*'ZDN'*'PIE'*'PIE'.
.tA "Integer" "0"
.LI "\fB'PW1'\fR"
\fBPenalty Scheme Weight 1.\fR
\&'PW1' specifies the weight for the gradient term in the penalty
function.
.tA "Real" "2."
.LI "\fB'PW2'\fR"
\fBPenalty Scheme Weight 2.\fR
\&'PW2' specifies the weight for the number-of-contours term in
the penalty function.
.tA "Real" "0."
.LI "\fB'PW3'\fR"
\fBPenalty Scheme Weight 3.\fR
\&'PW3' specifies the weight for the change-in-direction term in
the penalty function.
.tA "Real" "1."
.LI "\fB'PW4'\fR"
\fBPenalty Scheme Weight 4.\fR
\&'PW4' specifies the weight for the optimum-distance term in the
penalty function.
.tA "Real" "1."
.LI "\fB'RC1'\fR"
\fBRegular Scheme Constant 1.\fR
.EQ
.EN
\&'RC1' specifies the desired distance from the beginning of a
contour line to the first label on that line when they are positioned using
the regular scheme.  The \fIn\fRth label on each labeled contour line will be
at a distance 
\&'RC1' + \&'RC2' * (\fIn\fR-1) + \&'RC3' * "R sub n" units (in the fractional
coordinate system) from the beginning of the line, where "R sub n" is a random
number between -1 and 1.
.tA "Real" ".25"
.LI "\fB'RC2'\fR"
\fBRegular Scheme Constant 2.\fR
\&'RC2' specifies the desired nominal distance between labels when
they are positioned using the regular scheme.  See the description of \&'RC1',
above.
.tA "Real" ".25"
.LI "\fB'RC3'\fR"
\fBRegular Scheme Constant 3.\fR
\&'RC3' specifies the desired maximum variation in the distance
between labels when they are positioned using the regular scheme.  See the
description of \&'RC1', above.
.tA "Real" ".05"
.EQ
.EN
.LI "\fB'RWC'\fR"
\fBReal Workspace for Contours.\fR
\&'RWC' specifies the amount of real workspace to be allotted to
hold coordinates of points defining contour lines and associated quantities.
Assume an internal parameter value
\fIn\fR.  If no 2-D smoothing is requested, the total space used will be
2\fIn\fR (\fIn\fR
for X coordinates and another \fIn\fR for Y coordinates).  If 2-D smoothing is
requested, the total space used will be 7\fIn\fR (\fIn\fR for X coordinates,
\fIn\fR for
Y coordinates, and 5\fIn\fR for scratch arrays).
.sp
Normally, the value of \&'RWC' is of no particular interest to you, since
the same contour lines are produced with a small value as would be produced
with a large value.  There is only one situation in which it becomes of more
interest:  When the penalty scheme is used to position labels, the length of
the portion of the contour line over which the penalty function is evaluated
is limited by the value of \&'RWC'.  If \&'RWC' is set too small, too 
many labels
may be put on a given contour line and some of them may be too close to each
other.  If this happens, the solution is to increase the value of \&'RWC'.
.tA "Integer" "100"
.ne 7
.LI "\fB'RWG'\fR"
\fBReal Workspace for Gradients.\fR
\&'RWG' specifies the amount of real workspace to be allotted to
hold gradients that are to be computed and used in positioning labels using
the penalty scheme.  Using a larger value provides for a more accurate
representation of the gradient field, up to the point at which it exceeds
\&'ZDM'*'ZDN'.
.tA "Integer" "1000"
.LI "\fB'RWM'\fR"
\fBReal Workspace for Masking.\fR
\&'RWM' specifies the amount of real workspace to be allotted for
use by CPCLDM, which draws contour lines masked by an area map, in calls to
the subroutine ARDRLN, (in the utility AREAS).  Assume 
an internal parameter value \fIn\fR: the
space used will be 2\fIn\fR (\fIn\fR for the X-coordinate array \&'XCS' and
\fIn\fR for the
Y-coordinate array \&'YCS', in calls to ARDRLN).  Any value 
of \fIn\fR greater than
or equal to 2 will work; smaller values will cause the generation of more
calls to the user subroutine RTPL (one of the arguments of CPCLDM).
.tA "Integer" "100"
.LI "\fB'RWU'\fR"
\fBReal Workspace Usage.\fR
\&'RWU' is intended for retrieval only.  It is set to zero by the call
to CPRECT or CPSPRS.  Thereafter, as CONPACK subroutines are called, the value
of \&'RWU' is updated to reflect the largest number of words of real workspace
needed at any one time.  Therefore, by retrieving its value after an entire
plot has been constructed, you can find out how large a real workspace was
actually required.
.tA "Integer" "None"
.LI "\fB'SET'\fR"
\fBDo-SET-Call Flag.\fR 
.sp
.ne 7
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
No SET call is to be done by CONPACK.
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
SET call is to be done by CPRECT or CPSPRS.
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
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
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
.if t .if \n(TW>\n(.li .tm Table at line 6397 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'1\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-12
.sp
Arguments 5-8 of a SET call done by you must be consistent
with the
ranges of the X and Y coordinates being used by CONPACK, as specified by the
values of the internal parameters 
\&'MAP', \&'XC1', \&'XCM', \&'YC1', and \&'YCN'.  See the descriptions
of those internal parameters.
.tA "Integer" "1"
.LI "\fB'SFS'\fR"
\fBScale Factor Selector.\fR
The scale factor is that value (usually, but not necessarily, a power of 10)
by which the actual values of contour field values are to be divided to get
the value of a numeric label.  If \&'SFS' is given a value greater than zero,
that value is the scale factor to be used.  If \&'SFS' is given a value less
than or equal to zero, it is truncated to form an integer directing CONPACK
to select a scale factor.  Set the value of \&'SFS' as follows:
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
The value selected is the scale factor used.
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
Scale factor should be selected in such a way as to
reduce the ZDAT element having the largest absolute value to the range
from .1 to .999....
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
Scale factor should be selected in such a way as to
reduce the ZDAT element having the largest absolute value to the range
from 1. to 9.999....
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
Scale factor should be selected in such a way as to
place the decimal point in the ZDAT element having the largest absolute
value after the rightmost significant digit of that value (as defined by
the values of \&'NSD' and \&'NLS').
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
Scale factor should be selected in such a way as to
remove extra zeros from the ends of the ZDAT element having the largest
absolute value.  For example, if that element were .000136, the scale
factor would be 10 to the power -3; if that element were 136000
(assuming three significant digits are desired), the scale factor would
be 10 to the power 3.  If there are no extra zeros on either end of the
ZDAT element having the largest absolute value, the scale factor will be
1.
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
Scale factor should be selected in such a
way as to reduce all contour labels to integers.
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
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-2.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-3.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLE  -4.
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
.if t .if \n(TW>\n(.li .tm Table at line 6458 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'>0.\h'|\n(41u'
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
\&\h'|\n(40u'0.\h'|\n(41u'
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
\&\h'|\n(40u'-1.\h'|\n(41u'
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
\&\h'|\n(40u'-2.\h'|\n(41u'
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
\&\h'|\n(40u'-3.\h'|\n(41u'
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
\&\h'|\n(40u'LE  -4.\h'|\n(41u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-43
.tA "Real" "1."
.ne 4
.LI "\fB'SFU'\fR"
\fBScale Factor Used.\fR
\&'SFU' is intended for retrieval only; it gives the value of the
scale factor selected for use by CONPACK.
.tA "Real" "None"
.LI "\fB'SPV'\fR"
\fBSpecial Value.\fR
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
No special value is used.
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
Specifies a special value, which may be used in
data fields to signal missing data.  No contour lines will be drawn within
any grid cell with a special value at one or more of its four corners.
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
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNE  0.
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
.if t .if \n(TW>\n(.li .tm Table at line 6483 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'0.\h'|\n(41u'
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
\&\h'|\n(40u'NE  0.\h'|\n(41u'
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
.tA "Real" "0."
.LI "\fB'SSL'\fR"
\fBSmoothed Segment Length.\fR
\&'SSL' specifies the distance between points used to draw the
curves generated by 2-D smoothing; it is expressed as a fraction of the width
of the window in the coordinate system in which the smoothing is being done.
.tA "Real" ".01"
.LI "\fB'T2D'\fR"
\fBTension on Two-dimensional Splines.\fR
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
No 2-D smoothing is done.
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
2-D smoothing (using cubic splines under
tension) is done.
The absolute value of \&'T2D' is the desired tension.
Smoothing is done before the mapping, if any,
requested by the flag \&'MAP'.
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
2-D smoothing (using cubic splines under tension) is done.  The absolute
value of \&'T2D' is the desired tension.  Smoothing is done after
mapping.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w>0.
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
.if t .if \n(TW>\n(.li .tm Table at line 6516 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'0.\h'|\n(41u'
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
\&\h'|\n(40u'<0.\h'|\n(41u'
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
\&\h'|\n(40u'>0.\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.tA "Real" "0."
.ne 5
.LI "\fB'T3D'\fR"
\fBTension on Three-dimensional Splines.\fR
\&'T3D' specifies the tension on the 3-D (bicubic)
splines used by
CPSPRS to smooth the data being contoured.
.tA "Real" "1."
.LI "\fB'VPB'\fR"
\fBViewport Bottom.\fR
\&'VPB' is only used when \&'SET' is non-zero, specifying that CONPACK
should do the call to SET.  It specifies the position of the bottom edge of
the area in which the viewport is to be placed, expressed as a fraction
between 0. (the bottom edge of the plotter frame) and 1. (the top edge of the
plotter frame).   See the description of \&'VPS'.
.tA "Real" ".05"
.LI "\fB'VPL'\fR"
\fBViewport Left.\fR
\&'VPL' is only used when \&'SET' is non-zero, specifying that CONPACK
should do the call to SET.  It specifies the position of the left edge of
the area in which the viewport is to be placed, expressed as a fraction
between 0. (the left edge of the plotter frame) and 1. (the right edge of the
plotter frame).   See the description of \&'VPS'.
.tA "Real" ".05"
.LI "\fB'VPR'\fR"
\fBViewport Right.\fR
\&'VPR' is only used when \&'SET' is non-zero, specifying that CONPACK
should do the call to SET.  It specifies the position of the right edge of
the area in which the viewport is to be placed, expressed as a fraction
between 0. (the left edge of the plotter frame) and 1. (the right edge of the
plotter frame).   See the description of \&'VPS'.
.tA "Real" ".95"
.LI "\fB'VPS'\fR"
\fBViewport Shape.\fR
\&'VPS' is only used when \&'SET' is non-zero, specifying
that CONPACK
should do the call to SET.  It specifies the desired viewport shape, as
follows:
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
Specifies the exact shape of the viewport; the absolute
value is the ratio of the width of the viewport to its height.
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
Specifies a viewport completely filling the area specified
by \&'VPL', \&'VPR', \&'VPB', and \&'VPT'.
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
Specifies a plot of the shape determined
by the values of \&'XC1', \&'XCM', \&'YC1', and \&'YCN', reverting to the shape
specified by \&'VPL', \&'VPR', \&'VPB', and \&'VPT' if the ratio of the shorter
side to the longer side would be less than the value specified.
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
Specifies a plot of the shape determined by the values of \&'XC1', \&'XCM', \&'YC1', and \&'YCN', reverting to
a square if the ratio of the 
longer side to the shorter side would be greater than the value specified.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBRange
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w<0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0.<'VPS'<1.
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGE 1.
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
.if t .if \n(TW>\n(.li .tm Table at line 6583 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'<0.\h'|\n(41u'
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
\&\h'|\n(40u'0.\h'|\n(41u'
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
\&\h'|\n(40u'0.<'VPS'<1.\h'|\n(41u'
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
\&\h'|\n(40u'GE 1.\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-27
.sp
The viewport, whatever its final shape, is centered in, and made as large as
possible in, the area specified by the internal parameters \&'VPB', \&'VPL', \&'VPR', and
\&'VPT'.
.tA "Real" ".25"
.LI "\fB'VPT'\fR"
\fBViewport Top.\fR
\&'VPT' is only used when \&'SET' is non-zero, specifying that CONPACK
should do the call to SET.  It specifies the position of the top edge of the
area in which the viewport is to be placed, expressed as a fraction between
0. (the bottom edge of the plotter frame) and 1. (the top edge of the plotter
frame).   See the description of \&'VPS'.
.tA "Real" ".95"
.LI "\fB'WDB'\fR"
\fBWindow Bottom.\fR
When CONPACK does the call to \&'SET', the internal parameter \&'WDB' is used to determine
argument number 7, the user Y coordinate at the bottom of the window.  If
\&'WDB' is not equal to \&'WDT', \&'WDB' is used.  If \&'WDB' is equal to \&'WDT', but
\&'YC1' is not equal to \&'YCN', then \&'YC1' is used.  Otherwise, the 
value 1. is used. 

.tA "Real" "0."
.LI "\fB'WDL'\fR"
\fBWindow Left.\fR
When CONPACK does the call to \&'SET', the internal parameter \&'WDL' is used to determine
argument number 5, the user X coordinate at the left edge of the window.  If
\&'WDL' is not equal to \&'WDR', \&'WDL' is used.  If \&'WDL' is equal to \&'WDR', but
\&'XC1' is not equal to \&'XCM', then \&'XC1' is used.  Otherwise, the 
value 1. is used.

.tA "Real" "0."
.LI "\fB'WDR'\fR"
\fBWindow Right.\fR
When CONPACK does the call to \&'SET', the internal parameter \&'WDR' is used to determine
argument number 6, the user X coordinate at the right edge of the window.  If
\&'WDR' is not equal to \&'WDL', \&'WDR' is used.  If \&'WDR' is equal to \&'WDL', but
\&'XCM' is not equal to \&'XC1', then \&'XCM' is used.  Otherwise, the value
REAL('ZDM') is used.
.tA "Real" "0."
.LI "\fB'WDT'\fR"
\fBWindow Top.\fR
When CONPACK does the call to \&'SET', the internal parameter \&'WDB' is used to determine
argument number 8, the user Y coordinate at the top of the window.  If \&'WDT'
is not equal to \&'WDB', \&'WDT' is used.  If \&'WDT' is equal to \&'WDB', but \&'YCN'
is not equal to \&'YC1', then \&'YCN' is used.  Otherwise, the value REAL('ZDN')
is used.
.tA "Real" "0."
.LI "\fB'WSO'\fR"
\fBWorkspace Overflow Flag.\fR
\&'WSO' determines what to do when a real or integer workspace overflow
occurs.
Set \&'WSO' to one of the following values:
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
Execution terminates with a fatal-error
call to SETER.
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
An error message is written to the error
file, after which execution continues.
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
No error message is written, and 
execution continues.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
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
.if t .if \n(TW>\n(.li .tm Table at line 6656 file man/conpack.l is too wide - \n(TW units
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
\&\h'|\n(40u'\fBValue\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
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
\&\h'|\n(40u'1\h'|\n(41u'
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
\&\h'|\n(40u'2\h'|\n(41u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-19
.sp
When execution continues, the resulting plot will be incomplete.  The values
of \&'IWU' and \&'RWU' may be retrieved to find out how much workspace would have
been used if the call on which the workspace overflow occurred had succeeded.
It should be noted
that, if you provide these amounts on a subsequent run, you are not
assured that workspace overflow will be averted.
You may still run out of real or integer workspace.
See "Workspace Management" under the heading "CONPACK Organization and
Philosophy," earlier in this chapter.
.tA "Integer" "1"
.ne 6
.LI "\fB'XC1'\fR"
\fBX Coordinate at Index 1.\fR
\&'XC1' specifies the X coordinate value that corresponds to a
value of 1 for the first subscript of the data array, prior to any mapping
implied by a non-zero value of \&'MAP'.  If \&'XC1' is equal to \&'XCM', 1. will
be used.
.tA "Real" "0."
.LI "\fB'XCM'\fR"
\fBX Coordinate at Index M.\fR
\&'XCM' specifies the X coordinate value that corresponds to a
value of \&'ZDM' for the first subscript of the data array, prior to any
mapping implied by a non-zero value of \&'MAP'.  If \&'XC1' is equal to \&'XCM',
REAL('ZDM') will be used.
.tA "Real" "0."
.LI "\fB'YC1'\fR"
\fBY Coordinate at Index 1.\fR
\&'YC1' specifies the Y coordinate value that corresponds to a
value of 1 for the second subscript of the data array, prior to any mapping
implied by a non-zero value of \&'MAP'.  If \&'YC1' is equal to \&'YCM', 1. will
be used.
.tA "Real" "0."
.LI "\fB'YCN'\fR"
\fBY Coordinate at Index N.\fR
\&'YCN' specifies the Y coordinate value that corresponds to a
value of \&'ZDN' for the second subscript of the data array, prior to any
mapping implied by a non-zero value of \&'MAP'.  If \&'YC1' is equal to \&'YCN',
REAL('ZDN') will be used.
.tA "Real" "0."
.LI "\fB'ZD1'\fR"
\fBZDAT 1st Dimension.\fR
\&'ZD1' specifies the first dimension of the array ZDAT, which
contains the data to be contoured.  If CPRECT is called, it sets \&'ZD1' (the
argument KZDT is the desired value).  If CPSPRS is called, it either picks a
value of \&'ZD1' (if \&'ZDS' is non-zero) or expects the user to have done so
(if \&'ZDS' is zero).
.tA "Integer" "1"
.ne 8
.LI "\fB'ZDM'\fR"
\fBZ Data Array Dimension M.\fR
\&'ZDM' specifies the first dimension of the array of data to be
contoured.  Its value will be less than or equal to the value of \&'ZD1'.  If
CPRECT is called, it sets \&'ZDM' (the argument MZDT is the desired value).  If
CPSPRS is called, it either picks a value of \&'ZDM' (if \&'ZDS' is non-zero) or
expects the user to have done so (if \&'ZDS' is zero).
.tA "Integer" "1"
.LI "\fB'ZDN'\fR"
\fBZ Data Array Dimension N.\fR
\&'ZDN' specifies the second dimension of the array of data to be
contoured.  If CPRECT is called, it sets \&'ZDN' (the argument NZDT is the
desired value).  If CPSPRS is called, it either picks a value of \&'ZDN' (if
\&'ZDS' is non-zero) or expects the user to have done so (if \&'ZDS' is zero).
.tA "Integer" "1"
.LI "\fB'ZDS'\fR"
\fBZDAT Dimension Selector.\fR
If \&'ZDS' is non-zero, CPSPRS will select values for \&'ZD1', \&'ZDM', and \&'ZDN';
otherwise, they will be expected to have been set by the user.  Note that,
if the size of the dense array is not a product of the size of the sparse
array and some perfect square, the aspect ratio of the dense grid may be
slightly different from that of the sparse grid.
.tA "Integer" "1"
.LI "\fB'ZDU'\fR"
\fBZ Data Value, Unscaled.\fR  See \&'ZDV', the next entry.
.tA "Real" "None"
.LI "\fB'ZDV'\fR"
\fBZ Data Value.\fR
\&'ZDV' is mostly for output.  Its value may be retrieved in a
user version of CPCHHL to retrieve the value of the high or low that is
being labeled.  If a character string representing the value is desired,
CPGETC may be used to obtain it (as modified by the current scale factor);
thus, to obtain the character representation of an arbitrary value in a form
consistent with the other values on a contour plot, set \&'ZDV' with a call to
CPSETR and retrieve the value of \&'ZDV' with a call to CPGETC; if an unscaled
value is desired, use the internal parameter name \&'ZDU' in the call to CPGETC.
.tA "Real" "None"
.ne 4
.LI "\fB'ZMN'\fR"
\fBZ Minimum Value.\fR
The minimum value in the field, as found by CPRECT or CPSPRS.  For
output only.
.tA "Real" "None"
.LI "\fB'ZMX'\fR"
\fBZ Maximum Value.\fR
The maximum value in the field, as found by CPRECT or CPSPRS.  For
output only.
.tA "Real" "None"
.LE
.L1 "CONPACK Examples"
This section contains many examples that show
how to use various features of
CONPACK.  Each example includes one or more contour plots and the code that
produced those plots.
.sp
Example 1 includes six contour plots, identified as 1-1, 1-2, and so on,
illustrating a progression from a very simple contour plot to one of
considerable complexity.  The same array of data was used for all six.
.sp
Example 1-1 was produced as efficiently as possible, using the default
settings of CONPACK parameters and the lowest-quality characters.
.sp
Example 1-2 was drawn using various features to produce a more attractive
plot.  Characters of medium quality were used.
.sp
Example 1-3 is the same as Example 1-2, but with shading of that part of the
plot where the data values are less than zero.
.sp
Example 1-4 is the same as Example 1-3, but with the 2-D smoother turned on.
Note that there are two places on the plot where the 2-D smoothing has caused
contour lines to touch one another.  This can be corrected by using a greater
value for 'T2D', the tension on the splines.  (In this particular case, the
value 4.0 works well.)
.sp
Example 1-5 uses CPSPRS to do 3-D interpolation.  The resulting curves are
very smooth.  The range of the data has been increased somewhat.  Note that
the size of ZDNS (5152) is exactly 16 times the size of ZDAT (23 x 14); as a
result, CPSPRS uses 'ZDM' = 92 (4 x 23) and 'ZDN' = 56 (4 x 14), thus
preserving the same aspect ratio for the data grid.  If the size of ZDNS had
not been an exact multiple of the size of ZDAT and a perfect square, the
dimensions might have been chosen in such a way as to give a little different
aspect ratio.  This may or may not be of importance to the user.
.sp
Example 1-6 shows the same plot as that of Example 1-5, but it is mapped onto
a satellite-view projection of the globe.
.sp
Example 2 demonstrates the capability of putting more than one contour plot
on a frame.  It also illustrates how to shade the area between two different
contour levels and shows four of the ways in which a scale factor may be
selected.
.sp
Example 3 demonstrates the use of various kinds of mappings and the use of
a special value to signal missing data in a portion of the input data array.
The default version of CPMPXY is replaced by an alternate version, which does
the same mapping as the default for 'MAP' = 1 or 2, but does two other useful
mappings as well.  
.sp
In Example 3-1, the contour plot is mapped onto an EZMAP
background; longitude is a linear function of the first subscript, and
latitude is a linear function of the second subscript.  
.sp
In Example 3-2, the plot
is mapped onto a polar-coordinate background; rho is a linear function of the
first subscript, and theta is a linear function of the second subscript.  
.sp
In
Example 3-3, the grid is rectangular, but the X and Y coordinates are not
regularly spaced; their positions are specified by singly-dimensioned arrays
in a labeled common block.  
.sp
In Example 3-4, the grid is completely
distorted into the shape of a four-pointed star; the X and Y coordinates
corresponding to each index pair (I,J) are specified by doubly-dimensioned
arrays provided in another labeled common block.
.sp
Example 4 demonstrates how to specify all contour levels and associated
information exactly.  Intermediate contour levels (for which only labels, but
not lines, are drawn) are used to effect a labeling of the contour bands
defined by the other levels. The results are adequate, if not spectacular.
The utility SOFTFILL is used to produce the shading.
.sp
Example 5 shows the same data as Example 4.  Twenty levels of shading are
used.  No labels are placed on the plot; instead, 
the routine LBLBAR in the utility LABELBAR is used to draw
a label bar that
indicates the values associated with the shading.
.sp
Example 6 shows the use of color, but not for solid fill.  Contour lines for
positive values are shown in red, those for negative values are shown in blue,
and the zero contour is shown in white.  Highs and lows are labeled
with just the number (no 'H' or 'L') and are in green. The range of values is
intentionally such as to demonstrate
the use of exponents in all numeric labels; a
scale factor could, of course, be used to avoid this.
.sp
Example 7 shows the same data as Example 1, but with color fill of contour
bands and with a color-filled label bar
drawn by the routine LBLBAR, in the utility LABELBAR.
.sp
Example 8 shows a color-filled contour plot on the surface of the earth, in
orthographic projection, with a color bar 
drawn by the routine LBLBAR, in the utility LABELBAR.
It is assumed that the field being contoured has meaning only over the
African continent and, accordingly, contours are only shown there.  Lines
of latitude and longitude are drawn only over the ocean areas.
.sp
Example 9 shows four plots drawn by the subroutines CPEZCT and CPCNRC.
.sp
Example 10-1 shows a basic plot drawn by the subroutine CPEZCT.
.sp
Example 10-2 shows a basic plot drawn by the subroutine CPCNRC.
.sp
Example 10-3 shows a basic solid-fill plot drawn by CONPACK subroutines.
.L2 "Timing and Other Statistics"
The examples were run on the NCAR CRAY X-MP/48 under the Cray Operating System,
and the resulting timing information is listed 
on the table below.
.sp
.ce
.B
Timing Results
.R
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
.rm 80 81 82 83 84 85
.nr 80 0
.nr 38 \wExample
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNumber
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-1C
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-2H
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-2L
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-4
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-4A
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-4B
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-4C
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-5
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1-6
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2-1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2-2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2-3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2-4
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3-1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3-2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3-3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w3-4
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w4
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w5
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w6
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w7
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w8
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w9
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wCPU Time
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(in seconds)
.if \n(81<\n(38 .nr 81 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w.131
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w.114
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w.471
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w.591
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w.463
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w.718
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.056
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.246
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w.885
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w.764
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w3
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.308
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w5
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.287
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.255
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.460
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.186
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.193
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.746
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.196
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.172
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.175
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.368
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w3
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.333
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.030
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w.359
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w8
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.711
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w4
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.702
.if \n(32<\n(38 .nr 32 \n(38
.81
.rm 81
.nr 61 \n(31
.nr 38 \n(61+\n(32
.if \n(38>\n(81 .nr 81 \n(38
.if \n(38<\n(81 .nr 61 +(\n(81-\n(38)/2
.nr 82 0
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wWorkspace
.if \n(82<\n(38 .nr 82 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w100
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w500
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w500
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w100
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w100
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w120
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w-
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 62 \n(31
.nr 38 \n(62+\n(32
.if \n(38>\n(82 .nr 82 \n(38
.if \n(38<\n(82 .nr 62 +(\n(82-\n(38)/2
.nr 83 0
.nr 38 \wReal
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wWorkspace
.if \n(83<\n(38 .nr 83 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w200
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \wNA
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w1484
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1484
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1484
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1484
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1484
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w2684
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1484
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \wNA
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \w1700
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1592
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1692
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1692
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1592
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1692
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1592
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1592
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1592
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1592
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1588
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w200
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1486
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w200
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w400
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w-
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 63 \n(31
.nr 38 \n(63+\n(32
.if \n(38>\n(83 .nr 83 \n(38
.if \n(38<\n(83 .nr 63 +(\n(83-\n(38)/2
.nr 84 0
.nr 38 \wArea Map
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wSpace
.if \n(84<\n(38 .nr 84 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \wNA
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w1659
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1659
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1659
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w3820
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w6270
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w6450
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1659
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \wNA
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w9560
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w8390
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w7380
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w8490
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w6710
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w7230
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w7390
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w7050
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w7440
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w7480
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w17349
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w25229
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w2169
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w7643
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w84399
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w-
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 64 \n(31
.nr 38 \n(64+\n(32
.if \n(38>\n(84 .nr 84 \n(38
.if \n(38<\n(84 .nr 64 +(\n(84-\n(38)/2
.nr 85 0
.nr 38 \wMetafile Size
.if \n(85<\n(38 .nr 85 \n(38
.nr 38 \w(in bytes)
.if \n(85<\n(38 .nr 85 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w10080
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w10080
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w15840
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w24480
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w11520
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w28800
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w38880
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w38880
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w34560
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w17280
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w43200
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w67680
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w30600
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w30600
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w30600
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w30600
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w28080
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w28080
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w28080
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w28080
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w72000
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w208800
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w31680
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w21600
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w118080
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w187200
.if \n(31<\n(38 .nr 31 \n(38
.85
.rm 85
.nr 65 \n(31
.nr 38 \n(65+\n(32
.if \n(38>\n(85 .nr 85 \n(38
.if \n(38<\n(85 .nr 65 +(\n(85-\n(38)/2
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 61 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 62 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 63 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr 64 +\n(44
.nr 45 \n(84+(3*\n(38)
.nr 85 +\n(45
.nr 65 +\n(45
.nr TW \n(85
.if t .if \n(TW>\n(.li .tm Table at line 6906 file man/conpack.l is too wide - \n(TW units
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
.B
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Example\h'|\n(41u'CPU Time\h'|\n(42u'Integer\h'|\n(43u'Real\h'|\n(44u'Area Map\h'|\n(45u'Metafile Size
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Number\h'|\n(41u'(in seconds)\h'|\n(42u'Workspace\h'|\n(43u'Workspace\h'|\n(44u'Space\h'|\n(45u'(in bytes)
.R
.sp.5
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-1\h'|\n(41u'.131\h'|\n(42u'100\h'|\n(43u'200\h'|\n(44u'0\h'|\n(45u'10080
.ta \n(80u \n(61u \n(81u \n(62u \n(83u \n(84u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-1C\h'|\n(41u'.114\h'|\n(42u'500\h'|\n(43u'NA\h'|\n(44u'NA\h'|\n(45u'10080
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-2\h'|\n(41u'.471\h'|\n(42u'120\h'|\n(43u'1484\h'|\n(44u'1659\h'|\n(45u'15840
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-2H\h'|\n(41u'.591\h'|\n(42u'120\h'|\n(43u'1484\h'|\n(44u'1659\h'|\n(45u'24480
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-2L\h'|\n(41u'.463\h'|\n(42u'120\h'|\n(43u'1484\h'|\n(44u'1659\h'|\n(45u'11520
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-3\h'|\n(41u'.718\h'|\n(42u'120\h'|\n(43u'1484\h'|\n(44u'3820\h'|\n(45u'28800
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-4\h'|\n(41u'1.056\h'|\n(42u'120\h'|\n(43u'1484\h'|\n(44u'6270\h'|\n(45u'38880
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-4A\h'|\n(41u'1.246\h'|\n(42u'120\h'|\n(43u'2684\h'|\n(44u'6450\h'|\n(45u'38880
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-4B\h'|\n(41u'.885\h'|\n(42u'120\h'|\n(43u'1484\h'|\n(44u'1659\h'|\n(45u'34560
.ta \n(80u \n(61u \n(81u \n(62u \n(83u \n(84u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-4C\h'|\n(41u'.764\h'|\n(42u'500\h'|\n(43u'NA\h'|\n(44u'NA\h'|\n(45u'17280
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-5\h'|\n(41u'3.308\h'|\n(42u'120\h'|\n(43u'1700\h'|\n(44u'9560\h'|\n(45u'43200
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1-6\h'|\n(41u'5.287\h'|\n(42u'120\h'|\n(43u'1592\h'|\n(44u'8390\h'|\n(45u'67680
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2-1\h'|\n(41u'1.255\h'|\n(42u'120\h'|\n(43u'1692\h'|\n(44u'7380\h'|\n(45u'30600
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2-2\h'|\n(41u'1.460\h'|\n(42u'120\h'|\n(43u'1692\h'|\n(44u'8490\h'|\n(45u'30600
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2-3\h'|\n(41u'1.186\h'|\n(42u'120\h'|\n(43u'1592\h'|\n(44u'6710\h'|\n(45u'30600
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2-4\h'|\n(41u'1.193\h'|\n(42u'120\h'|\n(43u'1692\h'|\n(44u'7230\h'|\n(45u'30600
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3-1\h'|\n(41u'1.746\h'|\n(42u'120\h'|\n(43u'1592\h'|\n(44u'7390\h'|\n(45u'28080
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3-2\h'|\n(41u'1.196\h'|\n(42u'120\h'|\n(43u'1592\h'|\n(44u'7050\h'|\n(45u'28080
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3-3\h'|\n(41u'1.172\h'|\n(42u'120\h'|\n(43u'1592\h'|\n(44u'7440\h'|\n(45u'28080
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3-4\h'|\n(41u'1.175\h'|\n(42u'120\h'|\n(43u'1592\h'|\n(44u'7480\h'|\n(45u'28080
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'4\h'|\n(41u'2.368\h'|\n(42u'120\h'|\n(43u'1588\h'|\n(44u'17349\h'|\n(45u'72000
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'5\h'|\n(41u'3.333\h'|\n(42u'100\h'|\n(43u'200\h'|\n(44u'25229\h'|\n(45u'208800
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'6\h'|\n(41u'1.030\h'|\n(42u'120\h'|\n(43u'1486\h'|\n(44u'2169\h'|\n(45u'31680
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'7\h'|\n(41u'.359\h'|\n(42u'100\h'|\n(43u'200\h'|\n(44u'7643\h'|\n(45u'21600
.ta \n(80u \n(61u \n(81u \n(62u \n(63u \n(64u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'8\h'|\n(41u'8.711\h'|\n(42u'120\h'|\n(43u'400\h'|\n(44u'84399\h'|\n(45u'118080
.ta \n(80u \n(61u \n(81u \n(82u \n(83u \n(84u \n(65u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'9\h'|\n(41u'4.702\h'|\n(42u'-\h'|\n(43u'-\h'|\n(44u'-\h'|\n(45u'187200
.sp .5
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-37
.ne 9
.L3 "Notes on the Timing Results"
"CPU Time (in seconds)" is the time on the NCAR CRAY X-MP/48 under the Cray
Operating System.
.sp
"Integer Workspace" is the amount of integer workspace used, in integer 
storage units.
.sp
"Real Workspace" is the amount of real workspace used, in real
storage units.
.sp
"Area Map Space" is the amount of area map space used, 
in integer storage units.
.sp
.ne 6
The letter added to some example numbers indicates a related version of the
example, for which the code and plot are not given.  Each of these
examples is explained here:
.BL
.LI
Example 1-1C used CONREC to draw a plot much like that in Example 1-1.  Note
that it required a little less time.
.LI
Examples 1-2H and 1-2L were identical to Example 1-2, except that 1-2H
used high-quality characters and 1-2L low-quality characters.
.LI
Example 1-4A was identical to Example 1-4, except that 'LLP' = -3 (rather
than +3) was used to keep the 2-D smoother on while positioning the labels;
this required using 'RWC' = 200, rather than the default 100 (the reasons
for which are explained in the description of 'RWC').  The labels were
positioned only a little better, the time required to execute the penalty
scheme was increased, and the maximum real workspace requirements were
increased (because the space for the gradient array was required at the
same time as the space for the 2-D smoothing).
.LI
Example 1-4B was identical to Example 1-4, except that cross-hatching of
areas below the zero contour was not done.  This plot was done in order to
compare its time with that for 1-4C.
.LI
Example 1-4C used the super version of CONREC to draw a plot much like
that drawn by Example 1-4B.  Note that it required a little less time.
.LE
.sp 
The statistics given for Example 9 are for the combination of all four plots.
.L2 "Subroutines Used in More than One Example"
A number of routines are used in more than one of the examples.  Many of them
do useful things and are of interest to users.
The code for these common routines follows the code for Examples 1-9.
Brief descriptions are as follows:
.BL
.LI
The routine GENDAT is called to generate dummy data.
.br
.ne 3
.LI
The function FRAN is called by GENDAT; it simulates the behavior of a
random number generator, but produces a predictable string of numbers,
so that the exact plots shown in the examples may be duplicated.
.LI
The routine CAPSAP is called to compute and print statistics about each
plot or subplot.
.LI
The routine LABTOP is called to put a label above the current viewport.
.LI
The routine BNDARY is called to draw the boundary of the plotter frame.
.LI
The routine DFCLRS is called to define a set of color indices.
.LI
The function SECOND has the default value 0.  To get a meaningful 
timing result, replace it by a version that has
as its value the elapsed CPU time, in seconds.
.LE
.L1 "CONPACK Examples \(em Code"
.L2 "Code Common to Examples 1 through 9"
.in -.5i
.sF
      SUBROUTINE GENDAT (DATA,IDIM,M,N,MLOW,MHGH,DLOW,DHGH)
C
C This is a routine to generate test data for two-dimensional graphics
C routines.  Given an array "DATA", dimensioned "IDIM x 1", it fills
C the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
C of data having approximately "MLOW" lows and "MHGH" highs, a minimum
C value of exactly "DLOW" and a maximum value of exactly "DHGH".
C
C "MLOW" and "MHGH" are each forced to be greater than or equal to 1
C and less than or equal to 25.
C
C The function used is a sum of exponentials.
C
        DIMENSION DATA(IDIM,1),CCNT(3,50)
C
        FOVM=9./FLOAT(M)
        FOVN=9./FLOAT(N)
C
        NLOW=MAX0(1,MIN0(25,MLOW))
        NHGH=MAX0(1,MIN0(25,MHGH))
        NCNT=NLOW+NHGH
C
        DO 101 K=1,NCNT
          CCNT(1,K)=1.+(FLOAT(M)-1.)*FRAN()
          CCNT(2,K)=1.+(FLOAT(N)-1.)*FRAN()
          IF (K.LE.NLOW) THEN
            CCNT(3,K)=-1.
          ELSE
            CCNT(3,K)=+1.
          END IF
  101   CONTINUE
C
        DMIN=+1.E36
        DMAX=-1.E36
        DO 104 J=1,N
          DO 103 I=1,M
            DATA(I,J)=.5*(DLOW+DHGH)
            DO 102 K=1,NCNT
              DATA(I,J)=DATA(I,J) + .5 * (DHGH-DLOW) * CCNT(3,K) *
     +                  EXP( - ( ( FOVM*(FLOAT(I)-CCNT(1,K)) )**2 +
     +                           ( FOVN*(FLOAT(J)-CCNT(2,K)) )**2   ) )
  102       CONTINUE
            DMIN=AMIN1(DMIN,DATA(I,J))
            DMAX=AMAX1(DMAX,DATA(I,J))
  103     CONTINUE
  104   CONTINUE
C
        DO 106 J=1,N
          DO 105 I=1,M
            DATA(I,J)=(DATA(I,J)-DMIN)/(DMAX-DMIN)*(DHGH-DLOW)+DLOW
  105     CONTINUE
  106   CONTINUE
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      FUNCTION FRAN ()
        DIMENSION RSEQ (100)
        SAVE ISEQ
        DATA RSEQ / .749,.973,.666,.804,.081,.483,.919,.903,.951,.960 ,
     +              .039,.269,.270,.756,.222,.478,.621,.063,.550,.798 ,
     +              .027,.569,.149,.697,.451,.738,.508,.041,.266,.249 ,
     +              .019,.191,.266,.625,.492,.940,.508,.406,.972,.311 ,
     +              .757,.378,.299,.536,.619,.844,.342,.295,.447,.499 ,
     +              .688,.193,.225,.520,.954,.749,.997,.693,.217,.273 ,
     +              .961,.948,.902,.104,.495,.257,.524,.100,.492,.347 ,
     +              .981,.019,.225,.806,.678,.710,.235,.600,.994,.758 ,
     +              .682,.373,.009,.469,.203,.730,.588,.603,.213,.495 ,
     +              .884,.032,.185,.127,.010,.180,.689,.354,.372,.429 /
        DATA ISEQ / 0 /
        ISEQ=MOD(ISEQ,100)+1
        FRAN=RSEQ(ISEQ)
        RETURN
      END


=========================================================================
=========================================================================
 
      SUBROUTINE CAPSAP (LABL,TIME,IAMA,LAMA)
C
        DIMENSION IAMA(*)
C
        CHARACTER*(*) LABL
C
C Compute and print the time required to draw the contour plot and how
C much space was used in the various arrays.
C
        TIME=SECOND(DUMI)-TIME
        PRINT * , 'PLOT TITLE WAS ',LABL
        PRINT * , 'TIME TO DRAW PLOT WAS  ',TIME
        CALL CPGETI ('IWU - INTEGER WORKSPACE USAGE',IIWU)
        CALL CPGETI ('RWU - REAL WORKSPACE USAGE',IRWU)
        PRINT * , 'INTEGER WORKSPACE USED ',IIWU
        PRINT * , '   REAL WORKSPACE USED ',IRWU
        IF (LAMA.NE.0) THEN
          IAMU=LAMA-(IAMA(6)-IAMA(5)-1)
          PRINT * , '   AREA MAP SPACE USED ',IAMU
        END IF
C
C Done.
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE LABTOP (LABL,SIZE)
C
        CHARACTER*(*) LABL
C
C Put a label just above the top of the plot.  The SET call is re-done
C to allow for the use of fractional coordinates, and the text extent
C capabilities of the package PLOTCHAR are used to determine the label
C position.
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        SZFS=SIZE*(XVPR-XVPL)
        CALL    SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
        CALL PCGETI ('QU - QUALITY FLAG',IQUA)
        CALL PCSETI ('QU - QUALITY FLAG',0)
        CALL PCSETI ('TE - TEXT EXTENT COMPUTATION FLAG',1)
        CALL PLCHHQ (.5,.5,LABL,SZFS,360.,0.)
        CALL PCGETR ('DB - DISTANCE TO BOTTOM OF STRING',DBOS)
        CALL PLCHHQ (.5*(XVPL+XVPR),YVPT+SZFS+DBOS,LABL,SZFS,0.,0.)
        CALL PCSETI ('QU - QUALITY FLAG',IQUA)
        CALL    SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
C Done.
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE BNDARY
C
C Draw a line showing where the edge of the plotter frame is.
C
        CALL PLOTIF (0.,0.,0)
        CALL PLOTIF (1.,0.,1)
        CALL PLOTIF (1.,1.,1)
        CALL PLOTIF (0.,1.,1)
        CALL PLOTIF (0.,0.,1)
        CALL PLOTIF (0.,0.,2)
C
C Done.
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE DFCLRS
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


=========================================================================
=========================================================================
 
      FUNCTION SECOND (DUMI)
        SAVE IFLG
        DATA IFLG / 0 /
        IF (IFLG.EQ.0) THEN
          IFLG=1
          PRINT * , '**************************************************'
          PRINT * , '** THE DUMMY FUNCTION "SECOND" HAS BEEN CALLED. **'
          PRINT * , '** A ZERO WILL BE RETURNED AS ITS VALUE.  THIS  **'
          PRINT * , '** MEANS THAT TIMING INFORMATION PRINTED WILL   **'
          PRINT * , '** BE WRONG.  TO GET ACTUAL TIMINGS, YOU SHOULD **'
          PRINT * , '** REPLACE THIS FUNCTION WITH ONE HAVING AS ITS **'
          PRINT * , '** VALUE THE ELAPSED CPU TIME, IN SECONDS.  ON  **'
          PRINT * , '** THE CRAY, UNDER COS, THE SYSTEM FUNCTION     **'
          PRINT * , '** CALLED "SECOND" DOES THIS, SO YOU CAN JUST   **'
          PRINT * , '** DELETE THE DUMMY ONE.                        **'
          PRINT * , '**************************************************'
        END IF
        SECOND=0.
        RETURN
      END
.eF
.L2 "Examples 1-1 through 1-6"
.in -.5i
.sF
      PROGRAM EXMPL1
C
C Declare an array to hold the data to be contoured.
C
        DIMENSION ZDAT(23,14)
C
C Declare an array to hold dense data (5152=4x4x23x14).
C
        DIMENSION ZDNS(5152)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.
C
        DIMENSION IAMA(20000)
C
C Declare the arrays needed by ARSCAM for x/y coordinates.
C
        DIMENSION XCRA(1000),YCRA(1000)
C
C Declare the arrays needed by ARSCAM for area and group identifiers.
C
        DIMENSION IARA(10),IGRA(10)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Declare the routine which will draw contour lines, avoiding labels.
C
        EXTERNAL DRAWCL
C
C Declare the routine which does the shading.
C
        EXTERNAL SHADER
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,23,23,14,20,20,-136.148,451.834)
C
C Example 1-1 ---------------------------------------------------------
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Force PLOTCHAR to use characters of the lowest quality.
C
        CALL PCSETI ('QU - QUALITY FLAG',2)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,23,23,14,RWRK,5000,IWRK,1000)
C
C Draw the default background.
C
        CALL CPBACK (ZDAT,RWRK,IWRK)
C
C Draw contour lines and labels.
C
        CALL CPCLDR (ZDAT,RWRK,IWRK)
C
C Add the informational label and the high/low labels.
C
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-1',TIME,IAMA,0)
        CALL LABTOP ('EXAMPLE 1-1',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Example 1-2 ---------------------------------------------------------
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Make PLOTCHAR use medium-quality characters.
C
        CALL PCSETI ('QU - QUALITY FLAG',1)
C
C Turn on the positioning of labels by the penalty scheme.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
C
C Turn on the drawing of the high and low label boxes.
C
        CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',1)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label, another high/low label, or the edge.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',7)
C
C Tell CONPACK not to choose contour levels, so that the ones chosen
C for example 1-1 will be used.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION FLAG',0)
C
C Increase the line width for labelled levels.
C
        CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
        DO 101 ICLV=1,NCLV
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
          CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
          IF (ICLU.EQ.3) THEN
            CALL CPSETI ('CLL - CONTOUR-LINE LINE WIDTH',2)
          END IF
  101   CONTINUE
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,23,23,14,RWRK,5000,IWRK,1000)
C
C Draw the default background, using a wider line than normal.
C
        CALL GSLWSC (2.)
        CALL CPBACK (ZDAT,RWRK,IWRK)
        CALL GSLWSC (1.)
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,20000)
C
C Put label boxes into the area map.
C
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Draw contour lines, avoiding drawing them through the label boxes.
C
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
C
C Draw all the labels.
C
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-2',TIME,IAMA,20000)
        CALL LABTOP ('EXAMPLE 1-2',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Example 1-3 ---------------------------------------------------------
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Make PLOTCHAR use high-quality characters.
C
        CALL PCSETI ('QU - QUALITY FLAG',0)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label or another high/low label, but to move those which overlap the
C edge inward a little.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',11)
C
C Turn off the area identifiers for all except the zero contour and set
C its identifiers in such a way that we can shade the areas "below" that
C contour.
C
        DO 102 ICLV=1,NCLV
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
          CALL CPGETR ('CLV - CONTOUR LEVEL VALUE',CLEV)
          IF (CLEV.NE.0.) THEN
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',0)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',0)
          ELSE
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',2)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',1)
          END IF
  102   CONTINUE
C
C Draw the contour plot, using the same calls as for example 1-2.
C
        CALL CPRECT (ZDAT,23,23,14,RWRK,5000,IWRK,1000)
        CALL GSLWSC (2.)
        CALL CPBACK (ZDAT,RWRK,IWRK)
        CALL GSLWSC (1.)
        CALL ARINAM (IAMA,20000)
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Now, add the zero contour line to the area map.
C
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Scan the area map.  The routine SHADER will be called to shade the
C areas below the zero contour line.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-3',TIME,IAMA,20000)
        CALL LABTOP ('EXAMPLE 1-3',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Example 1-4 ---------------------------------------------------------
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Turn on the 2D smoother.
C
        CALL CPSETR ('T2D - TENSION ON THE 2D SPLINES',1.)
C
C Draw the contour plot, using the same calls as for example 1-3.
C
        CALL CPRECT (ZDAT,23,23,14,RWRK,5000,IWRK,1000)
        CALL GSLWSC (2.)
        CALL CPBACK (ZDAT,RWRK,IWRK)
        CALL GSLWSC (1.)
        CALL ARINAM (IAMA,20000)
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
        CALL CPLBDR (ZDAT,RWRK,IWRK)
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-4',TIME,IAMA,20000)
        CALL LABTOP ('EXAMPLE 1-4',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Example 1-5 ---------------------------------------------------------
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Make CONPACK set up the contour levels again (the range of the data
C may be increased by 3D interpolation), but force it to use the same
C contour interval and label interval that were used for the first four
C plots.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION FLAG',16)
        CALL CPGETR ('CIU - CONTOUR INTERVAL USED',CINU)
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',CINU)
        CALL CPGETI ('LIU - LABEL INTERVAL USED',LINU)
        CALL CPSETI ('LIS - LABEL INTERVAL SPECIFIER',LINU)
C
C Provide more room for storing coordinates used to trace contour
C lines.  The default is slightly too small to hold a complete line,
C and this causes some lines to have a couple of labels right next to
C one another.
C
        CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOUR TRACING',200)
C
C Turn off the 2D smoother.
C
        CALL CPSETR ('T2D - TENSION ON THE 2D SPLINES',0.)
C
C Initialize the drawing of the contour plot.
C
        CALL CPSPRS (ZDAT,23,23,14,RWRK,5000,IWRK,1000,ZDNS,5152)
C
C Force the selection of contour levels and tweak associated parameters.
C
        CALL CPPKCL (ZDNS,RWRK,IWRK)
C
        CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
        DO 103 ICLV=1,NCLV
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
          CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
          IF (ICLU.EQ.3) THEN
            CALL CPSETI ('CLL - CONTOUR LINE LINE WIDTH',2)
          END IF
          CALL CPGETR ('CLV - CONTOUR LEVEL VALUE',CLEV)
          IF (CLEV.NE.0.) THEN
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',0)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',0)
          ELSE
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',2)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',1)
          END IF
  103   CONTINUE
C
C The rest is pretty much the same as for example 1-4, but the array
C ZDNS is used in place of ZDAT.
C
        CALL GSLWSC (2.)
        CALL PERIM (0,0,0,0)
        CALL GSLWSC (1.)
        CALL ARINAM (IAMA,20000)
        CALL CPLBAM (ZDNS,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDNS,RWRK,IWRK,IAMA,DRAWCL)
        CALL CPLBDR (ZDNS,RWRK,IWRK)
        CALL CPCLAM (ZDNS,RWRK,IWRK,IAMA)
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-5',TIME,IAMA,20000)
        CALL LABTOP ('EXAMPLE 1-5',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Example 1-6 ---------------------------------------------------------
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Turn off the selection of contour levels, so that the set picked for
C example 1-5 will be used.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION FLAG',0)
C
C Draw an EZMAP background.  The perimeter and the grid are turned off.
C The "political + U.S. states" dataset is used and it is dotted.  We
C use a satellite-view projection, centered over the U.S., showing
C maximal area.
C
        CALL MAPSTI ('PE - PERIMETER',0)
        CALL MAPSTI ('GR - GRID',0)
        CALL MAPSTC ('OU - OUTLINE DATASET','PS')
        CALL MAPSTI ('DO - DOTTING OF OUTLINES',1)
        CALL MAPSTR ('SA - SATELLITE HEIGHT',1.13)
        CALL MAPROJ ('SV - SATELLITE-VIEW',40.,-95.,0.)
        CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
        CALL MAPDRW
C
C Tell CONPACK that the SET call has been done, force it to generate X
C coordinates that are longitudes and Y coordinates that are latitudes,
C turn on mapping to an EZMAP background, define the out-of-range value
C (returned by MAPTRN for an unprojectable point), and put the
C informational label in a different place.
C
        CALL CPSETI ('SET - DO SET-CALL FLAG',0)
        CALL CPSETR ('XC1 - X COORDINATE AT I = 1',-130.)
        CALL CPSETR ('XCM - X COORDINATE AT I = M',-60.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',10.)
        CALL CPSETR ('YCN - Y COORDINATE AT J = N',70.)
        CALL CPSETI ('MAP - MAPPING FLAG',1)
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
        CALL CPSETI ('ILP - INFORMATIONAL LABEL POSITIONING',3)
        CALL CPSETR ('ILX - INFORMATIONAL LABEL X POSITION',.5)
        CALL CPSETR ('ILY - INFORMATIONAL LABEL Y POSITION',-.02)
C
C The rest of the calls are just as in example 1-5, except that the
C perimeter is not drawn.
C
        CALL CPSPRS (ZDAT,23,23,14,RWRK,5000,IWRK,1000,ZDNS,5152)
        CALL ARINAM (IAMA,20000)
        CALL CPLBAM (ZDNS,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDNS,RWRK,IWRK,IAMA,DRAWCL)
        CALL CPLBDR (ZDNS,RWRK,IWRK)
        CALL CPCLAM (ZDNS,RWRK,IWRK,IAMA)
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-6',TIME,IAMA,20000)
        CALL LABTOP ('EXAMPLE 1-6',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C ---------------------------------------------------------------------
C
C Close GKS.
C
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE DRAWCL (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of DRAWCL draws the polyline defined by the points
C ((XCS(I),YCS(I)),I=1,NCS) if and only if none of the area identifiers
C for the area containing the polyline are negative.  The dash package
C routine CURVED is called to do the drawing.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Turn on drawing.
C
        IDR=1
C
C If any area identifier is negative, turn off drawing.
C
        DO 101 I=1,NAI
          IF (IAI(I).LT.0) IDR=0
  101   CONTINUE
C
C If drawing is turned on, draw the polyline.
C
        IF (IDR.NE.0) CALL CURVED (XCS,YCS,NCS)
C
C Done.
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE SHADER (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of SHADER shades the polygon whose edge is defined by
C the points ((XCS(I),YCS(I)),I=1,NCS) if and only, relative to edge
C group 3, its area identifier is a 1.  The package SOFTFILL is used
C to do the shading.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Define workspaces for the shading routine.
C
        DIMENSION DST(1100),IND(1200)
C
C Turn off shading.
C
        ISH=0
C
C If the area identifier for group 3 is a 1, turn on shading.
C
        DO 101 I=1,NAI
          IF (IAG(I).EQ.3.AND.IAI(I).EQ.1) ISH=1
  101   CONTINUE
C
C If shading is turned on, shade the area.  The last point of the
C edge is redundant and may be omitted.
C
        IF (ISH.NE.0) THEN
          CALL SFSETI ('ANGLE',45)
          CALL SFSETR ('SPACING',.006)
          CALL SFWRLD (XCS,YCS,NCS-1,DST,1100,IND,1200)
          CALL SFSETI ('ANGLE',135)
          CALL SFNORM (XCS,YCS,NCS-1,DST,1100,IND,1200)
        END IF
C
C Done.
C
        RETURN
C
      END
.eF
.L2 "Examples 2-1 through 2-4"
.in -.5i
.sF
      PROGRAM EXMPL2
C
C Declare an array to hold the data to be contoured.
C
        DIMENSION ZDAT(33,33)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.
C
        DIMENSION IAMA(20000)
C
C Declare the arrays needed by ARSCAM for x/y coordinates.
C
        DIMENSION XCRA(1000),YCRA(1000)
C
C Declare the arrays needed by ARSCAM for area and group identifiers.
C
        DIMENSION IARA(10),IGRA(10)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Declare the routine which will draw contour lines, avoiding labels.
C
        EXTERNAL DRAWCL
C
C Declare the routine which does the shading.
C
        EXTERNAL SHADER
C
C Dimension a character variable to hold plot labels.
C
        CHARACTER*11 LABL
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Turn on the drawing of the grid edge ("contour line number -1") and
C thicken it somewhat.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
C
C Turn on the positioning of labels by the penalty scheme and provide a
C little more room for X and Y coordinates defining contour lines, so
C as not to have labels right next to each other on a contour line.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
        CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOURS',200)
C
C Turn on the drawing of the high and low label boxes.
C
        CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',1)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label or another high/low label, but to move those which overlap the
C edge inward a little.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',11)
C
C Make all CONPACK-written characters a little bigger.
C
        CALL CPSETR ('CWM - CHARACTER WIDTH MULTIPLIER',1.25)
C
C Move the informational label into the lower left-hand corner and
C turn on the box around it, making the box thicker than normal.
C
        CALL CPSETR ('ILX - INFORMATIONAL LABEL X POSITION',.02)
        CALL CPSETR ('ILY - INFORMATIONAL LABEL Y POSITION',.02)
        CALL CPSETI ('ILP - INFORMATIONAL LABEL POSIIONING',-4)
        CALL CPSETI ('ILB - INFORMATIONAL LABEL BOX',1)
        CALL CPSETR ('ILL - INFORMATIONAL LABEL LINE WIDTH',2.)
C
C Change the text of the informational label.
C
        CALL CPSETC ('ILT - INFORMATIONAL LABEL TEXT',
     +               'CONTOUR FROM $CMN$ TO $CMX$ BY $CIU$ (X $SFU$)')
C
C Do four different plots, one in each quadrant.
C
        DO 102 IPLT=1,4
C
C Generate an array of test data.
C
          CALL GENDAT (ZDAT,33,33,33,20,20,.000025,.000075)
C
C Get the current elapsed time, in seconds.
C
          TIME=SECOND(DUMI)
C
C Move the viewport to the proper quadrant.
C
          CALL CPSETR ('VPL - VIEWPORT LEFT EDGE',
     +                 .0250+.4875*REAL(MOD(IPLT-1,2)))
          CALL CPSETR ('VPR - VIEWPORT RIGHT EDGE',
     +                 .4875+.4875*REAL(MOD(IPLT-1,2)))
          CALL CPSETR ('VPB - VIEWPORT BOTTOM EDGE',
     +                 .0250+.4875*REAL((4-IPLT)/2))
          CALL CPSETR ('VPT - VIEWPORT TOP EDGE',
     +                 .4875+.4875*REAL((4-IPLT)/2))
C
C Specify how the scale factor is to be selected.
C
          CALL CPSETI ('SFS - SCALE FACTOR SELECTION',-IPLT)
C
C Initialize the drawing of the contour plot.
C
          CALL CPRECT (ZDAT,33,33,33,RWRK,5000,IWRK,1000)
C
C Force the selection of contour levels, so that associated quantities
C may be tweaked.
C
          CALL CPPKCL (ZDAT,RWRK,IWRK)
C
C Increase the line width for labelled levels and turn off the area
C identifiers for all levels.
C
          CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
          DO 101 ICLV=1,NCLV
            CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
            CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
            IF (ICLU.EQ.3) THEN
              CALL CPSETI ('CLL - CONTOUR-LINE LINE WIDTH',2)
            END IF
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',0)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',0)
  101     CONTINUE
C
C Add two new levels for which no contour lines are to be drawn, but
C between which shading is to be done.
C
          NCLV=NCLV+2
          CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLV-1)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',.000045)
          CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',0)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',1)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',2)
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLV)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',.000055)
          CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',0)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',3)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',1)
C
C Draw the contour plot.
C
          CALL ARINAM (IAMA,20000)
          CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
          CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
          CALL CPLBDR (ZDAT,RWRK,IWRK)
          CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
          CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot and label it.
C
          LABL='EXAMPLE 2-'//CHAR(ICHAR('0')+IPLT)
          CALL CAPSAP (LABL,TIME,IAMA,20000)
          CALL LABTOP (LABL,.017)
C
  102   CONTINUE
C
C Put a boundary line at the edge of the plotter frame.
C
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE DRAWCL (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of DRAWCL draws the polyline defined by the points
C ((XCS(I),YCS(I)),I=1,NCS) if and only if none of the area identifiers
C for the area containing the polyline are negative.  The dash package
C routine CURVED is called to do the drawing.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Turn on drawing.
C
        IDR=1
C
C If any area identifier is negative, turn off drawing.
C
        DO 101 I=1,NAI
          IF (IAI(I).LT.0) IDR=0
  101   CONTINUE
C
C If drawing is turned on, draw the polyline.
C
        IF (IDR.NE.0) CALL CURVED (XCS,YCS,NCS)
C
C Done.
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE SHADER (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of SHADER shades the polygon whose edge is defined by
C the points ((XCS(I),YCS(I)),I=1,NCS) if and only, relative to edge
C group 3, its area identifier is a 1.  The package SOFTFILL is used
C to do the shading.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Define workspaces for the shading routine.
C
        DIMENSION DST(1100),IND(1200)
C
C Turn off shading.
C
        ISH=0
C
C If the area identifier for group 3 is a 1, turn on shading.
C
        DO 101 I=1,NAI
          IF (IAG(I).EQ.3.AND.IAI(I).EQ.1) ISH=1
  101   CONTINUE
C
C If shading is turned on, shade the area.  The last point of the
C edge is redundant and may be omitted.
C
        IF (ISH.NE.0) THEN
          CALL SFSETI ('ANGLE',45)
          CALL SFSETR ('SPACING',.006)
          CALL SFWRLD (XCS,YCS,NCS-1,DST,1100,IND,1200)
          CALL SFSETI ('ANGLE',135)
          CALL SFNORM (XCS,YCS,NCS-1,DST,1100,IND,1200)
        END IF
C
C Done.
C
        RETURN
C
      END
.eF
.L2 "Examples 3-1 through 3-4"
.in -.5i
.sF
      PROGRAM EXMPL3
C
C Declare an array to hold the data to be contoured.
C
        DIMENSION ZDAT(33,33)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.
C
        DIMENSION IAMA(20000)
C
C Declare the arrays needed by ARSCAM for x/y coordinates.
C
        DIMENSION XCRA(1000),YCRA(1000)
C
C Declare the arrays needed by ARSCAM for area and group identifiers.
C
        DIMENSION IARA(10),IGRA(10)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Declare the routine which will draw contour lines, avoiding labels.
C
        EXTERNAL DRAWCL
C
C Declare the routine which does the shading.
C
        EXTERNAL SHADER
C
C Dimension a character variable to hold plot labels.
C
        CHARACTER*11 LABL
C
C Declare common blocks required for communication with CPMPXY.
C
        COMMON /CPMPC1/ XFOI(33),YFOJ(33)
        COMMON /CPMPC2/ XFIJ(33,33),YFIJ(33,33)
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Turn on the positioning of labels by the penalty scheme and provide a
C little more room for X and Y coordinates defining contour lines, so
C as not to have labels right next to each other on a contour line.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
        CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOURS',200)
C
C Turn on the drawing of the high and low label boxes.
C
        CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',1)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label or another high/low label, but to move those which overlap the
C edge inward a little.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',11)
C
C Make all CONPACK-written characters a little bigger.
C
        CALL CPSETR ('CWM - CHARACTER WIDTH MULTIPLIER',1.25)
C
C Turn on the drawing of the grid edge ("contour line number -1") and
C thicken it somewhat.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
C
C Tell CONPACK to do no SET call.
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
C
C Turn on the special-value feature and the outlining of special-value
C areas ("contour line number -2"), using a double-width line.
C
        CALL CPSETR ('SPV - SPECIAL VALUE',1.E36)
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-2)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
C
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,33,33,33,20,20,-1.,1.)
C
C Put special values in a roughly circular area.
C
        DO 102 I=1,33
          DO 101 J=1,33
            IF (REAL(I-20)**2+REAL(J-10)**2.LT.25) ZDAT(I,J)=1.E36
  101     CONTINUE
  102   CONTINUE
C
C Do four different plots, one in each quadrant.
C
        DO 108 IPLT=1,4
C
C Get the current elapsed time, in seconds.
C
          TIME=SECOND(DUMI)
C
C Compute viewport parameters.
C
          XVPL=.0250+.5000*REAL(MOD(IPLT-1,2))
          XVPR=.4750+.5000*REAL(MOD(IPLT-1,2))
          YVPB=.0250+.5000*REAL((4-IPLT)/2)
          YVPT=.4750+.5000*REAL((4-IPLT)/2)
C
C For each of the four plots, use a different mapping function and
C create a different background.
C
          CALL CPSETI ('MAP - MAPPING FUNCTION',IPLT)
C
C EZMAP.
C
          IF (IPLT.EQ.1) THEN
            CALL MAPSTI ('GR - GRID INTERVAL',30)
            CALL MAPSTC ('OU - OUTLINE DATASET','CO')
            CALL MAPSTI ('DO - DOTTING OF OUTLINES',1)
            CALL MAPPOS (XVPL,XVPR,YVPB,YVPT)
            CALL MAPROJ ('OR - ORTHOGRAPHIC',40.,-95.,0.)
            CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
            CALL MAPDRW
            CALL CPSETR ('XC1 - LONGITUDE AT I = 1',-160.)
            CALL CPSETR ('XCM - LONGITUDE AT I = M',-30.)
            CALL CPSETR ('YC1 - LATITUDE AT J = 1',-10.)
            CALL CPSETR ('YCN - LATITUDE AT J = N',70.)
C
C Polar coordinates.
C
          ELSE IF (IPLT.EQ.2) THEN
            CALL CPSETR ('XC1 - RHO AT I = 1',.1)
            CALL CPSETR ('XCM - RHO AT I = M',1.)
            CALL CPSETR ('YC1 - THETA AT J = 1',0.)
            CALL CPSETR ('YCN - THETA AT J = N',90.)
            CALL SET    (XVPL,XVPR,YVPB,YVPT,0.,1.,0.,1.,1)
C
C Rectangular, irregularly-spaced.
C
          ELSE IF (IPLT.EQ.3) THEN
            CALL CPSETR ('XC1 - X COORDINATE AT I = 1',1.)
            CALL CPSETR ('XCM - X COORDINATE AT I = M',33.)
            CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',1.)
            CALL CPSETR ('YCN - Y COORDINATE AT J = N',33.)
            CALL SET    (XVPL,XVPR,YVPB,YVPT,0.,1.,0.,1.,1)
            DO 103 I=1,33
              XFOI(I)=ALOG10(1.+9.*REAL(I-1)/32.)
  103       CONTINUE
            DO 104 J=1,33
              YFOJ(J)=ALOG10(1.+9.*REAL(J-1)/32.)
  104       CONTINUE
C
C Parameterized distortion.
C
          ELSE IF (IPLT.EQ.4) THEN
            CALL CPSETR ('XC1 - X COORDINATE AT I = 1',1.)
            CALL CPSETR ('XCM - X COORDINATE AT I = M',33.)
            CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',1.)
            CALL CPSETR ('YCN - Y COORDINATE AT J = N',33.)
            CALL SET    (XVPL,XVPR,YVPB,YVPT,0.,1.,0.,1.,1)
            DO 106 I=1,33
              DO 105 J=1,33
                XFIJ(I,J)=REAL(I-1)/32.+(REAL(17-I)/64.)*
     +                                          (REAL(16-ABS(J-17))/16.)
                YFIJ(I,J)=REAL(J-1)/32.+(REAL(17-J)/64.)*
     +                                          (REAL(16-ABS(I-17))/16.)
  105         CONTINUE
  106       CONTINUE
C
          END IF
C
C Initialize the drawing of the contour plot.
C
          CALL CPRECT (ZDAT,33,33,33,RWRK,5000,IWRK,1000)
C
C Force the selection of contour levels, so that associated quantities
C may be tweaked.
C
          CALL CPPKCL (ZDAT,RWRK,IWRK)
C
C Increase the line width for labelled levels and turn off the area
C identifiers for all levels.
C
          CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
          DO 107 ICLV=1,NCLV
            CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
            CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
            IF (ICLU.EQ.3) THEN
              CALL CPSETI ('CLL - CONTOUR-LINE LINE WIDTH',2)
            END IF
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',0)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',0)
  107     CONTINUE
C
C Add two new levels for which no contour lines are to be drawn, but
C between which shading is to be done.
C
          NCLV=NCLV+2
          CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLV-1)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',-.15)
          CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',0)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',1)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',2)
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLV)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',+.15)
          CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',0)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',3)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',1)
C
C Draw the contour plot.
C
          CALL ARINAM (IAMA,20000)
          CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
          CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
          CALL CPLBDR (ZDAT,RWRK,IWRK)
          CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
          CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot and label it.
C
          LABL='EXAMPLE 3-'//CHAR(ICHAR('0')+IPLT)
          CALL CAPSAP (LABL,TIME,IAMA,20000)
          CALL LABTOP (LABL,.017)
C
  108   CONTINUE
C
C Put a boundary line at the edge of the plotter frame.
C
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE CPMPXY (IMAP,XINP,YINP,XOTP,YOTP)
C
C This version of CPMPXY implements four different mappings:
C
C   IMAP = 1 implies an EZMAP mapping.  XINP and YINP are assumed to be
C   the longitude and latitude, in degrees, of a point on the globe.
C
C   IMAP = 2 implies a polar coordinate mapping.  XINP and YINP are
C   assumed to be values of rho and theta (in degrees).
C
C   IMAP = 3 implies an orthogonal, but unequally-spaced mapping.  XINP
C   is assumed to lie in the range from 1 to M, YINP in the range from
C   1 to N, where M and N are the dimensions of the grid.  The common
C   block CPMPC1 contains arrays XFOI and YFOJ giving the X coordinates
C   associated with I = 1 to M and the Y coordinates associated with
C   J = 1 to N.
C
C   IMAP = 4 implies a generalized distortion.  XINP is assumed to lie
C   in the range from 1 to M, YINP in the range from 1 to N, where M
C   and N are the dimensions of the grid.  The common block CPMPC2
C   contains arrays XFIJ and YFIJ, giving the X and Y coordinates
C   associated with index pairs (I,J).
C
C Declare common blocks.
C
        COMMON /CPMPC1/ XFOI(33),YFOJ(33)
        COMMON /CPMPC2/ XFIJ(33,33),YFIJ(33,33)
C
C Do the mapping.
C
        IF (IMAP.EQ.1) THEN
          CALL MAPTRN (YINP,XINP,XOTP,YOTP)
        ELSE IF (IMAP.EQ.2) THEN
          XOTP=XINP*COS(.017453292519943*YINP)
          YOTP=XINP*SIN(.017453292519943*YINP)
        ELSE IF (IMAP.EQ.3) THEN
          I=MAX(1,MIN(32,INT(XINP)))
          J=MAX(1,MIN(32,INT(YINP)))
          XOTP=(REAL(I+1)-XINP)*XFOI(I)+(XINP-REAL(I))*XFOI(I+1)
          YOTP=(REAL(J+1)-YINP)*YFOJ(J)+(YINP-REAL(J))*YFOJ(J+1)
        ELSE IF (IMAP.EQ.4) THEN
          I=MAX(1,MIN(32,INT(XINP)))
          J=MAX(1,MIN(32,INT(YINP)))
          XOTP=(REAL(J+1)-YINP)*
     +    ((REAL(I+1)-XINP)*XFIJ(I,J  )+(XINP-REAL(I))*XFIJ(I+1,J  ))
     +    +(YINP-REAL(J))*
     +    ((REAL(I+1)-XINP)*XFIJ(I,J+1)+(XINP-REAL(I))*XFIJ(I+1,J+1))
          YOTP=(REAL(J+1)-YINP)*
     +    ((REAL(I+1)-XINP)*YFIJ(I,J  )+(XINP-REAL(I))*YFIJ(I+1,J  ))
     +    +(YINP-REAL(J))*
     +    ((REAL(I+1)-XINP)*YFIJ(I,J+1)+(XINP-REAL(I))*YFIJ(I+1,J+1))
        ELSE
          XOTP=XINP
          YOTP=YINP
        END IF
C
C Done.
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE DRAWCL (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of DRAWCL draws the polyline defined by the points
C ((XCS(I),YCS(I)),I=1,NCS) if and only if none of the area identifiers
C for the area containing the polyline are negative.  The dash package
C routine CURVED is called to do the drawing.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Turn on drawing.
C
        IDR=1
C
C If any area identifier is negative, turn off drawing.
C
        DO 101 I=1,NAI
          IF (IAI(I).LT.0) IDR=0
  101   CONTINUE
C
C If drawing is turned on, draw the polyline.
C
        IF (IDR.NE.0) CALL CURVED (XCS,YCS,NCS)
C
C Done.
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE SHADER (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of SHADER shades the polygon whose edge is defined by
C the points ((XCS(I),YCS(I)),I=1,NCS) if and only, relative to edge
C group 3, its area identifier is a 1.  The package SOFTFILL is used
C to do the shading.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Define workspaces for the shading routine.
C
        DIMENSION DST(1100),IND(1200)
C
C Turn off shading.
C
        ISH=0
C
C If the area identifier for group 3 is a 1, turn on shading.
C
        DO 101 I=1,NAI
          IF (IAG(I).EQ.3.AND.IAI(I).EQ.1) ISH=1
  101   CONTINUE
C
C If shading is turned on, shade the area.  The last point of the
C edge is redundant and may be omitted.
C
        IF (ISH.NE.0) THEN
          CALL SFSETI ('ANGLE',45)
          CALL SFSETR ('SPACING',.006)
          CALL SFWRLD (XCS,YCS,NCS-1,DST,1100,IND,1200)
          CALL SFSETI ('ANGLE',135)
          CALL SFNORM (XCS,YCS,NCS-1,DST,1100,IND,1200)
        END IF
C
C Done.
C
        RETURN
C
      END
.eF
.L2 "Example 4"
.in -.5i
.sF
      PROGRAM EXMPL4
C
C Declare an array to hold the data to be contoured.
C
        DIMENSION ZDAT(53,37)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.
C
        DIMENSION IAMA(24000)
C
C Declare the arrays needed by ARSCAM for x/y coordinates.
C
        DIMENSION XCRA(1000),YCRA(1000)
C
C Declare the arrays needed by ARSCAM for area and group identifiers.
C
        DIMENSION IARA(10),IGRA(10)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Declare an array to hold line labels.
C
        CHARACTER*4 LABL(10)
C
C Declare the routine which will draw contour lines, avoiding labels.
C
        EXTERNAL DRAWCL
C
C Declare the routine which does the shading.
C
        EXTERNAL SHADAM
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Define a set of line labels.
C
        DATA LABL / '0-1 ','1-2 ','2-3 ','3-4 ','4-5 ',
     +              '5-6 ','6-7 ','7-8 ','8-9 ','9-10' /
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Generate a relatively smooth array of test data, with values ranging
C from 0 to 10.
C
        CALL GENDAT (ZDAT,53,53,37,10,10,0.,10.)
C
C Initialize the software fill package to do the desired type of fill.
C
        CALL SFSETI ('TYPE OF FILL',-2)
        CALL SFSETI ('ANGLE OF FILL LINES',45)
        CALL SFSETR ('SPACING OF FILL LINES',.000625)
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Turn on the positioning of labels by the penalty scheme and provide a
C little more room for X and Y coordinates defining contour lines, so
C as not to have labels right next to each other on a contour line.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
        CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOURS',200)
C
C Turn off the drawing of high and low labels.
C
        CALL CPSETC ('HLT - HIGH/LOW LABEL TEXT',' ')
C
C Turn on the drawing of the grid edge ("contour line number -1") and
C thicken it somewhat.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
C
C Turn off the informational label.
C
        CALL CPSETC ('ILT - INFORMATIONAL LABEL TEXT',' ')
C
C Turn on line-label boxes.
C
        CALL CPSETI ('LLB - LINE LABEL BOXES',1)
C
C Force the use of contour lines at the values 1, 2, 3, ... 9 and
C provide for labelling, but not drawing, contour lines at levels
C .5, 1.5, 2.5, ... 9.5, with labels of the form "0-1", "1-2", ...,
C "9-10".  Arrange for the areas between contour lines drawn to be
C shaded.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION',0)
C
        CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',19)
C
        DO 101 I=1,9
          CALL CPSETI ('PAI - PARAMETER ARRAY IDENTIFIER',I)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',FLOAT(I))
          CALL CPSETI ('CLU - CONTOUR LEVEL USE',1)
          CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',I+1)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',I)
  101   CONTINUE
C
        DO 102 I=10,19
          CALL CPSETI ('PAI - PARAMETER ARRAY IDENTIFIER',I)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',FLOAT(I-10)+.5)
          CALL CPSETI ('CLU - CONTOUR LEVEL USE',2)
          CALL CPSETC ('LLT - LINE LABEL TEXT',LABL(I-9))
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',0)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',0)
  102   CONTINUE
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,53,53,37,RWRK,5000,IWRK,1000)
C
C Draw the contour plot.
C
        CALL ARINAM (IAMA,24000)
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
        CALL CPLBDR (ZDAT,RWRK,IWRK)
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADAM)
C
C Compute and print statistics for the plot and label it.
C
        CALL CAPSAP ('EXAMPLE 4',TIME,IAMA,24000)
        CALL LABTOP ('EXAMPLE 4',.017)
C
  106   CONTINUE
C
C Put a boundary line at the edge of the plotter frame.
C
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE DRAWCL (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of DRAWCL draws the polyline defined by the points
C ((XCS(I),YCS(I)),I=1,NCS) if and only if none of the area identifiers
C for the area containing the polyline are negative.  The dash package
C routine CURVED is called to do the drawing.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Turn on drawing.
C
        IDR=1
C
C If any area identifier is negative, turn off drawing.
C
        DO 101 I=1,NAI
          IF (IAI(I).LT.0) IDR=0
  101   CONTINUE
C
C If drawing is turned on, draw the polyline.
C
        IF (IDR.NE.0) CALL CURVED (XCS,YCS,NCS)
C
C Done.
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE SHADAM (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of SHADAM shades the polygon whose edge is defined by
C the points ((XCS(I),YCS(I)),I=1,NCS) if and only, relative to edge
C group 3, its area identifier is between 1 and 10.  The package
C SOFTFILL is used to do the shading; the density of the shading
C increases with the value of the area identifier.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Define workspaces for the shading routine.
C
        DIMENSION DST(1100),IND(1200)
C
C Turn off shading.
C
        ISH=0
C
C If the area identifier for group 3 is in the right range, turn on
C shading.
C
        DO 101 I=1,NAI
          IF (IAG(I).EQ.3.AND.IAI(I).GE.1.AND.IAI(I).LE.10) ISH=IAI(I)
  101   CONTINUE
C
C If shading is turned on, shade the area.  The last point of the
C edge is redundant and may be omitted.
C
        IF (ISH.NE.0) CALL SFSGFA (XCS,YCS,NCS,DST,1100,IND,1200,ISH-1)
C
C Done.
C
        RETURN
C
      END
.eF
.L2 "Example 5"
.in -.5i
.sF
      PROGRAM EXMPL5
C
C Declare an array to hold the data to be contoured.
C
        DIMENSION ZDAT(53,37)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.
C
        DIMENSION IAMA(40000)
C
C Declare the arrays needed by ARSCAM for x/y coordinates.
C
        DIMENSION XCRA(1000),YCRA(1000)
C
C Declare the arrays needed by ARSCAM for area and group identifiers.
C
        DIMENSION IARA(10),IGRA(10)
C
C Declare arrays to hold the list of indices and the list of labels
C required by the label-bar routine.
C
        DIMENSION LIND(20)
        CHARACTER*2 LLBS(21)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Declare the routine which does the shading of areas from an area map.
C
        EXTERNAL SHADAM
C
C Define the list of indices and the list of labels required by the
C label-bar routine.
C
        DATA LIND / 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19 /
C
        DATA LLBS / '0',' ','1',' ','2',' ','3',' ','4',' ','5',
     +              ' ','6',' ','7',' ','8',' ','9',' ','10' /
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Generate a relatively smooth array of test data, with values ranging
C from 0 to 10.
C
        CALL GENDAT (ZDAT,53,53,37,10,10,0.,10.)
C
C Initialize the software fill package to do the desired type of fill.
C
        CALL SFSETI ('TYPE OF FILL',-4)
        CALL SFSETI ('ANGLE OF FILL LINES',15)
        CALL SFSETR ('SPACING OF FILL LINES',.000625)
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Turn off line labels.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',0)
C
C Turn off the drawing of high and low labels.
C
        CALL CPSETC ('HLT - HIGH/LOW LABEL TEXT',' ')
C
C Turn on the drawing of the grid edge ("contour line number -1") and
C thicken it somewhat.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
C
C Turn off the informational label.
C
        CALL CPSETC ('ILT - INFORMATIONAL LABEL TEXT',' ')
C
C Force the use of contour lines at the values 1, 2, 3, ... 9 and
C values half-way between.  Arrange for the areas between contour lines
C to be shaded.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION',0)
C
        CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',19)
C
        DO 101 I=1,19
          CALL CPSETI ('PAI - PARAMETER ARRAY IDENTIFIER',I)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',REAL(I)/2.)
          CALL CPSETI ('CLU - CONTOUR LEVEL USE',1)
          CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',I+1)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',I)
  101   CONTINUE
C
C Force the plot into the upper portion of the frame.
C
        CALL CPSETR ('VPB - VIEWPORT BOTTOM',.25)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,53,53,37,RWRK,5000,IWRK,1000)
C
C Draw the contour plot.
C
        CALL ARINAM (IAMA,40000)
        CALL CPCLDR (ZDAT,RWRK,IWRK)
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADAM)
C
C Put a horizontal label bar at the bottom of the frame.
C
        CALL LBSETR ('WBL - WIDTH OF BOX LINES',2.)
        CALL LBLBAR (0,.05,.95,.15,.25,20,1.,.5,LIND,0,LLBS,21,1)
C
C Compute and print statistics for the plot and label it.
C
        CALL CAPSAP ('EXAMPLE 5',TIME,IAMA,40000)
        CALL LABTOP ('EXAMPLE 5',.017)
C
C Put a boundary line at the edge of the plotter frame.
C
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE SHADAM (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of SHADAM shades the area-map polygon whose edge is
C defined by the points ((XCS(I),YCS(I)),I=1,NCS) if and only, relative
C to edge group 3, its area identifier is between 1 and 20.  The density
C of the shading increases with the area identifier.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Declare the workspaces required by SFSGFA.
C
        DIMENSION DST(1100),IND(1200)
C
C Turn off shading.
C
        ISH=0
C
C If the area identifier for group 3 is in the right range, turn on
C shading.
C
        DO 101 I=1,NAI
          IF (IAG(I).EQ.3.AND.IAI(I).GE.1.AND.IAI(I).LE.20) ISH=IAI(I)
  101   CONTINUE
C
C If shading is turned on, shade the area.  The last point of the
C edge is redundant and may be omitted.
C
        IF (ISH.NE.0) CALL SFSGFA (XCS,YCS,NCS,DST,1100,IND,1200,ISH-1)
C
C Done.
C
        RETURN
C
      END
.eF
.L2 "Example 6"
.in -.5i
.sF
      PROGRAM EXMPL6
C
C Declare required data arrays and workspace arrays.
C
        DIMENSION ZDAT(27,23),RWRK(5000),IWRK(1000),IAMA(10000)
        DIMENSION IASF(13)
C
C Declare the routine which will draw contour lines, avoiding labels.
C
        EXTERNAL DRAWCL
C
C Define an array for GKS aspect source flags.
C
        DATA IASF / 13*1 /
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Define color indices.
C
        CALL DFCLRS
C
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,27,27,23,25,25,-362.362E11,451.834E11)
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Increase the approximate number of contour levels used.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION',25)
C
C Turn on the positioning of labels by the penalty scheme.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
C
C Label highs and low with just the number, boxed and colored green.
C
        CALL CPSETC ('HLT - HIGH/LOW TEXT','$ZDV$')
        CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',1)
        CALL CPSETI ('HLC - HIGH/LOW LABEL COLOR INDEX',9)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label, another high/low label, or the edge.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',7)
C
C Turn on the drawing of the grid edge ("contour line number -1"),
C thicken it somewhat, and make it white.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
        CALL CPSETI ('CLC - CONTOUR LINE COLOR',1)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,27,27,23,RWRK,5000,IWRK,1000)
C
C Force the selection of contour levels by CONPACK.
C
        CALL CPPKCL (ZDAT,RWRK,IWRK)
C
C Force the color of the negative contours to blue, the color of the
C positive contours to red, and the color of the zero contour to white.
C If a positive or negative contour is labelled, use a darker shade and
C make the color of the label match.
C
        CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
        DO 102 ICLV=1,NCLV
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
          CALL CPGETR ('CLV - CONTOUR LEVEL',CLEV)
          CALL CPGETI ('CLU - CONTOUR LEVEL USE',ICLU)
          IF (CLEV.LT.0.) THEN
            IF (ICLU.EQ.1) THEN
              CALL CPSETI ('CLC - CONTOUR LINE COLOR',7)
            ELSE
              CALL CPSETI ('CLC - CONTOUR LINE COLOR',6)
              CALL CPSETI ('LLC - LINE LABEL COLOR',6)
            END IF
          ELSE IF (CLEV.EQ.0.) THEN
            CALL CPSETI ('CLC - CONTOUR LINE COLOR',1)
            CALL CPSETI ('LLC - LINE LABEL COLOR',1)
          ELSE
            IF (ICLU.EQ.1) THEN
              CALL CPSETI ('CLC - CONTOUR LINE COLOR',13)
            ELSE
              CALL CPSETI ('CLC - CONTOUR LINE COLOR',14)
              CALL CPSETI ('LLC - LINE LABEL COLOR',14)
            END IF
          END IF
  102   CONTINUE
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,10000)
C
C Put label boxes in the area map.
C
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Draw contour lines, avoiding drawing through label boxes.
C
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
C
C Fill in the labels.
C
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line at the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 6',TIME,IAMA,10000)
        CALL LABTOP ('EXAMPLE 6',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE DRAWCL (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of DRAWCL draws the polyline defined by the points
C ((XCS(I),YCS(I)),I=1,NCS) if and only if none of the area identifiers
C for the area containing the polyline are negative.  The dash package
C routine CURVED is called to do the drawing.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Turn on drawing.
C
        IDR=1
C
C If any area identifier is negative, turn off drawing.
C
        DO 101 I=1,NAI
          IF (IAI(I).LT.0) IDR=0
  101   CONTINUE
C
C If drawing is turned on, draw the polyline.
C
        IF (IDR.NE.0) CALL CURVED (XCS,YCS,NCS)
C
C Done.
C
        RETURN
C
      END
.eF
.L2 "Example 7"
.in -.5i
.sF
      PROGRAM EXMPL7
C
C Declare required data arrays and workspace arrays.
C
        DIMENSION ZDAT(23,14),RWRK(1000),IWRK(1000),IAMA(12000)
        DIMENSION IASF(13)
        DIMENSION XCRA(1000),YCRA(1000)
        DIMENSION IAIA(10),IGIA(10)
C
C Declare arrays to hold the list of indices and the list of labels
C required by the label-bar routine.
C
        DIMENSION LIND(14)
        CHARACTER*10 LLBS(15)
C
C Declare the routine which will color the areas.
C
        EXTERNAL COLRAM
C
C Define an array for GKS aspect source flags.
C
        DATA IASF / 13*1 /
C
C Define the list of indices required by the label-bar routine.
C
        DATA LIND / 2,3,4,5,6,7,8,9,10,11,12,13,14,15 /
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Define color indices.
C
        CALL DFCLRS
C
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,23,23,14,20,20,-136.148,451.834)
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Force the plot into the upper portion of the frame.
C
        CALL CPSETR ('VPB - VIEWPORT BOTTOM',.25)
C
C Disallow the trimming of trailing zeroes.
C
        CALL CPSETI ('NOF - NUMERIC OMISSION FLAGS',0)
C
C Tell CONPACK to use 13 contour levels, splitting the range into 14
C equal bands, one for each of the 14 colors available.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',-13)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,23,23,14,RWRK,1000,IWRK,1000)
C
C Initialize the area map and put the contour lines into it.
C
        CALL ARINAM (IAMA,12000)
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Color the map.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IAIA,IGIA,10,COLRAM)
C
C Put black contour lines over the colored map.
C
        CALL GSPLCI (0)
        CALL CPCLDR (ZDAT,RWRK,IWRK)
        CALL GSPLCI (1)
C
C Draw a label bar for the plot, relating colors to values.
C
        CALL CPGETR ('ZMN',ZMIN)
        CALL CPGETR ('ZMX',ZMAX)
C
        DO 102 I=1,15
          CALL CPSETR ('ZDV - Z DATA VALUE',
     +                 ZMIN+REAL(I-1)*(ZMAX-ZMIN)/14.)
          CALL CPGETC ('ZDV - Z DATA VALUE',LLBS(I))
  102   CONTINUE
C
        CALL LBSETI ('CBL - COLOR OF BOX LINES',0)
        CALL LBLBAR (0,.05,.95,.15,.25,14,1.,.5,LIND,0,LLBS,15,1)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line at the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 7',TIME,IAMA,12000)
        CALL LABTOP ('EXAMPLE 7',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE COLRAM (XCRA,YCRA,NCRA,IAIA,IGIA,NAIA)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C The arrays XCRA and YCRA, for indices 1 to NCRA, contain the X and Y
C coordinates of points defining a polygon.  The area identifiers in
C the array IAIA, each with an associated group identifier in the array
C IGIA, tell us whether the polygon is to be color-filled or not.
C
C
C Assume the polygon will be filled until we find otherwise.
C
        IFLL=1
C
C If any of the area identifiers is negative, don't fill the polygon.
C
        DO 101 I=1,NAIA
          IF (IAIA(I).LT.0) IFLL=0
  101   CONTINUE
C
C Otherwise, fill the polygon in the color implied by its area
C identifier relative to edge group 3 (the contour-line group).
C
        IF (IFLL.NE.0) THEN
          IFLL=0
          DO 102 I=1,NAIA
            IF (IGIA(I).EQ.3) IFLL=IAIA(I)
  102     CONTINUE
          IF (IFLL.GT.0.AND.IFLL.LT.15) THEN
            CALL GSFACI (IFLL+1)
            CALL GFA (NCRA-1,XCRA,YCRA)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
.eF
.L2 "Example 8"
.in -.5i
.sF
      PROGRAM EXMPL8
C
C Declare required data arrays and workspace arrays.
C
        DIMENSION ZDAT(40,40),RWRK(1000),IWRK(1000),IAMA(100000)
        DIMENSION IASF(13)
        DIMENSION XCRA(5000),YCRA(5000)
        DIMENSION IAIA(10),IGIA(10)
C
C Declare arrays to hold the list of indices and the list of labels
C required by the label-bar routine.
C
        DIMENSION LIND(14)
        CHARACTER*10 LLBS(14)
C
C Declare a routine to color the areas represented by the area map.
C
        EXTERNAL COLRAM
C
C Declare a routine to draw contour lines over land only.
C
        EXTERNAL COLRCL
C
C Declare a routine to draw lat/lon lines over ocean only.
C
        EXTERNAL COLRLL
C
C Define an array for GKS aspect source flags.
C
        DATA IASF / 13*1 /
C
C Define the list of indices required by the label-bar routine.
C
        DATA LIND / 7,2,3,4,5,6,8,9,10,11,12,13,14,15 /
C
C Define the list of labels required by the label-bar routine.
C
        DATA LLBS / 'OCEAN  ' , 'LAND   ' , '< 0    ' , '0-10   '  ,
     +              '10-20  ' , '20-30  ' , '30-40  ' , '40-50  '  ,
     +              '50-60  ' , '60-70  ' , '70-80  ' , '80-90  '  ,
     +              '90-100 ' , '> 100  ' /
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Define color indices.
C
        CALL DFCLRS
C
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,40,40,40,15,15,-10.,110.)
C
C Get the current elapsed time, in seconds.
C
        TIME=SECOND(DUMI)
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,100000)
C
C Use EZMAP and EZMAPA to create a background.
C
        CALL MAPPOS (.01,.74,.01,.99)
        CALL MAPROJ ('OR - ORTHOGRAPHIC PROJECTION',15.,15.,0.)
        CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
        CALL MAPSTI ('EL - ELLIPTICAL BOUNDARY',1)
        CALL MAPSTC ('OU - OUTLINE DATASET','CO')
        CALL MAPSTI ('VS - VERTICAL STRIPPING',0)
        CALL MAPINT
        CALL MAPBLA (IAMA)
C
C Add a line segment to the area map to separate Africa from Eurasia.
C Because this line segment is added last, it will determine the area
C identifier for all of Africa (223).  Also, add a line cutting
C Madagascar in two, with the same area identifier on both sides, so
C that it will be treated as a part of Africa.
C
        CALL MAPITA (26.0,35.0,0,IAMA,1,223,0)
        CALL MAPITA (28.8,33.0,1,IAMA,1,223,0)
        CALL MAPITA (33.0,30.0,1,IAMA,1,223,0)
        CALL MAPIQA (IAMA,1,223,0)
C
        CALL MAPITA (-20.0,42.5,0,IAMA,1,223,223)
        CALL MAPITA (-20.0,50.0,1,IAMA,1,223,223)
        CALL MAPIQA (IAMA,1,223,223)
C
C Tell CONPACK not to do the SET call (since it's already been done),
C to use mapping function 1 (EZMAP background), and what range of X and
C Y coordinates to send into the mapping function.  The X coordinates
C will be treated as latitudes and will range from 40 degrees west of
C Greenwich to 55 degrees east of Greenwich, and the Y coordinates will
C be treated as latitudes and will range from 45 degrees south of the
C equator to 45 degrees north of the equator.
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
        CALL CPSETI ('MAP - MAPPING FLAG',1)
        CALL CPSETR ('XC1 - X COORDINATE AT I=1',-18.)
        CALL CPSETR ('XCM - X COORDINATE AT I=M',+52.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J=1',-35.)
        CALL CPSETR ('YCN - Y COORDINATE AT J=N',+38.)
C
C Tell CONPACK exactly what contour levels to use.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',1)
        CALL CPSETR ('CMN - CONTOUR LEVEL MINIMUM',0.)
        CALL CPSETR ('CMX - CONTOUR LEVEL MAXIMUM',100.)
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',10.)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,40,40,40,RWRK,1000,IWRK,1000)
C
C Add contour lines to the area map.
C
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Color the map.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,5000,IAIA,IGIA,10,COLRAM)
C
C Outline the continents in black, put black contour lines over the
C color map, and put gray lines of latitude and longitude over the
C ocean.
C
        CALL GSPLCI (0)
        CALL MAPLOT
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,COLRCL)
        CALL GSPLCI (2)
        CALL MAPGRM (IAMA,XCRA,YCRA,5000,IAIA,IGIA,10,COLRLL)
        CALL GSPLCI (1)
C
C Draw a label bar for the plot, relating colors to values.
C
        CALL LBSETI ('CBL - COLOR OF BOX LINES',0)
        CALL LBLBAR (1,.76,.99,.13,.87,14,.5,1.,LIND,0,LLBS,14,1)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line at the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 8',TIME,IAMA,100000)
        CALL LABTOP ('EXAMPLE 8',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE COLRAM (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*),IOCI(12)
C
C This routine is called to color an area from an area map.  Its
C coordinates are given by the NCRA coordinates in the arrays XCRA and
C YCRA.  For each I from 1 to NAGI, IAIA(I) is the area identifier of
C the area relative to the group whose group identifier is IGIA(I).
C
C Define an array of color indices associated with area identifiers.
C
        DATA IOCI / 3,4,5,6,8,9,10,11,12,13,14,15 /
C
C Find the area identifier for the area relative to groups 1 and 3.
C The first of these tells us whether the area is over land or water,
C and the second tells us what contour band the area is in.
C
        IAI1=-1
        IAI3=-1
C
        DO 101 I=1,NAGI
          IF (IGIA(I).EQ.1) IAI1=IAIA(I)
          IF (IGIA(I).EQ.3) IAI3=IAIA(I)
  101   CONTINUE
C
C Color-fill the area, using blue for any area over water, gray for any
C area over land which is not over Africa or is outside the contour
C plot, and a color depending on the contour level elsewhere.
C
        IF (IAI1.GT.0) THEN
          IF (MAPACI(IAI1).EQ.1) THEN
            CALL GSFACI (7)
            CALL GFA (NCRA-1,XCRA,YCRA)
          ELSE
            IF (IAI1.NE.223.OR.IAI3.LE.0) THEN
              CALL GSFACI (2)
              CALL GFA (NCRA-1,XCRA,YCRA)
            ELSE
              CALL GSFACI (IOCI(IAI3))
              CALL GFA (NCRA-1,XCRA,YCRA)
            END IF
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE COLRCL (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C This routine is called to draw a portion of a contour line which is
C wholly contained in some area of an area map.  Its coordinates are
C given by the NCRA coordinates in the arrays XCRA and YCRA.  For each
C I from 1 to NAGI, IAIA(I) is the area identifier of the area relative
C to the group whose group identifier is IGIA(I).
C
C Find the area identifier for the area relative to groups 1 and 3.
C The first of these tells us whether the area is over land or water,
C and the second tells us what contour band the area is in.
C
        IAI1=-1
        IAI3=-1
C
        DO 101 I=1,NAGI
          IF (IGIA(I).EQ.1) IAI1=IAIA(I)
          IF (IGIA(I).EQ.3) IAI3=IAIA(I)
  101   CONTINUE
C
C Draw the line only if the area it is in is over Africa and within
C the boundary of the contour plot.
C
        IF (IAI1.EQ.223.AND.IAI3.GT.0) CALL GPL (NCRA,XCRA,YCRA)
C
C Done.
C
        RETURN
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE COLRLL (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C This routine is called to draw a portion of a line of latitude or
C longitude which is wholly contained in some area of an area map.  Its
C coordinates are given by the NCRA coordinates in the arrays XCRA and
C YCRA.  For each I from 1 to NAGI, IAIA(I) is the area identifier of
C the area relative to the group whose group identifier is IGIA(I).
C
C Find the area identifier for the area relative to group 1, which will
C tell us whether the area is over land or water.
C
        IAI1=-1
C
        DO 101 I=1,NAGI
          IF (IGIA(I).EQ.1.AND.IAIA(I).GT.0) IAI1=IAIA(I)
  101   CONTINUE
C
C Draw the line only if it is over water.
C
        IF (IAI1.GT.0.AND.MAPACI(IAI1).EQ.1) CALL GPL (NCRA,XCRA,YCRA)
C
C Done.
C
        RETURN
C
      END
.eF
.L2 "Examples 9-1 through 9-4"
.in -.5i
.sF
      PROGRAM EXMPL9
C
C Define an array for the data.
C
        DIMENSION ZDAT(40,40)
C
C Generate dummy data.
C
        CALL GENDAT (ZDAT,40,40,40,14,14,-145.6,389.7)
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off clipping by GKS.
C
        CALL GSCLIP (0)
C
C Force the use of medium-quality characters by the package PLOTCHAR.
C
        CALL PCSETI ('QU - QUALITY OF CHARACTERS',1)
C
C Put a label at the top of the first plot.  (The SET call is not
C needed for CPEZCT, but for the labelling routine.)
C
        CALL SET (.05,.95,.05,.95,0.,1.,0.,1.,1)
        CALL LABTOP ('EXAMPLE 9-1',.017)
C
C Put a boundary line at the edge of the plotter frame.
C
        CALL BNDARY
C
C Contour the data, using the EZCNTR simulator.
C
        CALL CPEZCT (ZDAT,40,40)
C
C Contour a subset of the data, forcing a contour interval of 20, using
C the CONREC simulator.
C
        CALL CPCNRC (ZDAT(7,9),40,32,24,0.,0.,20.,0,0,-682)
C
C Put a boundary line at the edge of the plotter frame, label the plot,
C and advance the frame.
C
        CALL BNDARY
        CALL LABTOP ('EXAMPLE 9-2',.017)
        CALL FRAME
C
C Switch to the "penalty scheme" for positioning contour-line labels
C and change one of the constants which are used by it.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
        CALL CPSETR ('PC1 - PENALTY SCHEME CONSTANT 1',1.5)
C
C Turn on the smoother, with a relatively high tension.
C
        CALL CPSETI ('T2D - TENSION ON THE 2D SMOOTHER',4)
C
C Force the labelling of every other contour line.  (This will only
C work when the contour interval is forced, as it will be in the next 
C call to CPCNRC.)
C
        CALL CPSETI ('LIS - LABEL INTERVAL SPECIFIER',2)
C
C Repeat the last plot, forcing a contour interval of 10.
C
        CALL CPCNRC (ZDAT(7,9),40,32,24,0.,0.,10.,0,0,-682)
C
C Put a boundary line at the edge of the plotter frame, label the plot,
C and advance the frame.
C
        CALL BNDARY
        CALL LABTOP ('EXAMPLE 9-3',.017)
        CALL FRAME
C
C Create an EZMAP background.
C
        CALL MAPSTI ('PE - PERIMETER',0)
        CALL MAPSTI ('GR - GRID',0)
        CALL MAPSTC ('OU - OUTLINE DATASET','PS')
        CALL MAPSTI ('DO - DOTTING OF OUTLINES',1)
        CALL MAPSTR ('SA - SATELLITE HEIGHT',1.13)
        CALL MAPROJ ('SV - SATELLITE-VIEW',40.,-95.,0.)
        CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
        CALL MAPDRW
C
C Arrange for output from CPCNRC to be placed on the EZMAP background.
C
        CALL CPSETR ('XC1 - X COORDINATE AT I = 1',-130.)
        CALL CPSETR ('XCM - X COORDINATE AT I = M',-60.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',10.)
        CALL CPSETR ('YCN - Y COORDINATE AT J = N',70.)
        CALL CPSETI ('MAP - MAPPING FLAG',1)
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C Define some special values and arrange for the special-value area to
C be outlined.
C
        ZDAT(15,13)=1.E36
        ZDAT(16,13)=1.E36
        ZDAT(15,14)=1.E36
        CALL CPSETR ('SPV - OUT-OF-RANGE VALUE',1.E36)
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-2)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
C
C Specify that high/low labels are to be written at an angle of 30
C degrees, using relatively small characters.  (These parameters will
C really be used for point-value labels.)
C
        CALL CPSETR ('HLA - HIGH/LOW LABEL ANGLE',30.)
        CALL CPSETR ('HLS - HIGH/LOW LABEL CHARACTER SIZE',.008)
C
C Turn off line labelling.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',0)
C
C Use the CONREC simulator to plot a subset of the data on the EZMAP
C background, labelling each data point.
C
        CALL CPCNRC (ZDAT(9,7),40,20,20,0.,0.,10.,1,1,-682)
C
C Put a boundary line at the edge of the plotter frame, label the plot,
C and advance the frame.
C
        CALL BNDARY
        CALL LABTOP ('EXAMPLE 9-4',.017)
        CALL FRAME
C
C Close GKS.
C
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END
.eF
.L2 "Examples 10-1 through 10-3"
.in -.5i
.sF
Note:  The user will have to provide a driver similar to DCONPA below in
order to utilize test subroutine TCONPA, which is on the distribution tape.

      PROGRAM DCONPA
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL TCONPA (IER)
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
 
      SUBROUTINE TCONPA (IERR)
C
C PURPOSE               To provide a simple demonstration of the use of
C                       CONPACK to contour regularly-spaced rectangular
C                       data.
C
C USAGE                 CALL TCONPA (IERR)
C
C ARGUMENTS (OUTPUT)    IERR
C
C                         An integer variable
C                         = 0, if the test was successful,
C                         = 1, otherwise
C
C I/O                   If the test is successful, the message "CONPACK
C                       TEST EXECUTED--SEE PLOTS TO CERTIFY" is printed
C                       on unit 6.  In addition, three frames are drawn
C                       on the graphics device.  In order to determine
C                       if the test was successful, it is necessary to
C                       examine these frames.
C
C PRECISION             Single.
C
C LANGUAGE              FORTRAN 77.
C
C REQUIRED ROUTINES     The AREAS, CONPACK, LABELBAR, SOFTFILL, and
C                       SPPS packages.
C
C REQUIRED GKS LEVEL    0A.
C
C ALGORITHM             The function
C
C                         Z(X,Y) = X + Y + 1./((X-.1)**2+Y**2+.09)
C                                         -1./((X+.1)**2+Y**2+.09)
C
C                       for X = -1. to 1. in increments of .1, and Y =
C                       -1.2 to 1.2 in increments of .1, is computed.
C                       Then, entries CPEZCT and CPCNRC are called to
C                       generate contour plots of Z.
C
C ZDAT contains the values to be plotted.
C
        DIMENSION  ZDAT(21,25)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Define real and integer work spaces and an area-map array.
C
        DIMENSION RWRK(1000),IWRK(1000),IAMA(20000)
C
C Define arrays for use by ARSCAM.
C
        DIMENSION XCRA(1000),YCRA(1000),IAIA(10),IGIA(10)
C
C Declare arrays to hold the list of indices and the list of labels
C required by the label-bar routine.
C
        DIMENSION LIND(10)
        CHARACTER*2 LLBS(9)
C
C Declare the routine which will color the areas.
C
        EXTERNAL CPCOLR
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Define the list of indices and the list of labels required by the
C label-bar routine.
C
        DATA LIND / 6,7,8,9,10,11,12,13,14,15 /
C
        DATA LLBS / '-4','-3','-2','-1',' 0',' 1',' 2',' 3',' 4' /
C
C Initialize the error parameter.
C
        IERR = 0
C
C Fill the 2D array to be plotted.
C
        DO  102 I=1,21
          X=.1*REAL(I-11)
          DO 101 J=1,25
            Y = .1*REAL(J-13)
            ZDAT(I,J)=X+Y+1./((X-.10)**2+Y**2+.09)
     +                   -1./((X+.10)**2+Y**2+.09)
  101     CONTINUE
  102   CONTINUE
C
C Frame 1 -- CPEZCT.
C
C The routine CPEZCT requires only the array name and dimensions.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL WTSTR (.5,.9765,
     +            'DEMONSTRATION PLOT FOR CONPACK ROUTINE CPEZCT',2,0,0)
C
      CALL CPEZCT (ZDAT,21,25)
C
C Frame 2 -- CPCNRC.
C
C The routine CPCNRC is called just like the old routine CONREC.
C
C In this example, the lowest contour level (-4.5), the highest contour
C level (4.5), and the increment between contour levels (0.3) are set.
C Line labels are positioned using the penalty scheme and the smoother
C is turned on.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL WTSTR (.5,.9765,
     +            'DEMONSTRATION PLOT FOR CONPACK ROUTINE CPCNRC',2,0,0)
C
      CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
      CALL CPSETR ('T2D - TENSION ON 2D SPLINES',3.6)
      CALL CPCNRC (ZDAT,21,21,25,-4.5,4.5,.3,0,0,0)
      CALL FRAME
C
C Frame 3 - A solid-filled contour plot.
C
C Put a label at the top of the frame.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL WTSTR (.5,.9765,
     +            'DEMONSTRATION PLOT FOR BASIC CONPACK ROUTINES',2,0,0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Define a bunch of color indices.
C
        CALL CPCLRS
C
C Force the plot into the left side of the frame.
C
        CALL CPSETR ('VPR - VIEWPORT RIGHT',.75)
C
C Force the use of exactly 9 contour levels, specify those levels, and
C define exactly what is to be done at each level.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',0)
        CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',9)
C
        DO 103 I=1,9
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)
          CALL CPSETR ('CLV - CONTOUR LEVEL',REAL(I-5))
          CALL CPSETI ('CLU - CONTOUR LEVEL USE',1)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',I)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',I+1)
  103   CONTINUE
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,21,21,25,RWRK,1000,IWRK,1000)
C
C Initialize the area map and put the contour lines into it.
C
        CALL ARINAM (IAMA,20000)
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Color the map.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IAIA,IGIA,10,CPCOLR)
C
C Put black contour lines over the colored map.
C
        CALL GSPLCI (0)
        CALL CPCLDR (ZDAT,RWRK,IWRK)
        CALL GSPLCI (1)
C
C Draw a color bar for the plot.
C
        CALL LBSETI ('CBL - COLOR OF BOX LINES',2)
        CALL PCSETR ('CS - CONSTANT SPACING FLAG',1.25)
        CALL LBLBAR (1,.80,.95,.05,.95,10,.5,1.,LIND,0,LLBS,9,1)
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
 1001 FORMAT (' CONPACK TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END


=========================================================================
=========================================================================
 
      SUBROUTINE CPCLRS
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


=========================================================================
=========================================================================
 
      SUBROUTINE CPCOLR (XCRA,YCRA,NCRA,IAIA,IGIA,NAIA)
C
        DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C The arrays XCRA and YCRA, for indices 1 to NCRA, contain the X and Y
C coordinates of points defining a polygon.  The area identifiers in
C the array IAIA, each with an associated group identifier in the array
C IGIA, tell us whether the polygon is to be color-filled or not.
C
C
C Assume the polygon will be filled until we find otherwise.
C
        IFLL=1
C
C If any of the area identifiers is negative, don't fill the polygon.
C
        DO 101 I=1,NAIA
          IF (IAIA(I).LT.0) IFLL=0
  101   CONTINUE
C
C Otherwise, fill the polygon in the color implied by its area
C identifier relative to edge group 3 (the contour-line group).
C
        IF (IFLL.NE.0) THEN
          IFLL=0
          DO 102 I=1,NAIA
            IF (IGIA(I).EQ.3) IFLL=IAIA(I)
  102     CONTINUE
          IF (IFLL.GE.1.AND.IFLL.LE.10) THEN
            CALL GSFACI (IFLL+5)
            CALL GFA (NCRA-1,XCRA,YCRA)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
.eF
.L1 "CONPACK Errors"
CONPACK uses the subroutine SETER in the error handling
package ERPRT77 to generate error messages.  (See Appendix A
for more information about the NCAR Graphics Error Handling
Package.)
.sp
Errors originating in CONPACK generate error messages that give
the name of the subroutine in which the error occurred.  All such names
start with the letters CP.
The error messages are divided here into two alphabetical
groups:
.sp
Group 1:  Errors generated by the user-callable subroutines
.sp
Group 2:  Errors generated by internal subroutines, which are called by
.ti +.75i
the user-callable subroutines
.L2 "Notes to Help You Interpret Error Messages"
.BL
.LI
The descriptions of messages that refer to an error exit from a GKS
subroutine (one with a name of the form GQ\fIxxxx\fR)
state, "Probably indicates that GKS is in the wrong state."
In this case, you need to examine all calls that change the state of GKS to
isolate the error.  Common errors 
are that GKS was not opened or that GKS was closed prematurely.
.LI
In Group 2 errors, three error messages generated by internal
subroutines say,
"SEE SPECIALIST."
In this case,
consult the NCAR Graphics package installer or 
your NCAR Graphics site representative 
if you cannot solve the problem.
There may be an error in package installation.
.LI
All but two of
the error messages are written by a call to the error-handling support
routine SETER, with a final argument indicating that the error is fatal and
that execution should be terminated.  The two exceptions are
in Group 2 and are marked with an asterisk.
.LE
.L2 "Group 1:  Errors Generated by User-callable Subroutines"
CPBACK - INITIALIZATION CALL NOT DONE
A call to CPRECT or CPSPRS has been omitted.
.sp
CPCLAM - INITIALIZATION CALL NOT DONE
A call to CPRECT or CPSPRS has been omitted.
.sp
CPCLAM - NO CONTOUR LEVELS DEFINED
The internal parameter \&'CLS' has the value 0 (indicating that the user intends to
supply contour levels), but no contour levels have been supplied.
.sp
CPCLDM - INITIALIZATION CALL NOT DONE
A call to CPRECT or CPSPRS has been omitted.
.sp
.ne 3
CPCLDM - NO CONTOUR LEVELS DEFINED
The internal parameter \&'CLS' has the value 0 (indicating that the user intends to
supply contour levels), but no contour levels have been supplied.
.sp
CPCLDM - ERROR EXIT FROM GQPLCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPCLDM - ERROR EXIT FROM GQTXCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPCLDR - INITIALIZATION CALL NOT DONE
A call to CPRECT or CPSPRS has been omitted.
.sp
CPCLDR - NO CONTOUR LEVELS DEFINED
The internal parameter \&'CLS' has the value 0 (indicating that the user intends to
supply contour levels), but no contour levels have been supplied.
.sp
CPCLDR - ERROR EXIT FROM GQPLCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPCLDR - ERROR EXIT FROM GQTXCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPCNRC - NO CONTOUR LEVELS IN SPECIFIED RANGE
CPCNRC has been instructed to choose contour levels, but not to use any
levels outside the range (FLOW,FHGH).  None of the chosen contour levels is in
that range.  What this most likely means is that the range of the data in the
array ZDAT is not what the user thought it should be.
.sp
CPGETC - PARAMETER NAME TOO SHORT - x
The internal parameter name (\fIx\fR) is less than three characters long.
.sp
CPGETC - GETTING xxx - PAI INCORRECT
An attempt has been made to get an element of the internal parameter array
named \fIxxx\fR,
and the current value of \&'PAI' (the parameter array index) is inappropriate
for that internal parameter array.
.sp
CPGETC - PARAMETER NAME NOT KNOWN - xxx
The given internal parameter name (\fIxxx\fR) is not one of the legal parameter names.
.sp
.ne 2
CPGETI OR CPGETR - PARAMETER NAME TOO SHORT - x
The internal parameter name (\fIx\fR) is less than three characters long.
.sp
CPGETI OR CPGETR - GETTING xxx - PAI INCORRECT
An attempt has been made to get an element of the internal parameter array
named \fIxxx\fR,
and the current value of \&'PAI' (the parameter array index) is inappropriate
for that internal parameter array.
.sp
CPGETI OR CPGETR - PARAMETER NAME NOT KNOWN - xxx
The given internal parameter name (\fIxxx\fR) is not one of the legal
parameter names.
.sp
CPLBAM - INITIALIZATION CALL NOT DONE
A call to CPRECT or CPSPRS has been omitted.
.sp
CPLBDR - INITIALIZATION CALL NOT DONE
A call to CPRECT or CPSPRS has been omitted.
.sp
CPLBDR - ERROR EXIT FROM GQPLCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPLBDR - ERROR EXIT FROM GQTXCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPLBDR - ERROR EXIT FROM GQFACI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPPKCL - INITIALIZATION CALL NOT DONE
A call to CPRECT or CPSPRS has been omitted.
.sp
CPPKCL - TOO MANY CONTOUR LEVELS
Indicates that the internal parameter \&'CLS' has been given a negative value whose
absolute value is too large, requesting more than 256 contour levels.
.sp
CPPKLB - INITIALIZATION CALL NOT DONE
A call to CPRECT or CPSPRS has been omitted.
.sp
CPPKLB - NO CONTOUR LEVELS DEFINED
The internal parameter \&'CLS' has the value 0 (indicating that the user intends to
supply contour levels), but no contour levels have been supplied.
.sp
CPPKLP - INITIALIZATION CALL NOT DONE
A call to CPRECT or CPSPRS has been omitted.
.sp
.ne 2
CPSETC - PARAMETER NAME TOO SHORT - x
The internal parameter name (\fIx\fR) is less than three characters long.
.sp
.ne 4
CPSETC - SETTING xxx - PAI INCORRECT
An attempt has been made to set an element of the internal parameter array
named \fIxxx\fR,
and the current value of \&'PAI' (the parameter array index) is inappropriate
for that internal parameter array.
.sp
CPSETC - PARAMETER NAME NOT KNOWN - xxx
The given internal parameter name (\fIxxx\fR) is not one of the legal parameter names.
.sp
CPSETI OR CPSETR - PARAMETER NAME TOO SHORT - x
The internal parameter name (\fIx\fR) is less than three characters long.
.sp
CPSETI OR CPSETR - SETTING xxx - PAI INCORRECT
An attempt has been made to set an element of the internal parameter array
named \fIxxx\fR,
and the current value of \&'PAI' (the parameter array index) is inappropriate
for that internal parameter array.
.sp
CPSETI OR CPSETR - NCL LESS THAN 1 OR GREATER THAN n
An attempt has been made to set the number of contour levels to an illegal
value.  The value of \fIn\fR is the largest value that may be used.
.sp
CPSETI OR CPSETR - PARAMETER NAME NOT KNOWN - xxx
The given internal parameter name (\fIxxx\fR) is not one of the legal parameter names.
.sp
CPSPRS - IZD1, IZDM, OR IZDN SET INCORRECTLY
IZDS has been given a zero value, indicating that the user intends to supply
the values of IZD1, IZDM, and IZDN, and their current values are incorrect.
.sp
CPSPRS - SPECIAL-VALUE REPLACEMENT FAILURE
There are no two adjacent values in the sparse array that are not special
values.  Most likely, the entire sparse array is filled with special values.
.sp
.ne 5
CPSPRS - CANNOT CONTINUE WITHOUT WORKSPACE
Insufficient workspace has been provided for the execution of CPSPRS.  The
amount of space that it requires is entirely predictable.  See
"Workspace Management" under "CONPACK Organization and Philosophy," 
earlier in this chapter.
.L2 "Group 2:  Errors Generated by Internal Subroutines"
CPCFLB - ERROR EXIT FROM GQFACI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
.ne 3
CPCFLB - ERROR EXIT FROM GQPLCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
.ne 3
CPCFLB - ERROR EXIT FROM GQTXCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPGIWS - ARGUMENT ERROR - SEE SPECIALIST
Probably indicates an error in the implementation of the package.
See the NCAR Graphics package installer or your NCAR Graphics site
representative.
.sp
.ti -.15i
* CPGIWS~~~\fIm\fR WORDS REQUESTED~~~\fIn\fR WORDS AVAILABLE
This error is not necessarily fatal.  In any case, the plot will
be incomplete.
The values \fIm\fR and \fIn\fR indicate how much integer workspace would be needed
to continue executing the current CONPACK subroutine and how much 
additional integer
workspace is currently left.  Note that 
supplying an additional \fIm\fR words on a
subsequent attempt will get you past this point, but will not ensure
immunity from a subsequent failure.
.sp
CPGIWS - INTEGER WORKSPACE OVERFLOW
The internal parameter \&'WSO' has a value indicating that integer workspace overflow
should be treated as a fatal error, and such an overflow has occurred.
.sp
CPGRWS - ARGUMENT ERROR - SEE SPECIALIST
Probably indicates an error in the implementation of the package.
See the NCAR Graphics package installer or your NCAR Graphics site
representative.
.sp
.ti -.15i
* CPGRWS~~~\fIm\fR WORDS REQUESTED~~~\fIn\fR WORDS AVAILABLE
This error is not necessarily fatal.  In any case, the plot will
be incomplete.
The values \fIm\fR and \fIn\fR indicate how much real workspace would be needed
to continue executing the current CONPACK subroutine and how much additional real
workspace is currently left.  Note that supplying an additional \fIm\fR words on a
subsequent attempt will get you past this point, but will not ensure
immunity from a subsequent failure.
.sp
.n3 4
CPGRWS - REAL WORKSPACE OVERFLOW
The internal parameter \&'WSO' has a value indicating that real workspace overflow
should be treated as a fatal error, and such an overflow has occurred.
.sp
.ne
CPHLLB - ERROR EXIT FROM GQPLCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPHLLB - ERROR EXIT FROM GQTXCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPHLLB - ERROR EXIT FROM GQFACI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPINLB - ERROR EXIT FROM GQFACI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPINLB - ERROR EXIT FROM GQPLCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPINLB - ERROR EXIT FROM GQTXCI
An attempt to get a color index has resulted in an error.  Probably indicates
that GKS is in the wrong state.
.sp
CPTRES - ALGORITHM FAILURE - SEE SPECIALIST
An error has occurred while trying to trace the edge of a special-value area.
This should not be possible.
See the NCAR Graphics package installer or your NCAR Graphics site
representative.
.sp
CPTREV - CANNOT HANDLE POINT OF INFLECTION
The edge of an out-of-range area has a point of inflection (an S-shape).  The
algorithm that traces the edge will not handle such a point.
.sp
CPTREV - ALGORITHM FAILURE - CPMPXY ILL-DEFINED?
The algorithm that is used to trace the edges of out-of-range areas has
failed.  The failure is probably caused by an error in a user-supplied
version of the
mapping subroutine CPMPXY.  It should be the case that, for a given set of
inputs, CPMPXY produces consistent outputs.  It should also be the case
that the edges of out-of-range areas should be smooth curves with no sharp
bends.
