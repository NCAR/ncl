.\"
.\"	$Id: dashsupr.m,v 1.1.1.1 1992-04-17 22:30:21 ncargd Exp $
.\"
.TH DASHSUPR 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsS1 " CALL DASHDC (IPAT,JCRT,JSIZE)
.dsS2 " CALL DASHDB (IPAT)
.dsS3 " CALL CURVED (X,Y,N)
.dsS4 " CALL FRSTD (X,Y)
.dsS5 " CALL VECTD (X,Y)
.dsS6 " CALL LINED (XA,XB,YA,YB) no smoothing
.dsS7 " CALL LASTD Called after last point is processed
.nrsN 7
.dsS8 " CALL PWRTM (X,Y,IDPC,ISIZ,IOR,ICNT) Draw characters
.nrsN 8
./" USE tsi to PRINT THIS FILE!
.pn 195
.bp
.PH ""
.PF ""
.SK 1
.tr ~
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9DASHSUPR\s11@"
.EF "@\s\fB11%\s9\fR@@August 1987\s11@"
.OF "@\s9August 1987@@\s11\fB%\fR@"
.de hD          \" Heading macro level one
.br
.ne 5
.sp 2
.ps +3
.ft B           \" boldface, 14 pt.
\\$1
.ft R
.ps -3
.sp 
..
.de >>          \" display for indented lines
.in +.25i       \" usage: .>>
.sp
.nf
..              
.de <<          \" end of display for indented lines
.fi
.in -.25i       \" usage: .<<
.sp
..              
.de sf          \"start fortran (constant spacing)
.ps 10
.vs 12
.nf
.ft L
..
.de ef          \"end fortran (resume variable spacing & prev. size & font)
.ft
.fi
.ps
.vs
..
.br
.S 14
.B
DASHSUPR
.br
.in +0.7i
Software dashed-line package with character capability, smoothing,
and the capability of removing crowded lines.
.S 11
.sp
.R
.H 3 "Latest Revision"
August 1987 
.H 3 "Purpose"
DASHSUPR is a software dashed-line package with
smoothing capabilities and the capability of
removing crowded lines.
.H 3 "Usage"
First, do
.br
CALL RESET

Then, either
.br
CALL DASHDB (IPAT)
.br
where IPAT is a 16-bit dash pattern as
described in the subroutine DASHDB (see
DASHLINE documentation), or
.br
CALL DASHDC (IPAT,JCRT,JSIZE)
.br
as described below.

Then, call any of the following:
.nf
CALL CURVED (X,Y,N)
CALL FRSTD (X,Y)
CALL VECTD (X,Y)
CALL LASTD
.fi
.sp
LASTD is called only after the last
point of a line has been processed in VECTD.

The following may also be called, but no
smoothing will result:
.br
CALL LINED (XA,YA,XB,YB)

PWRTM can be called to draw characters and
mark, in the model picture, the regions where
the characters have been drawn.
.br
CALL PWRTM (X,Y,IDPC,ISIZ,IOR,ICNT)
.sp
For details, see the subroutine PWRTM in
this package.
.H 3 "ARGUMENTS"
.H 3 "On Input to DASHDC"
.VL .6i
.LI "\fBIPAT\fR"
A character string of arbitrary length
(60 characters seems to be a practical
limit) that specifies the dash pattern
to be used.  A dollar sign in IPAT
indicates solid; an apostrophe indicates
a gap; blanks are ignored.  Any character
in IPAT that is not a dollar sign,
apostrophe, or blank is considered to be
part of a line label.  Each line label
can be at most 15 characters in length.
Sufficient white space is reserved in the
dashed line for writing line labels.
.LI "\fBJCRT\fR"
The length in plotter address units per
$ or apostrophe.
.LI "\fBJSIZE\fR"
Is the size of the plotted characters:
.BL
.LI
If between 0 and 3, it is 1., 1.5, 2. and 3. 
times an 8 plotter address unit width.
.LI
If greater than 3, it is the character
width in plotter address units.
.LE
.LE
.H 3 "To Other Line-Drawing Routines"
.VL .6i
.LI "\fBCURVED(X,Y,N)\fR"
X and Y are arrays of world coordinate values
of length N or greater.  Line segments obeying
the specified dash pattern are drawn to
connect the N points.
.LI "\fBFRSTD(X,Y)\fR"
The current pen position is set to
the world coordinate value (X,Y).
.LI "\fBVECTD(X,Y)\fR"
A line segment is drawn between the
world coordinate value (X,Y) and the
most recent pen position.  (X,Y) then
becomes the most recent pen position.
.LI "\fBLINED(XA,XB,YA,YB)\fR"
A line is drawn between world coordinate
values (XA,YA) and (XB,YB).
.LI "\fBLASTD\fR"
When using FRSTD and VECTD, LASTD must be
called (no arguments needed).  LASTD sets up
the calls to the smoothing routines KURV1S and
KURV2S.
.LI "\fBPWRTM (X,Y,IDPC,ISIZ,IOR,ICNT)\fR"
The arguments for PWRTM are the same as those
for the utility support routine WTSTR.
.LE
.H 3 "On Output"
All arguments are unchanged for all routines.
.H 3 "Note"
When switching from the regular plotting
routines to a dashed-line package the first
call should not be to VECTD.
.H 3 "Entry Points"
DASHDB, DASHDC, CURVED, FRSTD, VECTD, LINED,
RESET, LASTD, KURV1S, KURV2S, CFVLD, FDVDLD,
DRAWPV, DASHBD, CUTUP, REMOVE, MARKL, PWRTM.
.H 3 "Common Blocks"
INTPR, DASHD1, DASHD2, DDFLAG, DCFLAG, DSAVE1,
DSAVE2, DSAVE3, DSAVE4, DSAVE5, CFFLAG, SMFLAG,
DFFLAG. 
.H 3 "Required Library Routines"
The ERPRT77 package and the SPPS.
.H 3 "Required GKS Level"
0A
.H 3 "I/O"
Plots solid or dashed lines, possibly with
characters at intervals in the line.
The lines may also be smoothed.
.H 3 "Precision"
Single
.H 3 "Language"
FORTRAN 77
.H 3 "History"
Originally written by Tom Reid at Texas A & M.
Made portable in 1977 for all machines that
support plotters with up to 15 bit resolution.
Converted to FORTRAN 77 and GKS in June 1984.
.H 3 "Algorithm"
Points for each line
segment are processed and passed to the
routines, KURV1S and KURV2S, which compute
splines under tension passing through these
points.  New points are generated between the
given points, resulting in smooth lines.
As each line is drawn, a test is done to see if
that part of the plotting plane has been drawn
on.  If it has, then that line, or part of that
line, is not drawn.  As the lines are drawn,
they are also marked into the model picture.

The model picture is a bit map, 1024 X 1024,
which marks each pixel as it is used.

Note:  The user should draw all lines of major
importance first and then lines
of minor importance since the algorithm
removes the last line drawn if line
removal is necessary.
.H 3 "Accuracy"
Plus or minus .5 plotter address units per call.
There is no cumulative error.
.H 3 "Timing"
About four times as long as DASHCHAR.
.H 3 "Portability"
FORTRAN 77
.H 3 "Internal Parameters"
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
.am 82
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Number of plotter addresses per
element in the dash pattern for
solid lines and gaps. The
pattern is repeated every
IPAU*16 plotter address units.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 82
.br
.di b+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Multiplicative factor for
first solid-line segment.
This can be used to off-set
labels.  For example, if
FPART = .5, the first solid
line segment is only
one-half as long as the other
solid-line segments.  This
moves all labels on this
line towards the beginning,
which reduces the
probability of the label
being written on top of a
label of a nearby line
drawn with FPART = 1.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 82
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Flag to control whether a gap
is left for characters when
plotting.
.br
= 9  Gap is left.
.br
= 0  No gap is left.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 82
.br
.di d+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Flag to turn on smoothing code.
.br
= 0  Smoothing.
.br
= 1  No smoothing.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.eo
.am 82
.br
.di e+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Tension factor.  Must be
greater than 0.  A large
tension factor (30.) would
essentially turn off smoothing.
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.eo
.am 82
.br
.di f+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Twice the maximum number of
interpolated points on a
horizontal line with length
equal to that of the grid.
More points per unit length are
interpolated for short lines
than for long lines.
.br
.di
.nr f| \n(dn
.nr f- \n(dl
..
.ec \
.eo
.am 82
.br
.di g+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Minimum distance in metacode
address units between points
that are saved.  When the
points on a line are being
processed, only the first of two
consecutive points is saved if
the points are less than SMALL
metacode address units apart.
This procedure is to prevent
cusps.
.br
.di
.nr g| \n(dn
.nr g- \n(dl
..
.ec \
.eo
.am 82
.br
.di h+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The maximum number of points
saved at one time.  If there
are more than L1 points on a
given line, L1 points are
processed, then the next L1,
until the entire line is
processed.  Smoothness between
segments is maintained
automatically.  If L1 is
increased, the dimensions of
XSAVE, YSAVE, XP, YP, and TEMP
in FDVDLD must be increased to
the new value of L1.
.br
.di
.nr h| \n(dn
.nr h- \n(dl
..
.ec \
.eo
.am 82
.br
.di i+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Number of plotter address units
added to each character string,
on the left and the right, as
free space.
.br
.di
.nr i| \n(dn
.nr i- \n(dl
..
.ec \
.eo
.am 82
.br
.di j+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
Number of plotter address units
added to each character string,
on the top and on the bottom, as
free space.
.br
.di
.nr j| \n(dn
.nr j- \n(dl
..
.ec \
.eo
.am 82
.br
.di k+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
The maximum length in each
coordinate of a single line to
be processed by the crowded
line algorithm.  Lines longer
than MLLINE metacode address
units in a coordinate are broken
into smaller segments, each of
which is processed separately.
This is done to prevent
anomalies in the removal of
long lines since only the
starting point and end point of
each line is checked in that
process.
.br
.di
.nr k| \n(dn
.nr k- \n(dl
..
.ec \
.eo
.am 82
.br
.di l+
.35
.ft \n(.f
.ll \n(34u*3u/6u
.if \n(.l<\n(82 .ll \n(82u
.in 0
An internal or external call to
set the pen (pen-up) to a
specific position is executed
only if this position is
more than ICLOSE metacode
address units away from the
current pen position (distance=difference 
in X-coordinates + difference in Y-coordinates).
.br
.di
.nr l| \n(dn
.nr l- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \wName
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIPAU
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wFPART
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIGP
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wName
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIOFFS
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wTENSN
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNP
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSMALL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wL1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wADDLR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wADDTB
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wMLLINE
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wName
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wICLOSE
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wDefault
.if \n(81<\n(38 .nr 81 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w3
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w9
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \wDefault
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.5
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w150
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w128
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w70
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w2
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w384
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \wDefault
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w6
.if \n(31<\n(38 .nr 31 \n(38
.81
.rm 81
.nr 61 \n(31
.nr 38 \n(61+\n(32
.if \n(38>\n(81 .nr 81 \n(38
.if \n(38<\n(81 .nr 61 +(\n(81-\n(38)/2
.nr 82 0
.82
.rm 82
.nr 83 0
.83
.rm 83
.nr 84 0
.84
.rm 84
.nr 38 \wFunction -\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wFunction -\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wFunction -\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 61 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr 43 \n(82+(3*\n(38)
.nr 83 +\n(43
.nr 44 \n(83+(3*\n(38)
.nr 84 +\n(44
.nr TW \n(84
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
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Name\h'|\n(41u'Default\h'|\n(42u'Function 
.R
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IPAU\h'|\n(41u'3\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'FPART\h'|\n(41u'1.\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IGP\h'|\n(41u'9\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.bp
.T&
.B
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Name\h'|\n(41u'Default\h'|\n(42u'Function 
.R
.sp
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IOFFS\h'|\n(41u'0\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.d+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(e|u+\n(.Vu
.if (\n(e|+\n(#^-1v)>\n(#- .nr #- +(\n(e|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'TENSN\h'|\n(41u'2.5\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.e+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(f|u+\n(.Vu
.if (\n(f|+\n(#^-1v)>\n(#- .nr #- +(\n(f|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NP\h'|\n(41u'150\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.f+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(g|u+\n(.Vu
.if (\n(g|+\n(#^-1v)>\n(#- .nr #- +(\n(g|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SMALL\h'|\n(41u'128.\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.g+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(h|u+\n(.Vu
.if (\n(h|+\n(#^-1v)>\n(#- .nr #- +(\n(h|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'L1\h'|\n(41u'70\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.h+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(i|u+\n(.Vu
.if (\n(i|+\n(#^-1v)>\n(#- .nr #- +(\n(i|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ADDLR\h'|\n(41u'2.\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.i+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(j|u+\n(.Vu
.if (\n(j|+\n(#^-1v)>\n(#- .nr #- +(\n(j|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ADDTB\h'|\n(41u'2.\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.j+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(k|u+\n(.Vu
.if (\n(k|+\n(#^-1v)>\n(#- .nr #- +(\n(k|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'MLLINE\h'|\n(41u'384\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.k+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.bp
.T&
.B
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Name\h'|\n(41u'Default\h'|\n(42u'Function 
.R
.sp
.ne \n(l|u+\n(.Vu
.if (\n(l|+\n(#^-1v)>\n(#- .nr #- +(\n(l|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ICLOSE\h'|\n(41u'6\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.l+
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
.rm i+
.rm j+
.rm k+
.rm l+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-153
.bp
.PH ""
.PF ""
.bp
