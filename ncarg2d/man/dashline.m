.\"
.\"	$Id: dashline.m,v 1.1.1.1 1992-04-17 22:30:21 ncargd Exp $
.\"
.TH DASHLINE 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsS1 " CALL DASHDC (IPAT,JCRT,JSIZE)
.dsS2 " CALL DASHDB (IPAT)
.dsS3 " CALL CURVED (X,Y,N)
.dsS4 " CALL FRSTD (X,Y)
.dsS5 " CALL VECTD (X,Y)
.dsS6 " CALL LINED (XA,XB,YA,YB) no smoothing
.dsS7 " CALL LASTD Called after last point is processed
.nrsN 7
./" USE tsi to PRINT THIS FILE!
.pn 185
.bp
.PH ""
.PF ""
.SK 1
.tr ~
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9DASHLINE\s11@"
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
DASHLINE  - Software dashed-line package
.S 11
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
DASHLINE is a software dashed-line package.
Some hardware dashed line generators
fail to produce pleasing results when
drawing very short vector segments or
vector segments of varying lengths.  This
package does not have this problem.
.H 3 "Usage"
First,
.br
CALL DASHDB (IPAT)

Then, call any of the following:
.br
.nf
CALL CURVED (X,Y,N)
CALL FRSTD (X,Y)
CALL VECTD (X,Y)
CALL LINED (XA,YA,XB,YB)
.fi
.ti -1.5i
.hD "SUBROUTINE DASHDB (IPAT)"
.H 3 "ARGUMENTS"
.H 3 "On Input"
.VL 2i
.LI "\fBDASHDB (IPAT)\fR"
IPAT is a 16-bit dash pattern.  By default
each bit in the pattern represents 3 plotter
address units (1=solid, 0=blank)
.LE
.H 3 "To Other Line-Drawing Routines"
.VL 2i
.LI "\fBCURVED(X,Y,N)\fR"
X and Y are arrays of world coordinate values
of length N or greater.  Line segments obeying
the specified dash pattern are drawn to
connect the N points.
.LI "\fBFRSTD(X,Y)\fR"
The current pen position is set to
the world coordinate value (X,Y)
.LI "\fBVECTD(X,Y)\fR"
A line segment is drawn between the
world coordinate value (X,Y) and the
most recent pen position.  (X,Y) then
becomes the most recent pen position.
.LI "\fBLINED(XA,XB,YA,YB)\fR"
A line is drawn between world coordinate
values (XA,YA) and (XB,YB).
.LE
.H 3 "On Output"
All arguments are unchanged for all routines.
.H 3 "Entry Points"
DASHDB, CURVED, FRSTD, VECTD, LINED,
LASTD, CFVLD, DRAWPV, DASHBD
.H 3 "Common Blocks"
INTPR, DSHD, DSHDA, DSHDD, DSHDC, DSHDB
.H 3 "Required Library Routines"
The ERPRT77 package and the SPPS.
.H 3 "Required GKS Level"
0A
.H 3 "Precision"
Single
.H 3 "Language"
FORTRAN 77
.H 3 "History"
Written in 1969, standardized in November 1972.
Made portable in September 1977 for use
with all computer systems having at least
a 16-bit word.
Converted to FORTRAN 77 and GKS in April 1984.
.H 3 "Algorithm"
The position in the dash pattern is
remembered as points are processed.  The
distance transversed in plotter address space
is used to determine whether to draw segments,
parts of segments, characters, or nothing.
The plotter address space is 1024 X 1024.
.H 3 "Accuracy"
Plus or minus .5 plotter address units per call.
There is no cumulative error.
.H 3 "Timing"
For solid or blank lines, there is almost no
overhead.  Dashed lines take about 4 times as
long as drawing solid lines.  (The line
drawing software is so fast that the increase
will not be noticed in most programs.)
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
An internal or external call to
set the pen (pen-up) to a
specific position is executed
only if this position is
more than ICLOSE metacode
address units away from the
current pen position (distance=
difference in X-coordinates +
difference in Y-coordinates).
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
.nr 38 \wName
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIPAU
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
.nr 38 \wFunction-\n(82-3n-\n(83-3n-\n(84
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
\&\h'|\n(40u'Name\h'|\n(41u'Default\h'|\n(42u'Function
.R
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IPAU\h'|\n(41u'3\h'|\n(42u'
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
.ta \n(80u \n(61u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ICLOSE\h'|\n(41u'6\h'|\n(42u'
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
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-27
