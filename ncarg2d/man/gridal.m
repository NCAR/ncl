.\"
.\"	$Id: gridal.m,v 1.1.1.1 1992-04-17 22:30:33 ncargd Exp $
.\"
.TH GRIDAL 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " GRIDAL - Draws background grids
.dsS1 " CALL GACOLR (KAXS,KLBL,KMJT,KMNT) sets color of background parts
.dsS2 " CALL GRID (MJRX,MNRX,MJRY,MNRY) Draws unlabled grid
.dsS3 " CALL GRIDL (MJRX,MNRX,MJRY,MNRY) Draws labled grid
.dsS4 " CALL HALFAX (MJRX,MNRX,MJRY,MNRY,XINT,YINT,IXLB,IYLB) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \ Draws a pair of intersecting axes
.dsS5 " CALL LABMOD (FMTX,FMTY,NUMX,NUMY,ISZX,ISZY,IXDC,IYDC,IXOR) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \ changes appearance of labels
.dsS6 " CALL PERIM (MJRX,MNRX,MJRY,MNRY) Draws unlabled perimeter
.dsS7 " CALL PERIML (MJRX,MNRX,MJRY,MNRY) Labled perimeter
.dsS8 " CALL TICKS (LMJR,LMNR) superseded by TICK4, use-
.dsS9 " CALL TICK4 (LMJX,LMNX,LMJY,LMNY) control tick mark length and direction
.nrsN 9
.tr ~
.pn 305
.bp
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9GRIDAL\s11@"
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
.S 11
.nf
.ha
.na
SUBROUTINE GRIDAL (MAJRX,MINRX,MAJRY,MINRY,IXLAB,IYLAB,IGPH,X,Y) 
.ad
.hy
.fi
.R
.H 3 "\fBLatest Revision\fR" 
August 1987
.H 3 "\fBPurpose\fR"
This package allows one to draw backgrounds for X/Y plots.
Included are routines for
drawing grids, perimeters, and pairs of axes.
.H 3 "\fBUsage\fR"
.hw GACOLR, GRIDAL, HALFAX, LABMOD, PERIM, PERIML
Each user entry point in this package (GACOLR, GRID,
.hw GRIDAL
GRIDAL, GRIDL, HALFAX, LABMOD, PERIM, PERIML, TICKS, 
and TICK4) is described below.  Here, we discuss features of 
the package as a whole.
.sp
Each of the routines GRID, GRIDL, HALFAX,
PERIM, PERIML, and GRIDAL draws a background
of some sort within the current GKS viewport,
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
.nr 80 0
.nr 38 \wGRID
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGRIDL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wHALFAX
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wPERIM
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wPERIML
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGRIDAL
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wdraws an unlabeled grid
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdraws a labeled grid
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdraws a pair of intersecting axes
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdraws an unlabeled perimeter
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdraws a labeled perimeter
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdraws any of the above
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
\&\h'|\n(40u'GRID\h'|\n(41u'draws an unlabeled grid
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'GRIDL\h'|\n(41u'draws a labeled grid
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'HALFAX\h'|\n(41u'draws a pair of intersecting axes
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'PERIM\h'|\n(41u'draws an unlabeled perimeter
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'PERIML\h'|\n(41u'draws a labeled perimeter
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'GRIDAL\h'|\n(41u'draws any of the above
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-9
.sp
Ticks, grid lines, and numeric labels are
positioned as determined by the current GKS
window and by the values of the two flags 'LS'
and 'MI', in the SPPS package.  (The SPPS
routine SET may be called to set the GKS
viewport and window and the flags 'LS' and
\&'MI'; the flag 'LS' determines whether the
mappings of user "world" coordinates into the
viewport are linear or logarithmic and the
flag 'MI' whether the mappings are in the
normal direction or are mirror-imaged.)
.sp
The routines GACOLR, LABMOD, TICKS, and TICK4
do no drawing.  Each is called to preset
parameters which affect the behavior of a
subsequent call to one of the background-drawing 
routines, as follows:
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
.nr 38 \wGACOLR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLABMOD
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wTICKS and TICK4
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wsets the color of background parts
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wchanges the appearance of labels
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wchange the length and direction of ticks
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
\&\h'|\n(40u'GACOLR\h'|\n(41u'sets the color of background parts
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LABMOD\h'|\n(41u'changes the appearance of labels
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'TICKS and TICK4\h'|\n(41u'change the length and direction of ticks
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.sp
.H 3 "\fBArguments\fR"
Several of the routines have arguments MJRX, MNRX,
MJRY, and MNRY, which specify the number
of major and minor divisions of the horizontal
(X) and vertical (Y) axes of the current
viewport.  These parameters have different
meanings, depending on the current setting
of the linear/log flag 'LS' in SPPS.
.sp
If the axis is linear:  MJRX(Y) specifies the
number of major divisions of the X(Y) axis and
MNRX(Y) specifies the number of minor divisions
within each major division.  In each case,
the value specifies the number of spaces
between grid lines or ticks rather than the
number of lines or 
ticks.  There is always
one more major division line or mark than the
number of major divisions specified by MJRX(Y).
Similarly, there is always one less minor
division line or tick per major division
than the number of minor divisions per major
division specified by MNRX(Y).
.sp
If the axis is logarithmic:  Each major
division point occurs at a value 10**MJRX(Y)
times the previous point.  For example, if
the minimum X-axis value were 3., the maximum
X-axis value 3000. and MJRX 1, then the major
division points would be 3., 30., 300., and
3000.  If MNRX(Y).LE.10, there are nine minor
divisions within each major division.  For
example, between 3. and 30., there would be
minor division points at 6., 9., 12., . . . 27.
If MNRX(Y).GT.10., minor divisions are omitted.
.H 3 "\fBEntry Points\fR"
GALBEX, GRID, GRIDAL, GRIDL, HALFAX, LABMOD, PERIM, PERIML, TICKS, TICK4
.H 3 "\fBCommon Blocks\fR"
GAREIN and GACHAR.
.H 3 "\fBRequired Routines\fR"
SETER and the SPPS.
.H 3 "\fBRequired GKS Level\fR"
0A
.H 3 "\fBI/O\fR"
Plots backgrounds.
.H 3 "\fBPrecision\fR"
Single.
.H 3 "\fBLanguage\fR"
FORTRAN 77.
.H 3 "\fBHistory\fR" 
These routines have been a part of the NCAR
System Plot Package for many years.  As part
of the GKS effort, they were incorporated into
a separate package.  The code here represents
a total rewrite in October 1986.
.ti -1.5i
.hD "SUBROUTINE GACOLR"
.H 3 "\fBPurpose\fR"
To set the color of various parts of the background.
.H 3 "\fBUsage\fR"
CALL GACOLR (KAXS,KLBL,KMJT,KMNT)
.H 3 "\fBArguments\fR"
KAXS, KLBL, KMJT, and KMNT are the color indices of the desired 
colors of the axes,
the labels, the major ticks/grid lines, and
the minor ticks/grid lines, respectively.
Values less than or equal to zero imply that
no call is to be done to set the color before
drawing items of the associated type.  The
default value of each of the four parameters
is zero.
.ti -1.5i
.hD "SUBROUTINE GRID"
.H 3 "\fBPurpose\fR"
To draw an unlabeled grid.
.H 3 "\fBUsage\fR"
CALL GRID (MJRX,MNRX,MJRY,MNRY)
.H 3 "\fBArguments\fR"
See the package description, above.
.ti -1.5i
.hD "SUBROUTINE GRIDAL"
.H 3 "\fBPurpose\fR"
A general entry point for all supported types
of backgrounds.  Each of the other background-drawing routines is implemented by a call to
GRIDAL.
.H 3 "\fBUsage\fR"
.nf
.na
CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,IXLB,IYLB,
~~~~~~~~~~~~~IGPH,XINT,YINT)
.ad
.fi
.H 3 "\fBArguments\fR"
MJRX, MNRX, MJRY, and MNRY specify the major
and minor divisions of the two axes, as
described in the package description, above.
.sp
IXLB and IYLB are integer flags, defined as follows:
.sp
.nf
.mk
IXLB = -1  No X axis drawn.
~~~~~~~~~~~~~~~~No X-axis labels.
.sp
~~~~~~~~=  0  X axis drawn.
~~~~~~~~~~~~~~~~No X-axis labels.
.sp
~~~~~~~~=  1  X axis drawn.
~~~~~~~~~~~~~~~~X-axis labels.
.rt
.in +2.5i
IYLB = -1  No Y axis drawn.
~~~~~~~~~~~~~~~~No Y-axis labels.
.sp
~~~~~~~~=  0  Y axis drawn.
~~~~~~~~~~~~~~~~No Y-axis labels.
.sp
~~~~~~~~=  1  Y axis drawn.
~~~~~~~~~~~~~~~~Y-axis labels.
.in -2.5i
.fi
.sp
IGPH specifies the background type:
.>>
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
.nr 38 \w\fBIGPH\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w4
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w5
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w6
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w8
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w9
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w10
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fBX axis\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wGRID
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wGRID
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wGRID
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPERIM
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPERIM
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPERIM
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHALFAX
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHALFAX
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wHALFAX
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.82
.rm 82
.nr 83 0
.83
.rm 83
.nr 84 0
.84
.rm 84
.nr 38 \w\fBY axis\fR-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wGRID-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wPERIM-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wHALFAX-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wGRID-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wPERIM-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wHALFAX-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wGRID-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wPERIM-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wHALFAX-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
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
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBIGPH\fR\h'|\n(41u'\fBX axis\fR\h'|\n(42u'\fBY axis\fR
.sp
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'GRID\h'|\n(42u'GRID
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'GRID\h'|\n(42u'PERIM
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2\h'|\n(41u'GRID\h'|\n(42u'HALFAX
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'4\h'|\n(41u'PERIM\h'|\n(42u'GRID
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'5\h'|\n(41u'PERIM\h'|\n(42u'PERIM
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'6\h'|\n(41u'PERIM\h'|\n(42u'HALFAX
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'8\h'|\n(41u'HALFAX\h'|\n(42u'GRID
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'9\h'|\n(41u'HALFAX\h'|\n(42u'PERIM
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'10\h'|\n(41u'HALFAX\h'|\n(42u'HALFAX
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-15
.<<
.sp
XINT and YINT are the user "world" coordinates
of the point of intersection of the two axes
if IGPH equals 10.  For other values of
IGPH for which one of the axes is of type
HALFAX, XINT and/or YINT specify the position
of that axis.
.ti -1.5i
.hD "SUBROUTINE GRIDL"
.H 3 "\fBPurpose\fR"
To draw a labeled grid.  Each major division
is labeled with its numeric value.
.H 3 "\fBUsage\fR"
CALL GRIDL (MJRX,MNRX,MJRY,MNRY)
.H 3 "\fBArguments\fR"
See the package description, above.
.ti -1.5i
.hD "SUBROUTINE HALFAX"
.H 3 "\fBPurpose\fR"
To draw orthogonal axes intersecting at a
specified point and with a specified set of
labels.
.H 3 "\fBUsage\fR"
.nf
.na
~~CALL HALFAX (MJRX,MNRX,MJRY,MNRY,XINT,YINT,
+~~~~~~~~~~~~~IXLB,IYLB)
.ad
.fi
.H 3 "\fBArguments\fR"
All arguments are as defined for GRIDAL, above.
In fact, the above call is equivalent to
.sp
.nf
.na
~~CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,IXLB,IYLB,
+~~~~~~~~~~~~10,XINT,YINT)
.ad
.fi
.ti -1.5i
.hD "SUBROUTINE LABMOD"
.H 3 "\fBPurpose\fR"
To preset parameters controlling the appearance
of labels drawn by GRIDAL, GRIDL, . . . et al.
LABMOD itself does no plotting and, in order
to have any effect, must be called prior to
the background-drawing routines for which it
is presetting parameters.
.H 3 "\fBUsage\fR"
.nf
.na
~~CALL LABMOD (FMTX,FMTY,NUMX,NUMY,ISZX,ISZY,
+~~~~~~~~~~~~~IXDC,IYDC,IXOR)
.ad
.fi
.H 3 "\fBArguments\fR"
FMTX and FMTY are of type CHARACTER and contain
format specifications for the X-axis and Y-axis
numerical labels produced by GRIDAL, GRIDL,
HALFAX, or PERIML.  The specification must
begin with a left parenthesis and end with a
right parenthesis and must not be more than
ten characters long.  Conversions of types
E, F, G, and I are allowed; for example, one
might use FMTX='(F8.2)' and FMTY='(E10.0)'.
The default for both formats is '(E10.3)'.
.sp
Note:  I formats are allowed by this version
of GRIDAL; they were not allowed by previous
versions.
.sp
NUMX, if non-zero, is the number of characters
in each X-axis numeric label; if LBLX is a
string produced by the format FMTX, then the
label will be the substring LBLX(1:NUMX).  If
NUMX is 0, then the label will be the substring
LBLX(m:n), where LBLX(m:m) is the first non-blank character in LBLX, and LBLX(n:n) is the
last non-blank character following LBLX(m:m).
Using a non-zero NUMX causes the labels to be
centered differently than if a zero value is
used.  NUMY is defined similarly and applies
to Y-axis labels.  The default value for both
parameters is 0.

ISZX and ISZY are character sizes for the
labels, specified in plotter address units,
just as for the SPPS routines PWRIT and WTSTR.
The default value for both is 10.

IXDC is the decrement, in plotter address
units, from the left edge of the current
viewport to the nearest X address of the
label specified by FMTY, NUMY, and ISZY.
For example, if the horizontal extent of the
current viewport is defined by the normalized
device coordinates .1 and .9, and if IXDC is
60, and if there has been no call to the SPPS
routine SETI, then labels on the Y axis will
end at plotter coordinate 43 (.1*1023+1-60).
Negative values may be used to put labels
on the other side of the viewport; in the
.hw (-.8*1023-60)
example given, changing IXDC to -878
(-.8*1023-60)
would put the labels on the right side
of the viewport, with their left edges 60
plotter-coordinate units away from the edge
of the viewport.  There are two special values
of IXDC:
.VL 1c
.LI "~~"
If IXDC=0, the Y-axis labels will end 20
plotter address units to the left of the
viewport (equivalent to using IXDC=20).
.sp
If IXDC=1, Y-axis labels will begin 20
plotter address units to the right of the
viewport (equivalent to using IXDC=-20-w,
where w is the width of the viewport, in
plotter address units).
.LE
.sp
The default value of the X decrement is 20.
.sp
When HALFAX is called or when GRIDAL is called
with 
IGPH~=~2,~6,~or~10, IXDC is the distance
from the Y axis, rather than from the minimum
viewport coordinate, and the special values 0
and 1 are equivalent to 20 and -20.
.sp
IYDC is the decrement, in plotter address
units, from the bottom edge of the current
viewport to the nearest Y address of the
label specified by FMTX, NUMX, and ISZX.
Note that negative values may be used to put
labels above the viewport.  There are two
special values of IYDC:
.VL 1c
.LI "~~"
If IYDC=0, the top of the X-axis labels
will be 20 plotter address units below the
bottom edge of the viewport (equivalent to
using IYDC=20).
.sp
If IYDC=1, the bottom of the X-axis labels
will be 20 plotter address units above the
top edge of the viewport (equivalent to using
IYDC=-20-h, where h is the height of the
viewport, in plotter address units).
.sp
.LE
The default value of the Y decrement is 20.
.bp
When HALFAX is called or when GRIDAL is called
with 
IGPH~=~8,~9,~or~10, IYDC is the distance
from the X axis, rather than from the minimum
viewport coordinate, and the special values 0
and 1 are equivalent to 20 and -20.

IXOR specifies the orientation of the X-axis
labels:

.nf
.na
IXOR~=~0~~~horizontal
~~~~~~~~~=~1~~~vertical
.ad
.fi

The default orientation is horizontal.
.ti -1.5i
.hD "SUBROUTINE PERIM"
.H 3 "\fBPurpose\fR"
To draw an unlabeled perimeter with inward-pointing tick marks.  The directions and
lengths of tick marks may be changed by
calling TICKS and/or TICK4 (see below).
.H 3 "\fBUsage\fR"
CALL PERIM (MJRX,MNRX,MJRY,MNRY)
.H 3 "\fBArguments\fR"
See the package description, above.
.ti -1.5i
.hD "SUBROUTINE PERIML"
.H 3 "\fBPurpose\fR"
To draw a labeled perimeter with inward-pointing tick marks.  The directions and
lengths of tick marks may be changed by
calling TICKS and/or TICK4 (see below).
.H 3 "\fBUsage\fR"
CALL PERIML (MJRX,MNRX,MJRY,MNRY)
.H 3 "\fBArguments\fR"
See the package description, above.
.ti -1.5i
.hD "SUBROUTINE TICKS"
.H 3 "\fBPurpose\fR"
To allow program control of tick mark length
and direction.  This routine has been
superseded by TICK4, which should be used
instead.
.H 3 "\fBUsage\fR"
CALL TICKS (LMJR,LMNR)
.H 3 "\fBArguments\fR"
The above call is equivalent to

CALL TICK4 (LMJR,LMNR,LMJR,LMNR)

See the description of TICK4, below.
.ti -1.5i
.hD "SUBROUTINE TICK4"
.H 3 "\fBPurpose\fR"
To allow program control of tick mark length
and direction.
.H 3 "\fBUsage\fR"
CALL TICK4 (LMJX,LMNX,LMJY,LMNY)
.H 3 "\fBArguments\fR"
LMJX and LMNX are the lengths, in plotter
address units, of major and minor ticks on
the X axis.  The default values are 12 and 8.

LMJY and LMNY are the lengths, in plotter
address units, of major and minor ticks on
the Y axis.  The default values are 12 and 8.

By default, tick marks point inward.  Negative
values of LMJX, LMNX, LMJY, and LMNY may be
used to create outward-pointing tick marks.
