.\"
.\"	$Id: hafton.m,v 1.1.1.1 1992-04-17 22:30:29 ncargd Exp $
.\"
.TH HAFTON 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " HAFTON - Halftone pictures of 2-d arrays
.dsS1 " CALL EZHFTN (Z,M,N) if criteria below is met, else
.nrsN 1
.\" print with tsi
.tr ~
.pn 315
.bp
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9HAFTON\s11@"
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
.nh
.na
SUBROUTINE HAFTON (Z,L,M,N,FLO,HI,NLEV,NOPT,NPRM,ISPV,SPVAL)
.ad
.hy
.fi
.R
.H 3 "\fBDimension of Arguments\fR" 
Z(L,M)
.H 3 "\fBLatest Revision\fR"
August 1987
.H 3 "\fBPurpose\fR"
HAFTON draws a halftone picture from data
stored in a rectangular array with the
intensity in the picture proportional to
the data value.
.H 3 "\fBUsage\fR"
If the following assumptions are met, use
.sp
CALL EZHFTN (Z,M,N)
.sp
Assumptions:
.BL
.LI
All of the array is to be drawn.
.LI
Lowest value in Z will be at lowest
intensity on reader/printer output.
.LI
Highest value in Z will be at
highest intensity.
.LI
Values in between will appear
linearly spaced.
.LI
Maximum possible number of
intensities are used.
.LI
The picture will have a perimeter
drawn.
.LI
FRAME will be called after the
picture is drawn.
.LI
Z is filled with numbers that should
be used (no missing values).
.LE
.sp
If these assumptions are not met, use
.sp
.nf
CALL HAFTON (Z,L,M,N,FLO,HI,NLEV,NOPT,NPRM,ISPV,SPVAL)
.fi
.H 3 "\fBARGUMENTS\fR"
.H 3 "\fBOn Input for EZHFTN\fR"
.VL .6i
.LI "\fBZ\fR"
M by N array to be used to generate a
halftone plot.
.LI "\fBM\fR"
First dimension of Z.
.LI "\fBN\fR"
Second dimension of Z.
.H 3 "\fBOn Output for EZHFTN\fR"
All arguments are unchanged.
.H 3 "\fBOn Input for HAFTON\fR"
.VL .6i
.LI "\fBZ\fR"
The origin of the array to be plotted.
.LI "\fBL\fR"
The first dimension of Z in the calling
program.
.LI "\fBM\fR"
The number of data values to be plotted
in the x-direction (the first subscript
direction).  When plotting all of an
array, L = M.
.LI "\fBN\fR"
The number of data values to be plotted
in the y-direction (the second subscript
direction).
.LI "\fBFLO\fR"
The value of Z that corresponds to the
lowest intensity.  (When NOPT.LT.0, FLO
corresponds to the highest intensity.)
If FLO=HI=0.0, MIN(Z) will be used for FLO.
.LI "\fBHI\fR"
The value of Z that corresponds to the
highest intensity.  (When NOPT.LT.0, HI
corresponds to the lowest intensity.)  If
HI=FLO=0.0, MAX(Z) will be used for HI.
.LI "\fBNLEV\fR"
The number of intensity levels desired.
16 maximum.  If NLEV = 0 or 1, 16 levels
are used.
.LI "\fBNOPT\fR"
Flag to control the mapping of Z onto the
intensities.  The sign of NOPT controls
the directness or inverseness of the
mapping.
.BL
.LI
NOPT
positive yields direct mapping.
The largest value of Z produces the
most dense dots.  On mechanical plotters,
large values of Z will produce a dark
area on the paper.  With the film
development methods used at NCAR,
large values of Z will produce many
(white) dots on the film, also
resulting in a dark area on
reader-printer paper.
.LI
NOPT
negative yields inverse mapping.
The smallest values of Z produce the
most dense dots resulting in dark
areas on the paper.
.LE
.sp
The absolute value of NOPT determines the
mapping of Z onto the intensities.  For
IABS(NOPT)
.VL 1c
.LI "=~0"
The mapping is linear.  For
each intensity there is an equal
range in Z value.
.LI "=~1"
The mapping is linear.  For
each intensity there is an equal
range in Z value.
.LI "=~2"
The mapping is exponential.  For
larger values of Z, there is a
larger difference in intensity for
relatively close values of Z.  Details
in the larger values of Z are displayed
at the expense of the smaller values
of Z.
.LI "=~3"
The mapping is logarithmic, so
details of smaller values of Z are shown
at the expense of larger values of Z.
.LI "=~4"
Sinusoidal mapping, so mid-range values
of Z show details at the expense of
extreme values of Z.
.LI "=~5"
Arcsine mapping, so extreme values of
Z are shown at the expense of mid-range
values of Z.
.LE
.LI "\fBNPRM \fR"
Flag to control the drawing of a
perimeter around the halftone picture.
.BL
.LI
NPRM=0:  The perimeter is drawn with
ticks pointing at data locations.
(Side lengths are proportional to number
of data values.)
.LI
NPRM positive:  No perimeter is drawn.  The
picture fills the frame.
.LI
NPRM negative:  The picture is within the
confines of the user's current viewport
setting.
.LE
.LI "\fBISPV\fR"
Flag to tell if the special value feature
is being used.  The special value feature
is used to mark areas where the data is
not known or holes are wanted in the
picture.
.BL
.LI
ISPV = 0:  Special value feature not in
use.  SPVAL is ignored.
.LI
ISPV non-zero:  Special value feature
in use.  SPVAL defines the special
value.  Where Z contains the special
value, no halftone is drawn.  If ISPV
.VL 1c
.LI "=~0"
Special value feature not in use.
SPVAL is ignored.
.LI "=~1"
Nothing is drawn in special value
area.
.LI "=~2"
Contiguous special value areas are
surrounded by a polygonal line.
.LI "=~3"
Special value areas are filled
with X(s).
.LI "=~4"
Special value areas are filled in
with the highest intensity.
.LE
.sp
.LI "\fBSPVAL\fR"
The value used in Z to denote missing
values.  This argument is ignored if
ISPV = 0.
.H 3 "\fB On Output for HAFTON\fR"
All arguments are unchanged.
.H 3 "Note"
This routine produces a huge number of
plotter instructions per picture, averaging
over 100,000 line-draws per frame when M = N.
.H 3 "\fB Entry Points\fR"
EZHFTN, HAFTON, ZLSET, GRAY, BOUND, HFINIT
.H 3 "\fBCommon Blocks\fR"
HAFT01, HAFT02, HAFT03, HAFT04
.H 3 "\fBRequired Library Routines\fR"
GRIDAL, the ERPRT77 package, and the SPPS.
.H 3 "\fBRequired GKS Level\fR"
0A
.H 3 "\fBI/O\fR"
Plots halftone picture.
.H 3 "\fBPrecision\fR"
Single
.H 3 "\fBLanguage\fR"
FORTRAN 77
.H 3 "\fBHistory\fR"
Rewrite of PHOMAP originally written by
M. Perry of High Altitude Observatory,
NCAR.
.H 3 "\fBAlgorithm\fR"
Bi-linear interpolation on plotter
(resolution-limited) grid of normalized
representation of data.
.H 3 "\fBPortability\fR"
ANSI FORTRAN 77.
.H 3 "\fBInternal Parameters\fB"
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
Left-hand edge of the plot when NSET=0.  (0.0=left 
edge of frame, 1.0=right edge of frame.)
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
Bottom edge of the plot when NSET=0.  (0.0=
bottom of frame, 1.0=top of frame.)
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
Length of longer edge of plot (see also EXT).
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
Lengths of the sides of the plot are proportional 
to M and N (when NSET=0) except in
extreme cases, namely, when MIN(M,N)/MAX (M,N) is 
less than EXT.  Then a square plot is produced.  
When a rectangular plot is produced,
the plot is centered on the frame (as long as
SIDE+2*XLT = SIDE+2*YBT=1., as with the
defaults.)
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
A parameter to control the extremeness of the
mapping function specified by NOPT.  (For
IABS(NOPT)=0 or 1, the mapping function is
.hw ALPHA
linear and independent of ALPHA.)  For the nonlinear
mapping functions, when ALPHA is changed
to a number closer to 1., the mapping function
becomes more linear; when ALPHA is changed to
a larger number, the mapping function becomes
more extreme.
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
Maximum number of levels.  Limited by plotter.
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
Number of CRT units per gray-scale cell.
Limited by plotter.
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
Number of plotter address units per frame.
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
An array defining which of the available 
intensities are used when less than the maximum
number of intensities are requested.
.br
.di
.nr i| \n(dn
.nr i- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \wName
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wXLT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wYBT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSIDE
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wEXT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wALPHA
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wMXLEV
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNCRTG
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNCRTF
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIL
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wDefault
.if \n(81<\n(38 .nr 81 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.1
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.1
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.8
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w.25
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.6
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w16
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w8
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1024
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w(Below)
.if \n(81<\n(38 .nr 81 \n(38
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
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'XLT\h'|\n(41u'0.1\h'|\n(42u'
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
\&\h'|\n(40u'YBT\h'|\n(41u'0.1\h'|\n(42u'
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
\&\h'|\n(40u'SIDE\h'|\n(41u'0.8\h'|\n(42u'
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
.sp
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'EXT\h'|\n(41u'.25\h'|\n(42u'
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
\&\h'|\n(40u'ALPHA\h'|\n(41u'1.6\h'|\n(42u'
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
\&\h'|\n(40u'MXLEV\h'|\n(41u'16\h'|\n(42u'
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
\&\h'|\n(40u'NCRTG\h'|\n(41u'8\h'|\n(42u'
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
\&\h'|\n(40u'NCRTF\h'|\n(41u'1024\h'|\n(42u'
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
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IL\h'|\n(41u'(Below)\h'|\n(42u'
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-64
.bp
.sp2
.na
.nf
.B
NLEV        Intensities Used
.R
.sp
 2	        5,11,
 3	        4, 8,12,
 4	        3, 6,10,13,
 5	        2, 5, 8,11,14,
 6	        1, 4, 7, 9,12,15,
 7	        1, 4, 6, 8,10,12,15,
 8	        1, 3, 5, 7, 9,11,13,15,
 9	        1, 3, 4, 6, 8,10,12,13,15
 10	        1, 3, 4, 6, 7, 9,10,12,13,15,
 11	        1, 2, 3, 5, 6, 8,10,11,13,14,15,
 12	        1, 2, 3, 5, 6, 7, 9,10,11,13,14,15,
 13	        1, 2, 3, 4, 6, 7, 8, 9,10,12,13,14,15
 14	        1, 2, 3, 4, 5, 6, 7, 9,10,11,12,13,14,15,
 15	        1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
 16	        0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15
.ad
.fi
