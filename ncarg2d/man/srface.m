.\"
.\"	$Id: srface.m,v 1.1.1.1 1992-04-17 22:30:31 ncargd Exp $
.\"
.TH SRFACE 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " SRFACE - 3-d plot of functions with 2 variables
.dsS1 " CALL EZSRFC (Z,M,ANGH,ANGV,WORK) if criteria below is met, else
.nrsN 1
.tr ~
.pn 371
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9SRFACE\s11@"
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
SUBROUTINE SRFACE (X,Y,Z,M,MX,NX,NY,S,STEREO)
.R
.H 3 "Dimension of Arguments"
X(NX),Y(NY),Z(MX,NY),M(2,NX,NY),S(6)
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
SRFACE draws a perspective picture of a
function of two variables with hidden lines
removed.  The function is approximated by a
two-dimensional array of heights.
.H 3 "Usage"
If the following assumptions are met, use

CALL EZSRFC (Z,M,N,ANGH,ANGV,WORK)

Assumptions:
.BL
.LI
the entire array is to be drawn,
.LI
the data are equally spaced (in the
X-Y plane),
.LI
no stereo pairs,
.LI
scaling is chosen internally.
.LE
.sp
If these assumptions are not met, use

CALL SRFACE (X,Y,Z,M,MX,NX,NY,S, STEREO)
.H 3 "ARGUMENTS"
.H 3 "On Input for EZSRFC"
.VL .9i
.LI "\fBZ\fR"
The M by N array to be drawn.
.LI "\fBM\fR"
The first dimension of Z.
.LI "\fBN\fR"
The second dimension of Z.
.LI "\fBANGH\fR"
Angle in degrees in the X-Y plane to the
line of sight (counterclockwise from
the plus-X axis).
.LI "\fBANGV\fR"
Angle in degrees from the X-Y plane to
the line of sight (positive angles are
above the middle Z, negative below).
.LI "\fBWORK\fR"
A scratch storage dimensioned at least
2*M*N+M+N.
.LE
.H 3 "On Output for EZSRFC"
Z, M, N, ANGH, ANGV are unchanged.  WORK
has been written in.
.bp
.H 3 "ARGUMENTS"
.H 3 "On Input for SRFACE"
.VL .9i
.LI "\fBX\fR"
A linear array NX long containing the X
coordinates of the points in the surface
approximation.  See the "Notes" below.
.LI "\fBY\fR"
The linear array NY long containing the
Y coordinates of the points in the
surface approximation.  See the "Notes" below.
.LI "\fBZ\fR"
An array MX by NY containing the surface
to be drawn in NX by NY cells.
Z(I,J) = F(X(I),Y(J)).  See the "Notes" below.
.LI "\fBM\fR"
Scratch array at least 2*NX*NY words
long.
.LI "\fBMX\fR"
First dimension of Z.
.LI "\fBNX\fR"
Number of data values in the X direction
(the first subscript direction) in Z
to be plotted. When plotting an entire
array, MX=NX.
.LI "\fBNY\fR"
Number of data values in the Y direction
(the second subscript direction) to be
plotted.
.LI "\fBS\fR"
S defines the line of sight.  The viewer's
eye is at (S(1), S(2), S(3)) and the
point looked at is at (S(4), S(5), S(6)).
The eye should be outside the block with
opposite corners (X(1), Y(1), ZMIN) and
(X(NX), Y(NY), ZMAX) and the point looked
at should be inside it.  For a nice
perspective effect, the distance between
the eye and the point looked at should be
5 to 10 times the size of the block.  See
the "Notes" below.
.LI "\fBSTEREO\fR"
Flag to indicate if stereo pairs are to
be drawn.  0.0 means no stereo pair (one
picture).  Non-zero means put out two
pictures.  The value of STEREO is the
relative angle between the eyes.  A value
of 1.0 produces standard separation.
Negative STEREO reverses the left and
right figures.  See the documentation below
for internal variable ISTP for additional
information.
.LE
.H 3 "On Output for SRFACE"
X, Y, Z, MX, NX, NY, S, STEREO are
unchanged.  M has been written in.
.H 3 "Notes"
.BL
.LI
The range of Z compared with the range
of X and Y determines the shape of the
picture.  They are assumed to be in the
same units and not wildly different in
magnitude.  S is assumed to be in the
same units as X, Y, and Z.
.LI
Picture size can be made relative to
distance.  See comments in SETR.
.LI
TRN32S can be used to translate from 3
space to 2 space.  See comments there.
.LI
Data with extreme discontinuities may
cause visibility errors.  If this problem
occurs, use a distant eye position
away from the +Z axis.
.bp
.LI
The default line color is set to
color index 1.  If the user wishes to
change the line color, he can do so by
defining color index 1 before calling
SRFACE, or by putting the common block
SRFINT in his calling 
program and
defining and using color index ISRFMJ
(defaulted to 1 in BLOCKDATA).
.LE
.H 3 "Entry Points"
SRFACE, SRFGK, EZSRFC, SETR, DRAWS, TRN32S,
CLSET, CTCELL, SRFABD
.H 3 "Common Blocks"
SRFACE, SRFINT, SRFBLK, PWRZIS, SRFIP1
.H 3 "Required Library Routines"
The SPPS
.H 3 "Required GKS Level"
0A
.H 3 "I/O"
Plots
.H 3 "Precision"
Single
.H 3 "Language"
FORTRAN 77
.H 3 "History"
Converted to FORTRAN 77 and GKS in March 1984.

Prepared for SIGGRAPH, August 1976.

Standardized in January 1973.

Written in December 1971.  Replaced K.S.+G.
algorithm called SOLIDS at NCAR.

.H 3 "Algorithm"
The data are processed from the near side of
the surface to the far side.  Visibility
information is stored (see reference).
Highest so far is visible from above.
.H 3 "Reference"
Wright, T.J., "A Two Space Solution to the
Hidden Line Problem for Plotting a Function
of Two Variables." \fIIEEE Trans. Comp.,\fR
pp 28-33, January 1973.
.H 3 "Accuracy"
If the ends of a line segment are visible,
the middle is assumed visible.
.H 3 "Timing"
Proportional to NX*NY.
.bp
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
STEREO type if STEREO non-zero.
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
Approximate number of
levels of constant Z that
are drawn if levels are not
specified.  40 levels
maximum.
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
.nr 38 \wIFR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wISTP
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIROTS
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIDRX
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIDRY
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIDRZ
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIUPPER
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wISKIRT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNCLA
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wTHETA
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wHSKIRT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCHI
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCLO
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCINC
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wDefault
.if \n(81<\n(38 .nr 81 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w1
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w6
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.02
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
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
.nr 38 \w~-1~~Call FRAME first.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~0~~Do not call FRAME.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w+1~~Call FRAME when done.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~-1~~Alternating frames, slightly offset -\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~~~~~~(for movies,  IROTS = 0).-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~0~~Blank frame between for stereo slide.  -\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~~~~~~IROTS = 1).-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w+1~~Both on same frame.  (left picture to left -\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~~~~~~side.  IROTS = 0).-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~0~~+Z in vertical plotting direction (CINE mode).-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w+1~~+Z in horizontal plotting direction (COMIC mode).-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w+1~~Draw lines of constant X.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~0~~Do not.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w+1~~Draw lines of constant Y.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~0~~Do not.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w+1~~Draw lines of constant Z (contour lines).-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~0~~Do not.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w+1~~Draw upper side of surface.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~0~~Draw both sides.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~-1~~Draw lower side.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w+1~~Draw a skirt around the surface.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~~~~~BOTTOM = HSKIRT.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~0~~Do not.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wAngle, in radians, between eyes for stereo pairs.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wHeight of skirt (if ISKIRT = 1).-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wHighest level of constant Z.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wLowest level of constant Z.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wIncrement between levels.-\n(82-3n-\n(83-3n-\n(84
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
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IFR\h'|\n(41u'1\h'|\n(42u'~-1~~Call FRAME first.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~0~~Do not call FRAME.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'+1~~Call FRAME when done.
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ISTP\h'|\n(41u'0\h'|\n(42u'
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
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~-1~~Alternating frames, slightly offset 
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~~~~~~(for movies,  IROTS = 0).
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~0~~Blank frame between for stereo slide.  
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~~~~~~IROTS = 1).
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'+1~~Both on same frame.  (left picture to left 
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~~~~~~side.  IROTS = 0).
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.sp
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IROTS\h'|\n(41u'0\h'|\n(42u'~~0~~+Z in vertical plotting direction (CINE mode).
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'+1~~+Z in horizontal plotting direction (COMIC mode).
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IDRX\h'|\n(41u'1\h'|\n(42u'+1~~Draw lines of constant X.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~0~~Do not.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IDRY\h'|\n(41u'1\h'|\n(42u'+1~~Draw lines of constant Y.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~0~~Do not.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IDRZ\h'|\n(41u'0\h'|\n(42u'+1~~Draw lines of constant Z (contour lines).
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~0~~Do not.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IUPPER\h'|\n(41u'0\h'|\n(42u'+1~~Draw upper side of surface.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~0~~Draw both sides.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~-1~~Draw lower side.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ISKIRT\h'|\n(41u'0\h'|\n(42u'+1~~Draw a skirt around the surface.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~~~~~BOTTOM = HSKIRT.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~0~~Do not.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NCLA\h'|\n(41u'6\h'|\n(42u'
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
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'THETA\h'|\n(41u'.02\h'|\n(42u'Angle, in radians, between eyes for stereo pairs.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'HSKIRT\h'|\n(41u'0.\h'|\n(42u'Height of skirt (if ISKIRT = 1).
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CHI\h'|\n(41u'0.\h'|\n(42u'Highest level of constant Z.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CLO\h'|\n(41u'0.\h'|\n(42u'Lowest level of constant Z.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CINC\h'|\n(41u'0.\h'|\n(42u'Increment between levels.
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-62
.sp
(If CHI, CLO, or CINC is zero, a nice
value is generated automatically.)
.bp
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
Flag to control use of special
value feature.  Do not have
both IOFFP=1 and ISKIRT=1.
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
Special value used to mark unknown data when IOFFP=1.
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
.nr 38 \wIOFFP~
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSPVAL~~
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
.nr 38 \w~~~
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w0
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.~~
.if \n(32<\n(38 .nr 32 \n(38
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
.nr 38 \w~~~~0~~Feature not in use-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~+1~~Feature in use.  No lines drawn to data -\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \w~~~~~~~points in Z that are equal to SPVAL.-\n(82-3n-\n(83-3n-\n(84
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
\&\h'|\n(40u'IOFFP~\h'|\n(41u'0~~~\h'|\n(42u'
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
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~~~0~~Feature not in use
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~+1~~Feature in use.  No lines drawn to data 
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'~~~~~~~points in Z that are equal to SPVAL.
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SPVAL~~\h'|\n(41u'0.~~\h'|\n(42u'
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
.if \n-(b.=0 .nr c. \n(.c-\n(d.-20
.LE
