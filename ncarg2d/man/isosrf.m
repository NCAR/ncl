.\"
.\"	$Id: isosrf.m,v 1.1.1.1 1992-04-17 22:30:30 ncargd Exp $
.\"
.TH ISOSRF 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " ISOSRF - Iso-surfaces from 2-d arrays, hidden lines removed
.dsS1 " CALL EZISOS (T,MU,MV,MW,EYE,SLAB,TISO) If criteria below is met, else
.nrsN 1
.tr ~
.pn 335
.bp
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9ISOSRF\s11@"
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
.na
.nf
SUBROUTINE ISOSRF (T,LU,MU,LV,MV,MW,EYE,MUVWP2,SLAB,TISO,IFLAG)           
.ad
.fi
.R
.H 3 "Dimension of Arguments"
T(LU,LV,MW),EYE(3),SLAB(MUVWP2,MUVWP2)
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
ISOSRF draws an approximation of an iso-surface from a three-dimensional array with
hidden lines removed.
.H 3 "Usage"
If the following assumptions are met, use

CALL EZISOS (T,MU,MV,MW,EYE,SLAB,TISO)

Assumptions:
.BL
.LI
All of the T array is to be used.
.LI
IFLAG is chosen internally.
.LI
FRAME is called by EZISOS.
.LE
.sp
If the assumptions are not met, use

.nf
.na
CALL ISOSRF (T,LU,MU,LV,MV,MW,EYE,MUVWP2,
             SLAB,TISO,IFLAG)
.ad
.fi
.H 3 "ARGUMENTS"
.H 3 "On Input"
.VL .9i
.LI "\fBT\fR"
Three dimensional array of data that defines
the iso-surface.
.LI "\fBLU\fR"
First dimension of T in the calling program.
.LI "\fBMU\fR"
The number of data values of T to be
processed in the U direction (the first
subscript direction).  When processing the
entire array, LU = MU (and LV = MV).
.LI "\fBLV\fR"
Second dimension of T in the calling program.
.LI "\fBMV\fR"
The number of data values of T to be
processed in the V direction (the second
subscript direction).
.LI "\fBMW\fR"
The number of data values of T to be
processed in the W direction (the third
subscript direction).
.LI "\fBEYE\fR"
The position of the eye in three-space.  T is
considered to be in a box with opposite
corners (1,1,1) and (MU,MV,MW).  The eye is
at (EYE(1),EYE(2),EYE(3)), which must be
outside the box that contains T.  While gaining
experience with the routine, a good choice
for EYE might be (5.0*MU,3.5*MV,2.0*MW).
.LI "\fBMUVWP2\fR"
The maximum of (MU,MV,MW)+2; that is,
MUVWP2 = MAX(MU,MV,MW)+2).
.LI "\fBSLAB\fR"
A work space used for internal storage.  SLAB
must be at least MUVWP2*MUVWP2 words long.
.LI "\fBTISO\fR"
The iso-value used to define the surface.  The
surface drawn will separate volumes of T that
have values greater than or equal to TISO from
volumes of T that have values less than TISO.
.LI "\fBIFLAG\fR"
This flag serves two purposes.
.BL
.LI
First, the absolute value of IFLAG
determines which types of lines are drawn
to approximate the surface.  Three types
of lines are considered:  lines of
constant U, lines of constant V, and lines
of constant W.  The following table lists
the types of lines drawn.

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
.nr 38 \wIABS(IFLAG)
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
.nr 38 \w~~0, 7
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w OR MORE~~
.if \n(32<\n(38 .nr 32 \n(38
.80
.rm 80
.nr 60 \n(31
.nr 38 \n(60+\n(32
.if \n(38>\n(80 .nr 80 \n(38
.if \n(38<\n(80 .nr 60 +(\n(80-\n(38)/2
.nr 81 0
.nr 38 \wU
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~NO~~
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~NO~~
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~NO~~
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~YES~~
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~YES~~
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~YES~~
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~~YES~~
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wV
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w~~NO~~
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w~~YES~~
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w~~YES~~
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w~~NO~~
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w~~NO~~
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w~~YES~~
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \w~~YES~~
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.83
.rm 83
.nr 84 0
.84
.rm 84
.nr 85 0
.85
.rm 85
.nr 38 \wLines of Constant-\n(81-3n-\n(82-3n-\n(83-3n-\n(84-3n-\n(85
.if \n(38>0 .nr 38 \n(38/4
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 83 +\n(38
.nr 84 +\n(38
.nr 38 \wW-\n(83-3n-\n(84-3n-\n(85
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 83 +\n(38
.nr 84 +\n(38
.nr 38 \w~~YES~~-\n(83-3n-\n(84-3n-\n(85
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 83 +\n(38
.nr 84 +\n(38
.nr 38 \w~~NO~~-\n(83-3n-\n(84-3n-\n(85
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 83 +\n(38
.nr 84 +\n(38
.nr 38 \w~~YES~~-\n(83-3n-\n(84-3n-\n(85
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 83 +\n(38
.nr 84 +\n(38
.nr 38 \w~~NO~~-\n(83-3n-\n(84-3n-\n(85
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 83 +\n(38
.nr 84 +\n(38
.nr 38 \w~~YES~~-\n(83-3n-\n(84-3n-\n(85
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 83 +\n(38
.nr 84 +\n(38
.nr 38 \w~~NO~~-\n(83-3n-\n(84-3n-\n(85
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 83 +\n(38
.nr 84 +\n(38
.nr 38 \w~~YES~~-\n(83-3n-\n(84-3n-\n(85
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 83 +\n(38
.nr 84 +\n(38
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 60 +\n(40
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
.ta \n(80u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'Lines of Constant
.sp
.ta \n(80u \n(81u \n(82u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IABS(IFLAG)\h'|\n(41u'U\h'|\n(42u'V\h'|\n(43u'W
.R
.sp
.ta \n(60u \n(80u \n(81u \n(82u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'~~NO~~\h'|\n(42u'~~NO~~\h'|\n(43u'~~YES~~
.ta \n(60u \n(80u \n(81u \n(82u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2\h'|\n(41u'~~NO~~\h'|\n(42u'~~YES~~\h'|\n(43u'~~NO~~
.ta \n(60u \n(80u \n(81u \n(82u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3\h'|\n(41u'~~NO~~\h'|\n(42u'~~YES~~\h'|\n(43u'~~YES~~
.ta \n(60u \n(80u \n(81u \n(82u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'4\h'|\n(41u'~~YES~~\h'|\n(42u'~~NO~~\h'|\n(43u'~~NO~~
.ta \n(60u \n(80u \n(81u \n(82u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'5\h'|\n(41u'~~YES~~\h'|\n(42u'~~NO~~\h'|\n(43u'~~YES~~
.ta \n(60u \n(80u \n(81u \n(82u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'6\h'|\n(41u'~~YES~~\h'|\n(42u'~~YES~~\h'|\n(43u'~~NO~~
.ta \n(60u \n(80u \n(81u \n(82u \n(85u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'~~0, 7 OR MORE~~\h'|\n(41u'~~YES~~\h'|\n(42u'~~YES~~\h'|\n(43u'~~YES~~
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-18
.LI
Second, the sign of IFLAG determines what
is inside and what is outside, hence,
which lines are visible and what is done
at the boundary of T.  For IFLAG:
.LE
.VL 1i
.LI "POSITIVE"
T values greater than TISO are
assumed to be inside the solid
formed by the drawn surface.
.LI "NEGATIVE"
T values less than TISO are
assumed to be inside the solid
formed by the drawn surface.
If the algorithm draws a cube, reverse the
sign of IFLAG.
.LE
.H 3 "On Output"
T,LU,MU,LV,MV,MW,EYE,MUVWP2,TISO and IFLAG are
unchanged.  SLAB has been written in.
.H 3 "Note"
.BL
.LI
This routine is for lower resolution arrays
than ISOSRFHR.  40 by 40 by 40 is a
practical maximum.
.LI
Transformations can be achieved by
adjusting scaling statement functions in
ISOSRF, SET3D, and TR32.
.LI
The hidden-line algorithm is not exact, so
visibility errors can occur.
.LI
Three-dimensional perspective character
labeling of ISOSRF is possible by using
the utility PWRZI.  For a description of
the usage, see the PWRZI documentation in this manual.
.LE
.H 3 "Entry Points"
ISOSRF, EZISOS, SET3D, TRN32I, ZEROSC,
STCNTR, DRCNTR, TR32, FRSTS, KURV1S, KURV2S,
FRSTC, FILLIN, DRAWI, ISOSRB, MMASK
.H 3 "Common Blocks"
ISOSR1, ISOSR2, ISOSR3, ISOSR4, ISOSR5,
ISOSR6, ISOSR7, ISOSR8, ISOSR9, TEMPR,
PWRZ1I
.H 3 "Required Library Routines"
The ERPRT77 package and the SPPS.
.H 3 "Required GKS Level"
0A
.H 3 "I/O"
Plots surface
.H 3 "Precision"
Single
.H 3 "Language"
FORTRAN 77
.H 3 "History"
Developed for users of ISOSRFHR with smaller
arrays.
.H 3 "Algorithm"
Cuts through the three-dimensional array are
contoured with a smoothing contourer that also
marks a model of the plotting plane.  Interiors
of boundaries are filled in and the result is
\&.OR.ed into another model of the plotting plane
that is used to test subsequent contour lines
for visibility.
.H 3 "Timing"
Varies widely with size of T and the volume of
the space enclosed by the surface drawn.
.H 3 "Note"
Space requirements can be reduced by
changing the size of the arrays ISCR, ISCA
(found in COMMON ISOSR2), MASK(found in
COMMON ISOSR5), and the variable NBPW
(COMMON ISOSR5).
ISCR and ISCA need 128x128 bits.  So on a
64 bit machine ISCR, ISCA can be
dimensioned to (2,128). NBPW set in
subroutine MMASK should contain the
number of bits per word you wish to
utilize.
The dimension of MASK and NMASK should
equal the value of NBPW.
LS should be set to the first dimension
of ISCA and ISCR.
.H 3 "Examples"
On a 60 bit machine:
.ti +5
DIMENSION ISCA(4,128), ISCR(4,128)
.ti +5
DIMENSION MASK(32)
.ti +5
NBPW = 32
.sp
On a 64 bit machine:
.ti +5
DIMENSION ISCA(2,128), ISCR(2,128)
.ti +5
DIMENSION MASK(64)
.ti +5
NBPW = 64
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
.BL
.LI
IREF=nonzero~~~Draw axes.
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
.LI
IREF=zero~~~~~~Do not draw axes.
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
.nr 38 \wIREF
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wDefault
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w1
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
.nr 38 \wFunction-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wFlag to control drawing of axes.-\n(82-3n-\n(83-3n-\n(84
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
\&\h'|\n(40u'Name\h'|\n(41u'Default\h'|\n(42u'Function
.R
.sp
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IREF\h'|\n(41u'1\h'|\n(42u'Flag to control drawing of axes.
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
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
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
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
.LE
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+

.if \n-(b.=0 .nr c. \n(.c-\n(d.-19
