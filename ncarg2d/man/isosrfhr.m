.\"
.\"	$Id: isosrfhr.m,v 1.1.1.1 1992-04-17 22:30:30 ncargd Exp $
.\"
.TH ISOSRFHR 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " ISOSRFHR - Iso-surfaces from high resolution 3-d arrays
.dsS1 " CALL DANDR (NV,NW,ST1,LX,NX,NY,IS2,IU,S,IOBJS,MV) Draw and rember
.nrsN 1
.tr ~
.pn 341
.bp
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9ISOSRFHR\s11@"
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
SUBROUTINE INIT3D (EYE,NU,NV,NW,ST1,LX,NY,IS2,IU,S) 
.R
.H 3 "Dimension of Arguments"
EYE(3),ST1(NV,NW,2),IS2(LX,NY),S(4),
IOBJS(MV,NW)
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
This package of three routines produces a
perspective picture of an arbitrary object or
group of objects with the hidden parts not
drawn.  The objects are assumed to be stored
in the format described below, a format that
was chosen to facilitate the display of
functions of three variables  or output from
high resolution three-dimensional computer
simulations.
.H 3 "Usage"
The object is defined by setting elements in a
three-dimensional array to one where the object
is and zero where it is not.  That is, the
position in the array corresponds to a position
in three-space, and the value of the array
tells whether any object is present at that
position or not.  Because a large array is
needed to define objects with good resolution,
only a part of the array is passed to the
package with each call.

There are two subroutines in the package.
INIT3D is called at the beginning of a picture.
This call can be skipped sometimes if certain
criteria are met and certain precautions are
taken.  See the TIMING section for details.
DANDR (Draw AND Remember) is called
successively to process different parts of the
three-dimensional array.  For example,
one plane would be processed in the
first call to DANDR, while a different plane
would be processed in a subsequent call.  An
example follows:
.sp
.sf
        CALL INIT3D(EYE,NU,NV,NW,ST1,LX,NY,IS2,IU,S)
        DO 1 IBKWDS = 1,NU
        I = NU+1-IBKWDS
.br
C  FORM OR READ SLAB I OF THE 3 DIMENSIONAL ARRAY
C  ONLY 1 OR 0 IN THE SLAB, CALLED IOBJS
        . . .
      1 CALL DANDR(NV,NW,ST1,LX,NX,NY,IS2,IU,S,IOBJS,MV)
.ef
.H 3 "ARGUMENTS"
.H 3 "On Input"
.VL .6i
.LI "\fBEYE\fR"
An array, dimensioned 3, containing the U, V,
and W coordinates of the eye position.  Objects
are considered to be in a box with 2 extreme
corners at (1,1,1) and (NU,NV,NW).  The eye
position must have positive coordinates away
from the coordinate planes U = 0, V = 0, and
W = 0.  While gaining experience with the
package, use EYE(1) = 5*NU, EYE(2) = 4*NV,
EYE(3) = 3*NW.
.LI "\fBNU\fR"
U direction length of the box containing the
objects.
.LI "\fBNV\fR"
V direction length of the box containing the
objects.
.LI "\fBNW\fR"
W direction length of the box containing the
objects.
.LI "\fBST1\fR"
A scratch array at least NV*NW*2 words long.
.LI "\fBLX\fR"
The number of words needed to hold NX bits.
Also, the first dimension of IS2.  See NX
and IS2.  On a 60 bit machine, LX=(NX-1)/60+1.
.LI "\fBNY\fR"
Number of cells in the Y-direction of the
model of the image plane.  Also the second
dimension of IS2.
.LI "\fBIS2\fR"
An array holding the image plane model.  It
is dimensioned LX by NY.  The model is NX by
NY and packed densely.  If hidden lines are
drawn, decrease NX and NY (and LX if
possible).  If visible lines are left out of
the picture, increase NX and NY (and LX if
need be).  As a guide, some examples showing
successful choices are listed:
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
.tm warning: can't span n-type cols, changed to c
.rm 80 81 82 83 84 85 86 87
.nr 80 0
.nr 38 \wGiven
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wNU
.if \n(81<\n(38 .nr 81 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w100
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w60
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w40
.if \n(31<\n(38 .nr 31 \n(38
.81
.rm 81
.nr 61 \n(31
.nr 38 \n(61+\n(32
.if \n(38>\n(81 .nr 81 \n(38
.if \n(38<\n(81 .nr 61 +(\n(81-\n(38)/2
.nr 82 0
.nr 38 \wNV
.if \n(82<\n(38 .nr 82 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w100
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w60
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w40
.if \n(31<\n(38 .nr 31 \n(38
.82
.rm 82
.nr 62 \n(31
.nr 38 \n(62+\n(32
.if \n(38>\n(82 .nr 82 \n(38
.if \n(38<\n(82 .nr 62 +(\n(82-\n(38)/2
.nr 83 0
.nr 38 \wNW
.if \n(83<\n(38 .nr 83 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w60
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w60
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w40
.if \n(31<\n(38 .nr 31 \n(38
.83
.rm 83
.nr 63 \n(31
.nr 38 \n(63+\n(32
.if \n(38>\n(83 .nr 83 \n(38
.if \n(38<\n(83 .nr 63 +(\n(83-\n(38)/2
.nr 84 0
.nr 38 \wResulting NX
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wfrom Testing
.if \n(84<\n(38 .nr 84 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w200
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w110
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w75
.if \n(31<\n(38 .nr 31 \n(38
.84
.rm 84
.nr 64 \n(31
.nr 38 \n(64+\n(32
.if \n(38>\n(84 .nr 84 \n(38
.if \n(38<\n(84 .nr 64 +(\n(84-\n(38)/2
.nr 85 0
.85
.rm 85
.nr 86 0
.86
.rm 86
.nr 87 0
.87
.rm 87
.nr 38 \wNY-\n(85-3n-\n(86-3n-\n(87
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 85 +\n(38
.nr 86 +\n(38
.nr 38 \w200-\n(85-3n-\n(86-3n-\n(87
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 85 +\n(38
.nr 86 +\n(38
.nr 38 \w110-\n(85-3n-\n(86-3n-\n(87
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 85 +\n(38
.nr 86 +\n(38
.nr 38 \w75-\n(85-3n-\n(86-3n-\n(87
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 85 +\n(38
.nr 86 +\n(38
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
.nr 46 \n(85+(3*\n(38)
.nr 86 +\n(46
.nr 47 \n(86+(3*\n(38)
.nr 87 +\n(47
.nr TW \n(87
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Given\h'|\n(41u'NU\h'|\n(42u'NV\h'|\n(43u'NW\h'|\n(44u'Resulting NX\h'|\n(45u'NY
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'\h'|\n(43u'\h'|\n(44u'from Testing\h'|\n(45u'
.sp
.R
.ta \n(80u \n(61u \n(62u \n(63u \n(64u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'100\h'|\n(42u'100\h'|\n(43u'60\h'|\n(44u'200\h'|\n(45u'200
.ta \n(80u \n(61u \n(62u \n(63u \n(64u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'60\h'|\n(42u'60\h'|\n(43u'60\h'|\n(44u'110\h'|\n(45u'110
.ta \n(80u \n(61u \n(62u \n(63u \n(64u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'40\h'|\n(42u'40\h'|\n(43u'40\h'|\n(44u'75\h'|\n(45u'75
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.sp
.LI "\fBIU\fR"
Unit number of scratch file for the package.
ST1 will be written NU times on this file.
.LI "\fBS\fR"
A real array, dimensioned 4, which contains the
world coordinates of the area where the picture
is to be drawn.  That is, all plotting
coordinates generated will be bounded as
follows:  X coordinates will be between S(1)
and S(2), Y coordinates will be between S(3)
and S(4).  To prevent distortion, have
S(2)-S(1) = S(4)-S(3).
.br
10.0 .LE. S(I) .LE. 1010.0   for I = 1,2,3,4.
.LI "\fBIOBJS\fR"
A NV by NW array (with actual first dimension
MV in the calling program) describing the
object.  If this is the Ith call to DANDR,
the part of the picture at U = NU+1-I is to
be processed.  IOBJS defines the objects to
be drawn in the following manner:
IOBJS(J,K) = 1 if any object contains the
point (NU+1-I,J,K) and IOBJS(J,K) = 0
otherwise.
.LI "\fBNX\fR"
This variable is an argument to DANDR.  It is
the number of cells in the X-direction of a
model of the image plane.  A silhouette of
the parts of the picture processed so far is
stored in this model.  Lines to be drawn are
tested for visibility by examining the
silhouette.  Lines in the silhouette are
hidden.  Lines out of the silhouette are
visible.  The solution is approximate
because the silhouette is not formed exactly.
See IS2.
.LI "\fBMV\fR"
Actual first dimension of IOBJS in the
calling program.  When plotting all of IOBJS,
NV = MV.
.LE
.H 3 "On Output"
EYE, NU, NV, NW, LX, NX, NY, IU, S, IOBJS, and
MV are unchanged.  ST1 and IS2 have been
written in.
.H 3 "Note"
This routine is for large arrays, 40 x 40 x 30
is a practical minimum.
.H 3 "Entry Points"
INIT3D, PERSPC, and DANDR
.H 3 "Common Blocks"
None
.H 3 "I/O"
Plots visible surfaces, uses scratch file or
tape.
.H 3 "Precision"
Single
.H 3 "Language"
FORTRAN 77
.H 3 "History"
Originally developed at NCAR starting in
late 1970.
.H 3 "Algorithm"
The basic method is to contour cuts through
the array starting with a cut nearest the
observer.  The algorithm leaves out the hidden
parts of the contours by suppressing those lines
enclosed within the lines produced while processing
the preceding cuts.  The technique is described
in detail in the reference cited below.
.H 3 "Required GKS Level"
0A
.H 3 "Reference"
Wright, T.: "A One-pass Hidden-line Remover for
Computer Drawn Three-space Objects."  Proc 1972
Summer Computer Simulation Conference, 261-267,
1972.
.H 3 "Accuracy"
The algorithm is not exact.  However,
reasonable pictures are produced.
.H 3 "Timing"
This routine is very time consuming.  If many
pictures are produced with the same size arrays
and eye position, much time can be saved by
rewinding unit IU, filling IS2 with zeros, and
skipping the call to INIT3D for other than the
first picture.
.H 3 "Portability"
Two machine-dependent constants are initialized
in DANDR.  INIT3D has an entry statement for
PERSPC.  In DANDR, .AND. and .OR. are used for
masking operations.
