.\"
.\"	$Id: autograph.m,v 1.1.1.1 1992-04-17 22:30:19 ncargd Exp $
.\"
.TH AUTOGRAPH 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " AUTOGRAPH - Draws curves or families of curves
.dsS1 " CALL EZY (YDRA,NPTS,GLAB) Draw curve
.dsS2 " CALL EZXY (XDRA,YDRA,NPTS,GLAB) Draws curve
.dsS3 " CALL EZMY (YDRA,IDXY,MANY,NPTS,GLAB) Draws family of curves
.dsS4 " CALL EZMXY (XDRA,YDRA,IDXY,MANY,NPTS,GLAB) Draws family of curves
.dsS5 " CALL IDIOT (XDRA,YDRA,NPTS,LTYP,LDSH,LABX,LABY,LABG,LFRA) ~~~~~~~~~~~~~~~~~~~~~~~~~~~ Old version of AUTOG
.dsS6 "
.dsS7 " The following routines provide access to control parameters
.dsS8 " CALL ANOTAT (XLAB,YLAB,LBAC,LSET,NDSH,DSHL)
.dsS9 " CALL DISPLA (LFRA,LROW,LTYP)
.dss1 " CALL AGSETP (TPGN,FURA,LURA)
.dss2 " CALL AGSETF (TPGN,FUSR)
.dss3 " CALL AGSETI (TPGN,ISUR)
.dss4 " CALL AGSETC (TPGN,CUSR)
.dss5 " CALL AGGETP (TPGN,FURA,LURA)
.dss6 " CALL AGGETF (TPGN,FUSR)
.dss7 " CALL AGGETI TPGN,IUSR)
.dss8 " CALL AGGETC (TPGN,CUSR)
.dss9 "
.dsN1 " The following are low-level callable routines
.dsN2 " CALL AGSTUP (XDRA,NVIX,IIVX,NEVX,IIEX,YDRA,NVIY,IIVY,NEVY,IIEY) ~~~~~~~~~~~~~~~~~~~~~~ Set up params.
.dsN3 " CALL AGBACK Draws background
.dsN4 " CALL AGCURV (XVEC,IIEX,YVEC,IIEY,NEXY,KDSH) Draws a curve
.dsN5 "
.dsN6 " The following are other user called routines
.dsN7 " CALL AGSAVE (IFNO) Save state
.dsN8 " CALL AGRSTR (IFNO) Restore state
.dsN9 " CALL AGBNCH (IDSH) Set dash pattern
.dsN0 " CALL AGDSHN (IDSH) Set dash
.nrsN 28
.pn 91
.bp
.PH ""
.PF ""
.SK 1
.tr ~
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9AUTOGRAPH\s11@"
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
.H 3
Following is a brief description of the AUTOGRAPH package.  For
complete documentation, see the manual, \fIAUTOGRAPH:  A Graphing
Utility\fR, which was shipped to you along with this manual.
.H 3 "Important Note"
If you have existing programs that use the FORTRAN 66 version 
of AUTOGRAPH (any version of AUTOGRAPH acquired before 
October 23, 1984), you must first convert them to use the FORTRAN
77 version of AUTOGRAPH.  In the manual, \fIAUTOGRAPH:
A Graphing Utility\fR, see Appendix A, "Conversion 
from FORTRAN 66 AUTOGRAPH to FORTRAN 77 AUTOGRAPH,"
for instructions.
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
To draw graphs, each with a labeled background
and each displaying one or more curves.
.H 3 "Access (on the CRAY)"
To use AUTOGRAPH routines on the CRAY, simply
call them; they are in the binary library
$NCARLB, which is automatically searched.
.sp
To get smoother curves, drawn using spline
interpolation, compile DASHSMTH, from ULIB,
to replace DASHCHAR, from $NCARLB:
.sp
.nf
GETSRC,LIB=ULIB,FILE=DASHSMTH,L=DSMTH.
CFT,I=DSMTH,L=0.
.fi
.sp
AUTOGRAPH contains a routine AGPWRT, which it
calls to draw labels.  This routine just passes
its arguments on to the system-plot-package
routine PWRIT.  To use one of the fancier
character-drawers, like PWRITX or PWRITY,
just compile a routine AGPWRT to replace the
default version; it has the same arguments as
PWRIT and may either draw the character string
itself, or just pass the arguments on to a
desired character-drawer.  
AUTOGRAPH contains a subroutine AGPWRT that you can modify
so that AUTOGRAPH will access character fonts that are
different from the default font.  In its distributed form, AGPWRT is a 
dummy subroutine.  Consult the documentation in the code.  The file 
AGUPWRTX on the distribution tape contains an implementation of AGPWRTX that 
allows AUTOGRAPH to access the PWRITX character set.  See the documentation
in the code for AGUPWRT.
See your NCAR Graphics site representative to learn how to access the code.
.H 3 "Usage"
Following this indented preamble are given two
lists:  one describing the AUTOGRAPH routines
and another describing the arguments of those
routines.
.sp
\fIAUTOGRAPH:  A Graphing Utility\fR gives
a complete write-up of AUTOGRAPH, in great
detail and with a set of helpful examples.
.H 3 "Entry Points"
Except for seven routines, which are included
in the package for historical reasons (EZY,
EZXY, EZMY, EZMXY, IDIOT, ANOTAT, and DISPLA),
the AUTOGRAPH routines have six-character names
beginning with the characters AG.  An alphabetized 
list follows:
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
.nr 38 \wAGAXIS
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAGBACK
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAGBNCH
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAGCHAX
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAGCHCU
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAGCHIL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAGCHNL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAGCTCS
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAGCTKO
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wAGCURV
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAGDASH
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAGDFLT
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAGDLCH
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAGDSHN
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAGEXAX
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAGEXUS
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAGEZSU
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wAGFPBN
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wAGFTOL
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wAGGETC
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wAGGETF
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wAGGETI
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wAGGETP
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wAGGTCH
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wAGINIT
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wAGKURV
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wAGLBLS
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wAGMAXI
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAGMINI
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAGNUMB
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAGPPID
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAGPWRT
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAGQURV
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAGRPCH
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAGRSTR
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wAGSAVE
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wAGSCAN
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAGSETC
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAGSETF
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAGSETI
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAGSETP
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAGSRCH
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAGSTCH
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAGSTUP
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wAGUTOL
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
.nr 85 0
.85
.rm 85
.nr 86 0
.86
.rm 86
.nr 87 0
.87
.rm 87
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AGAXIS\h'|\n(41u'AGCURV\h'|\n(42u'AGFTOL\h'|\n(43u'AGMAXI\h'|\n(44u'AGSCAN\h'|\n(45u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AGBACK\h'|\n(41u'AGDASH\h'|\n(42u'AGGETC\h'|\n(43u'AGMINI\h'|\n(44u'AGSETC\h'|\n(45u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AGBNCH\h'|\n(41u'AGDFLT\h'|\n(42u'AGGETF\h'|\n(43u'AGNUMB\h'|\n(44u'AGSETF\h'|\n(45u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AGCHAX\h'|\n(41u'AGDLCH\h'|\n(42u'AGGETI\h'|\n(43u'AGPPID\h'|\n(44u'AGSETI\h'|\n(45u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AGCHCU\h'|\n(41u'AGDSHN\h'|\n(42u'AGGETP\h'|\n(43u'AGPWRT\h'|\n(44u'AGSETP\h'|\n(45u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AGCHIL\h'|\n(41u'AGEXAX\h'|\n(42u'AGGTCH\h'|\n(43u'AGQURV\h'|\n(44u'AGSRCH\h'|\n(45u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AGCHNL\h'|\n(41u'AGEXUS\h'|\n(42u'AGINIT\h'|\n(43u'AGRPCH\h'|\n(44u'AGSTCH\h'|\n(45u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AGCTCS\h'|\n(41u'AGEZSU\h'|\n(42u'AGKURV\h'|\n(43u'AGRSTR\h'|\n(44u'AGSTUP\h'|\n(45u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u \n(87u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AGCTKO\h'|\n(41u'AGFPBN\h'|\n(42u'AGLBLS\h'|\n(43u'AGSAVE\h'|\n(44u'AGUTOL\h'|\n(45u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-12
.sp
NOTE:  The routine AGDFLT is a block-data
routine specifying the default values of
AUTOGRAPH control parameters.
.H 3 "Special Conditions"
Under certain conditions, AUTOGRAPH may print
an error message (via the routine SETER) and
stop.  Each error message includes the name of
the routine which issued it.  A description of
the condition which caused the error may be
found in the AUTOGRAPH write-up in the NCAR
graphics manual; look in the write-up of the
routine which issued the error message, under
the heading 'SPECIAL CONDITIONS.'

For error messages issued by the routine
AGNUMB, see the write-up of the routine AGSTUP.

If you get an error in the routine ALOG10, it
probably means that you are using a logarithmic
axis and some of the coordinate data along that
axis are zero or negative.
.H 3 "Common Blocks"
The AUTOGRAPH common blocks are AGCONP, AGORIP,
AGOCHP, AGCHR1, and AGCHR2.  AGCONP contains
the AUTOGRAPH "control parameters", primary and
secondary, all of which are real, AGORIP other
real and/or integer parameters, AGOCHP other
character parameters, AGCHR1 and AGCHR2 the
variables implementing the character-storage-and-retrieval 
scheme of AUTOGRAPH.
.H 3 "I/O"
Lower-level plotting routines are called to
produce graphical output and, when errors
occur, error messages may be written to the
system error file, as defined by I1MACH(4),
either directly or by way of a call to SETER.
.H 3 "Required ULIB Routines"
AUTOGRAPH uses the software dashed-line package
DASHCHAR.  Of course, either of the packages
DASHSMTH or DASHSUPR may be used instead, to
get smoother curves.
.H 3 "Language"
FORTRAN 77
.H 3 "History"
Dave Robertson wrote the original routine
IDIOT, which was intended to provide a simple,
quick-and-dirty, x-y graph-drawing capability.
In time, as it became obvious that many users
were adapting IDIOT to more sophisticated
tasks, Dan Anderson wrote the first AUTOGRAPH
package, based on IDIOT.  It allowed the user
to put more than one curve on a graph, to use
more sophisticated backgrounds, to specify
coordinate data in a variety of ways, and to
more easily control the scaling and positioning
of graphs.  Eventually, this package, too, was
found wanting.  In 1977, Dave Kennison entirely
re-wrote AUTOGRAPH, with the following goals:
to maintain the ease of use for simple graphs
which had been the principal virtue of the
package, to provide the user with as much
control as possible, to incorporate desirable
new features, and to make the package as
portable as possible.  In 1984, the package
was again worked over by Dave Kennison, to
make it compatible with FORTRAN 77 and
to remove any dependency on the LOC function,
which had proved to cause difficulties on
certain machines.  The user interface was
changed somewhat and some new features were
added.  A GKS-compatible version was written.
.H 3 "Space Required"
AUTOGRAPH is big; one pays a price for its
capabilities.  On the CRAY, it occupies a
little under 30000 (octal) locations.  The
required plot package routines take about
another 7000 (octal), the (modified) PORT
support routines about another 1000 (octal),
and system routines (math, I/O, miscellany)
another 30000 (octal).
.H 3 "Portability"
AUTOGRAPH may be ported with few modifications
to most systems having a FORTRAN 77 compiler.

The labeled common blocks may have to be
declared in a part of the user program which
is always core-resident so that variables
in them will maintain their values from one
AUTOGRAPH-routine call to the next.  Such a
problem may arise when AUTOGRAPH is placed in
an overlay or when some sort of memory-paging
scheme is used.
.H 3 "Required Resident Routines"
AUTOGRAPH uses the DASHCHAR routines DASHDB,
DASHDC, FRSTD, LASTD, LINED, AND VECTD, the
system-plot-package routines FRAME, GETSET,
GETSI, LINE, PWRIT, and SET, the support
routines ISHIFT and IOR, the (modified)
PORT utilities SETER and I1MACH, and the
FORTRAN-library routines ALOG10, ATAN2, COS,
SIN, AND SQRT.
.H 3 "Required GKS Level"
0A
.H 3 "User-Callable AUTOGRAPH Routines"
Following is a list of AUTOGRAPH routines to be called by the user
(organized by function).  Each routine is described briefly.  The
arguments of the routines are described in the next section.

Each of the following routines draws a complete graph with one call.
Each is implemented by a set of calls to the lower-level AUTOGRAPH
routines AGSTUP, AGCURV, and AGBACK (see, below).
.BL
.LI
EZY (YDRA,NPTS,GLAB) \(em draws a graph of the curve defined by the
data points 
.br
~~~~~~~~~~((I,YDRA(I)),I=1,NPTS), 
.br
with a graph label specified
by GLAB.
.LI
EZXY (XDRA,YDRA,NPTS,GLAB) \(em draws a graph of the curve defined by
the data points 
.br
~~~~~~~~~((XDRA(I),YDRA(I)),I=1,NPTS), 
.br
with a graph label
specified by GLAB.
.LI
EZMY (YDRA,IDXY,MANY,NPTS,GLAB) \(em draws a graph of the family of
curves defined by data points 
.br
~~~~~~~~~(((I,YDRA(I,J)),I=1,NPTS),J=1,MANY),
.br
with a graph label specified by GLAB.  The order of the subscripts
of YDRA may be reversed \(em see the routine DISPLA, argument LROW.
.LI
EZMXY (XDRA,YDRA,IDXY,MANY,NPTS,GLAB) \(em draws a graph of the
family of curves defined by the data points 
.br
~~~~~~~~~(((XDRA(I),YDRA(I,J)), I=1,NPTS),J=1,MANY), 
.br
with a graph label specified by GLAB.  XDRA
may be doubly-subscripted and the order of the subscripts of XDRA
and YDRA may be reversed \(em see the routine DISPLA, argument LROW.
.LI
IDIOT (XDRA,YDRA,NPTS,LTYP,LDSH,LABX,LABY,LABG,LFRA) \(em implements
the routine from which AUTOGRAPH grew \(em not recommended \(em provided
for antique lovers.
.sp
The following routines provide user access to the AUTOGRAPH control
parameters (in the labeled common block AGCONP).
.BL
.LI
ANOTAT (XLAB,YLAB,LBAC,LSET,NDSH,DSHL) \(em may be used to
change the
x- and y-axis (non-numeric) labels, the background type, the way
in which graphs are positioned and scaled, and the type of dash
patterns to be used in drawing curves.
.LI
DISPLA (LFRA,LROW,LTYP) \(em may be used to specify when, if ever,
the EZ. . . routines do a frame advance, how input arrays for EZMY
and EZMXY are dimensioned, and the linear/log nature of graphs.
.LI
AGSETP (TPGN,FURA,LURA) \(em a general-purpose parameter-setting
routine, used to set the group of parameters specified by TPGN,
using values obtained from the array 
.br
~~~~~~~~~(FURA(I),I=1,LURA).
.LI
AGSETF (TPGN,FUSR) \(em used to set the single parameter specified by
TPGN, giving it the floating-point value FUSR.
.LI
AGSETI (TPGN,IUSR) \(em used to set the single parameter specified by
TPGN, giving it the floating-point value FLOAT(IUSR).
.LI
AGSETC (TPGN,CUSR) \(em the character string CUSR is stashed in an
array inside AUTOGRAPH and the floating-point equivalent of an
identifier which may be used for later retrieval of the string is
stored as the value of the single parameter specified by TPGN.  The
single parameter must be a label name, a dash pattern, the 
.ne 2
text of
a label line, or the line-terminator character.
.LI
AGGETP (TPGN,FURA,LURA) \(em a general-purpose parameter-getting
routine, used to get the group of parameters specified by TPGN,
putting the result in the array 
.br
~~~~~~~~~(FURA(I),I=1,LURA).
.LI
AGGETF (TPGN,FUSR) \(em used to get, in FUSR, the floating-point
value of the single parameter specified by TPGN.
.LI
AGGETI (TPGN,IUSR) \(em used to get, in IUSR, the integer equivalent
of the value of the single parameter specified by TPGN.
.LI
AGGETC (TPGN,CUSR) \(em used to get, in CUSR, the character string
whose identifier is specified by the integer equivalent of the
single parameter specified by TPGN.  
.ne 2
The single parameter must
be a label name, a dash pattern, the text of a label line, or the
line-terminator character.
.sp
.LE
The following are lower-level routines, which may be used to draw
graphs of many different kinds.  The EZ. . . routines call these.  They
are intended to be called by user programs, as well.
.BL
.LI
AGSTUP (XDRA,NVIX,IIVX,NEVX,IIEX,YDRA,NVIY,IIVY,NEVY,IIEY) \(em this
routine must be called prior to the first call to either of the
two routines AGBACK and AGCURV, to force the set-up of secondary
parameters controlling the behavior of those routines.  After any
parameter-setting call, AGSTUP must be called again before calling
either AGBACK or AGCURV again.  AGSTUP calls the routine "SET", in
the plot package, so that user x/y coordinates in subsequent calls
will map properly into the plotter space.
.LI
AGBACK \(em draws the background defined by the current state of the
AUTOGRAPH control parameters.
.LI
AGCURV (XVEC,IIEX,YVEC,IIEY,NEXY,KDSH) \(em draws the curve defined
by the arguments, positioning it as specified by the current state
of the AUTOGRAPH control parameters.
.sp
.LE
The following utility routines are called by the user.
.BL
.LI
AGSAVE (IFNO) \(em used to save the current state of AUTOGRAPH by
writing the appropriate information to a specified file.  Most
commonly used to save the default state for later restoration.
This routine should be used instead of AGGETP when the object
is to save the whole state of AUTOGRAPH, since it saves not only
the primary control parameters, but all of the character strings
pointed to by the primary control parameters.  It is the user's
responsibility to position the file 
.hw before
.ne 2
before calling AGSAVE.
.LI
AGRSTR (IFNO) \(em used to restore a saved state of AUTOGRAPH by
reading the appropriate information from a specified file.  Most
commonly used to restore AUTOGRAPH to its default state.  It is
the user's responsibility to position the file 
.ne 2
before calling AGRSTR.
.LI
AGBNCH (IDSH) \(em a function, of type CHARACTER*16 (it must be
declared as such in a user routine referencing it), whose value,
given a 16-bit binary dash pattern, is the equivalent character
dash pattern.
.LI
AGDSHN (IDSH) \(em a function, of type CHARACTER*16 (it must be
declared as such in a user routine referencing it), whose value,
given an integer "n" (typically between 1 and 26) is the character
string 'DASH/ARRAY/nnnn.', which is the name of the nth dash
pattern parameter.  To set the 13th dash pattern, for example,
one might use "CALL AGSETC (AGDSHN(13),'$$$$$$CURVE 13$$$$$$')".
.sp
.LE
The following utility routines are called by AUTOGRAPH.  The versions
included in AUTOGRAPH itself are dummies; they do nothing but RETURN.
The user may replace one or more of these routines with versions to
accomplish specific purposes.
.BL
.LI
AGUTOL (IAXS,FUNS,IDMA,VINP,VOTP) \(em called by AUTOGRAPH to perform
the mapping from user-system values along an axis to label-system
values along the axis and vice-versa.  This routine may be replaced
by the user to create a desired graph.
.LI
AGCHAX (IFLG,IAXS,IPRT,VILS) \(em called by AUTOGRAPH just before and
just after the various parts of the axes are drawn.
.LI
AGCHCU (IFLG,KDSH) \(em called by AUTOGRAPH just before and just after
each curve is drawn.
.LI
AGCHIL (IFLG,LBNM,LNNO) \(em called by AUTOGRAPH just before and just
after each line of an informational label is drawn.
.LI
AGCHNL (IAXS,VILS,CHRM,MCIM,NCIM,IPXM,CHRE,MCIE,NCIE) \(em called by
AUTOGRAPH just after the character strings defining a numeric label
have been generated.
.LE
.H 3 "Arguments"
In calls to the routines EZY, EZXY, EZMY, and EZMXY:
.BL
.LI
XDRA is an array of x coordinates, dimensioned as implied by the
current value of the AUTOGRAPH control parameter 'ROW.' (see the
description of the argument LROW, below).  The value of the
AUTOGRAPH parameter 'NULL/1.' (1.E36, by default) when used as an
x coordinate, implies a missing data point; the curve segments
on either side of such a point are not drawn.
.LI
YDRA is an array of y coordinates, dimensioned as implied by the
current value of the AUTOGRAPH control parameter 'ROW.' (see the
description of the argument LROW, below).  The value of the
AUTOGRAPH parameter 'NULL/1.' (1.E36, by default) when used as a
y coordinate, implies a missing data point; the curve segments
on either side of such a point are not drawn.
.LI
IDXY is the first dimension of the arrays XDRA (if it has two
dimensions) and YDRA.
.LI
MANY is the number of curves to be drawn by the call to EZ. . . \(em
normally, the second dimension of XDRA (if it has two dimensions)
and YDRA.
.LI
NPTS is the number of points defining each curve to be drawn by
the routine EZ. . . \(em normally, the first (or only) dimension of
XDRA and YDRA.
.LI
GLAB is a character constant or a character variable, defining a
label to be placed at the top of the graph.  The string may not be
more than 40 characters long \(em if it is fewer than 40 characters
long, its last character must be a dollar sign.  (The dollar sign
is not a part of the label \(em it is stripped off.)  The character
string "CHAR(0)" may be used to indicate that the previous label,
whatever it was, should continue to be used.  The initial graph
label consists of blanks.
.sp
.LE
In calls to the routine ANOTAT:
.BL
.LI
XLAB and YLAB resemble GLAB (see above) and define labels for the
x and y axes.  The default x-axis label is the single character
X, the default y-axis label the single character Y.  Note that one
may use the string "CHAR(0)" to indicate that the x-axis (y-axis)
label is not to be changed from what it was previously.
.LI
LBAC, if non-zero, specifies a new value for the AUTOGRAPH control
parameter 'BACKGROUND.', as follows:
.VL 1c
.LI "1"
a perimeter background
.LI "2"
a grid background
.LI "3"
an axis background
.LI "4"
no background
.sp
.LE
The default value of 'BACKGROUND.' is 1.
.LI
LSET, if non-zero, specifies a new value for the AUTOGRAPH control
parameter 'SET.'.  This parameter may be negated to suspend the
drawing of curves by the EZ. . . routines, so that a call to one of
them will produce only a background.  The absolute value of 'SET.'
affects the way in which AUTOGRAPH determines the position and
shape of the graph and the scaling of the axes, as follows:
.VL 1c
.LI "1"
Restores the default values of the AUTOGRAPH parameters
in question.  AUTOGRAPH will set up an appropriate call
to the plot-package routine "SET", over-riding any prior
call to that routine.
.LI "2"
Tells AUTOGRAPH to use arguments 1-4 and 9 of the last
"SET" call.  Arguments 1-4 specify where the graph should
fall on the plotter frame, argument 9 whether the graph
is linear/linear, linear/log, etc.
.bp
.LI "3"
Tells AUTOGRAPH to use arguments 5-8 and 9 of the last
"SET" call.  Arguments 5-8 specify the scaling of the
axes, argument 9 whether the graph is linear/linear,
linear/log, etc.
.LI "4"
A combination of 2 and 3.  Arguments 1-4 of the last "SET"
call specify the position, arguments 5-8 the scaling, and
argument 9 the linear/log nature, of the graph.
.sp
.LE
.LE
(The plot-package routine "SET" is described in the "SPPS"
section of this manual;
it is not a part of AUTOGRAPH.)
.sp
If the routine DISPLA is called with its argument LTYP non-zero,
the linear/log nature of the graph will be that specified by LTYP,
not that specified by the last "SET" call, no matter what the value
of the control parameter 'SET.'.
.sp
The default value of 'SET.' is 1.
.BL
.LI
NDSH, if non-zero, specifies a new value of the AUTOGRAPH control
parameter 'DASH/SELECTOR.' (and therefore a new set of dashed-line
patterns), as described below.  Note:  The default value of the
dashed-line parameters is such that all curves will be drawn using
solid lines; if that is what you want, use a zero for NDSH.
.VL 1c
.LI "~~"
If the value of 'DASH/SELECTOR.' is negative, curves produced
by subsequent calls to EZMY or EZMXY will be drawn using a
set of alphabetic dashed-line patterns.  The first curve drawn
by a given call will be labeled 'A', the second 'B', . . ., the
twenty-sixth 'Z', the twenty-seventh 'A' again, and so on.
Curves drawn by calls to EZY and EZXY will be unaffected.
.LI "~~"
If the value of 'DASH/SELECTOR.' is positive, it must be less
than or equal to 26.  The next argument, DSHL, is an array
containing NDSH dashed-line patterns.  All curves produced by
subsequent calls to EZY, EZXY, EZMY, and EZMXY will be drawn
using the dashed-line patterns in (DSHL(I),I=1,NDSH) \(em the
first curve produced by a given call will have the pattern
specified by DSHL(1), the second that specified by DSHL(2),
the third that specified by DSHL(3), . . . the NDSH+1st that
specified by DSHL(1), . . . etc.  Each element of DSHL must
be a character string, in which a dollar sign stands for a
solid-line segment, a quote stands for a gap, and other
characters stand for themselves.  See the write-up of the
package "DASHCHAR" in this manual.  Binary dashed-line patterns may not be
defined by means of a call to ANOTAT, only by means of calls
to lower-level routines.
.sp
.LE
.LI
DSHL (if NDSH is greater than zero) is an array of dashed-line
patterns, as described above.
.sp
.LE
In calls to the routine DISPLA:
.BL
.LI
LFRA, if non-zero, specifies a new value for the AUTOGRAPH control
parameter 'FRAME.'.  Possible values are as follows:
.VL 1c
.LI "1"
The EZ. . . routines do a frame advance after drawing.
.LI "2"
No frame advance is done by the EZ. . . routines.
.LI "3"
The EZ. . . routines do a frame advance before drawing.
.LE
.sp
The default value of 'FRAME.' is 1.
.LI
LROW, if non-zero, specifies a new value for the AUTOGRAPH control
parameter 'ROW.'.  This parameter tells AUTOGRAPH how the argument
arrays XDRA and YDRA, in calls to the routines EZMY and EZMXY, are
subscripted, as follows:
.VL 1c
.LI "~~"
If 'ROW.' is positive, this implies that the first subscript
of YDRA is a point number and the second subscript is a curve
number.  If 'ROW.' is negative, the order is reversed.
.LI "~~"
If the absolute value of 'ROW.' is 1, this implies that XDRA
is singly-subscripted, by point number only.  If the absolute
value of 'ROW.' is 2 or greater, this implies that XDRA is
doubly-subscripted, just like YDRA.
.LE
.sp
The default value of 'ROW.' is 1, specifying that XDRA is singly-
subscripted and that YDRA is doubly-subscripted by point number
and curve number, in that order.
.LI
LTYP, if non-zero, specifies new values for the AUTOGRAPH control
parameters 'X/LOGARITHMIC.' and 'Y/LOGARITHMIC.', which determine
whether the X and Y axes are linear or logarithmic.  Possible
values are as follows:
.VL 1c
.LI "1"
x axis linear, y axis linear
.LI "2"
x axis linear, y axis logarithmic
.LI "3"
x axis logarithmic, y axis linear
.LI "4"
x axis logarithmic, y axis logarithmic
.LE
.sp
The default values of these parameters make both axes linear.
.sp
If the parameters 'X/LOGARITHMIC.' and 'Y/LOGARITHMIC.' are reset
by the routine DISPLA, they are given values which make them
immune to being reset when 'SET.' = 2, 3, or 4 (see the discussion
of the argument LSET, above).
.LE
.sp
.hw AGSETC AGGETP AGGETF
In calls to the routines AGSETP, AGSETF, AGSETI AGSETC, AGGETP,
AGGETF, AGGETI, and AGGETC:
.BL
.LI
TPGN is a character string identifying a group of AUTOGRAPH
control parameters.  It is of the form 'K1/K2/K3/ . . . /Kn.'.
Each Ki is a keyword.  The keyword K1 specifies a group of control
parameters, K2 a subgroup of that group, K3 a subgroup of that
subgroup, etc.  See \fIAUTOGRAPH:  A Graphing Utility\fR 
.hw description
for a more complete description of these parameter-group names and
the ways in which they may be abbreviated.
.LI
FURA is an array, from which control-parameter values are to be
taken (the routine AGSETP) or into which they are to be stored
(the routine AGGETP).  Note that the array is real; all of the
AUTOGRAPH parameters are stored internally as reals.
.LI
LURA is the length of the user array FURA.
.LI
FUSR is a variable, from which a single control parameter value is
to be taken (the routine AGSETF) or in which it is to be returned
(the routine AGGETF).  Note that the variable is real.
.LI
IUSR is a variable, from which a single-control parameter value is
to be taken (the routine AGSETI) or in which it is to be returned
(the routine AGGETI).  Note that, since the control parameters are
stored internally as reals, each of the routines AGSETI and AGGETI
does a conversion \(em from integer to real or vice-versa.  Note also
that AGSETI and AGGETI should only be used for parameters which
have intrinsically integral values.
.LI
CUSR is a character variable from which a character string
is to
be taken (the routine AGSETC) or into which it is to be retrieved
(the routine AGGETC).  The control parameter affected by the call
contains the floating-point equivalent of an integer identifier
returned by the routine which stashes the character string and
tendered to the routine which retrieves it (sort of the automated
equivalent of a hat check).  Note that AGSETC and AGGETC should
only be used for parameters which intrinsically represent character
strings.
.sp
.LE
In calls to the routine AGSTUP:
.BL
.LI
XDRA is an array of x coordinates of user data \(em usually, but not
necessarily, the same data which will later be used in calls to
the routine AGCURV.
.LI
NVIX is the number of vectors of data in XDRA \(em if XDRA is 
doubly-dimensioned, NVIX would normally have the value of its second
dimension, if XDRA is singly-dimensioned, a 1.
.LI
IIVX is the index increment between vectors in XDRA \(em if XDRA is
doubly-dimensioned, IIVX would normally have the value of its
first dimension, if XDRA is singly-dimensioned, a dummy value.
.LI
NEVX is the number of elements in each data vector in XDRA \(em if
XDRA is doubly-dimensioned, NEVX would normally have the value of
its first dimension, if XDRA is singly-dimensioned, the value of
that single dimension.
.LI
IIEX is the index increment between elements of a data vector in
XDRA \(em normally a 1.
.LI
YDRA, NVIY, IIVY, NEVY, and IIEY are analogous to XDRA, NVIX,
IIVX, NEVX, and IIEX, but define y-coordinate data.
.sp
.LE
.sp
In calls to the routine AGCURV:
.BL
.LI
XVEC is a vector of x coordinate data.
.LI
IIEX is the index increment between elements in XVEC.  AGCURV will
use XVEC(1), XVEC(1+IIEX), XVEC(1+2*IIEX), etc.
.LI
YVEC is a vector of y coordinate data.
.LI
IIEY is the index increment between elements in YVEC.  AGCURV will
use YVEC(1), YVEC(1+IIEY), YVEC(1+2*IIEY), etc.
.LI
NEXY is the number of points defining the curve to be drawn.
.LI
KDSH is a dashed-line selector.  Possible values are as follows:
.VL 1c
.LI "~~"
If KDSH is zero, AUTOGRAPH will assume that the user has
called the routine DASHD (see the section DASHCHAR in this
manual) to define the dashed-line pattern to be used.
.LI "~~"
If KDSH is less than zero and has absolute value M, AUTOGRAPH
will use the Mth (modulo 26) alphabetic dashed-line pattern.
Each of these patterns defines a solid line interrupted every
so often by a letter of the alphabet.
.LI "~~"
If KDSH is greater than zero and has the value M, AUTOGRAPH
will use the Mth (modulo N) dashed-line pattern in the group
of N dashed-line patterns defined by the AUTOGRAPH control
parameters in the group named 'DASH/PATTERNS.'.  The default
values of these parameters specify solid lines.
.sp
.LE
In calls to the routines AGSAVE and AGRSTR:
.BL
.LI
IFNO is the unit number associated with a file to which a single
unformatted logical record of data is to be written, or from which
such a record is to be read, by AUTOGRAPH.  The file is not rewound
before being written or read; positioning it properly is the user's
responsibility.
.sp
.LE
In calls to the function AGBNCH:
.BL
.LI
IDSH is a 16-bit binary dash pattern, the character equivalent of
which is to be returned as the value of AGBNCH.
.LE
.sp
In calls to the function AGDSHN:
.BL
.LI
IDSH is the number of the dash pattern parameter whose name is to
be returned as the value of the function AGDSHN.
.LE
.sp
In calls to the routine AGUTOL:
.BL
.LI
IAXS is the number of the axis.  The values 1, 2, 3, and 4 imply
the left, right, bottom, and top axes, respectively.
.LI
FUNS is the value of the parameter 'AXIS/s/FUNCTION.' which may be
used to select the desired mapping function for axis IAXS.  It is
recommended that the default value (zero) be used to specify the
identity mapping.  A non-zero value may be integral (1., 2., etc.)
and serve purely to select the code to be executed or it may be the
value of a real parameter in the equations defining the mapping.
.LI
IDMA specifies the direction of the mapping.  A value greater than zero
indicates that VINP is a value in the user system and that VOTP is to
be a value in the label system, a value less than zero the opposite.
.LI
VINP is an input value in one coordinate system along the axis.
.LI
VOTP is an output value in the other coordinate system along the axis.
.sp
.LE
In calls to the routine AGCHAX:
.BL
.LI
IFLG is zero if a particular object is about to be drawn, non-zero
if it has just been drawn.
.LI
IAXS is the number of the axis being drawn.  The values 1, 2, 3,
and 4 indicate the left, right, bottom, and top axes, respectively.
.LI
IPRT indicates the part of the axis being drawn.  Possible values
are as follows:
.VL 1c
.LI "1"
implies the line of the axis.
.LI "2"
implies a major tick.
.LI "3"
implies a minor tick.
.LI "4"
implies the mantissa of a numeric label.
.LI "5"
implies the exponent of a numeric label.
.sp
.LE
.LI
VILS is the value in the label system at the point where the part
is being drawn.  For IPRT = 1, VILS is zero.
.sp
.LE
In calls to the routine AGCHCU:
.BL
.LI
IFLG is zero if a particular object is about to be drawn, non-zero
if it has just been drawn.
.LI
KDSH is the value with which AGCURV was called, as follows:
.LE
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
.nr 38 \wAGCURV called by
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wEZY
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wEZXY
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wEZMY
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wEZMXY
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wthe user program~~~~~~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wValue of KDSH
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w1
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w"n" or "-n", where n is the curve number
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w"n" or "-n", where n is the curve number
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wthe user value
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
.B
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'AGCURV called by\h'|\n(41u'Value of KDSH
.R
.sp
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'EZY\h'|\n(41u'1
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'EZXY\h'|\n(41u'1
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'EZMY\h'|\n(41u'"n" or "-n", where n is the curve number
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'EZMXY\h'|\n(41u'"n" or "-n", where n is the curve number
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'the user program~~~~~~~~~~\h'|\n(41u'the user value
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.sp
In calls to the routine AGCHIL:
.BL
.LI
IFLG is zero if a particular object is about to be drawn, non-zero
if it has just been drawn.
.LI
LBNM is a character variable containing the name of the label being drawn.
.LI
LNNO is the number of the line being drawn.
.sp
.LE
In calls to the routine AGCHNL:
.BL
.LI
IAXS is the number of the axis being drawn.  The values 1, 2, 3,
and 4 imply the left, right, bottom, and top axes, respectively.
.LI
VILS is the value to be represented by the numeric label, in the
label system for the axis.  The value of VILS must not be altered.
.LI
CHRM, on entry, is a character string containing the mantissa of
the numeric label, as it will appear if AGCHNL makes no changes.
If the numeric label includes a "times" symbol, it is represented
by a blank in CHRM.  (See IPXM, below.)  CHRM may be modified.
.LI
MCIM is the length of CHRM \(em the maximum number of characters that
it will hold.  The value of MCIM must not be altered.
.LI
NCIM, on entry, is the number of meaningful characters in CHRM.  If
CHRM is changed, NCIM should be changed accordingly.
.LI
IPXM, on entry, is zero if there is no "times" symbol in CHRM; if
it is non-zero, it is the index of a character position in CHRM.
If AGCHNL changes the position of the "times" symbol in CHRM,
removes it, or adds it, the value of IPXM must be changed.
.LI
CHRE, on entry, is a character string containing the exponent of
the numeric label, as it will appear if AGCHNL makes no changes.
CHRE may be modified.
.LI
MCIE is the length of CHRE \(em the maximum number of characters that
it will hold.  The value of MCIE must not be altered.
.LI
NCIE, on entry, is the number of meaningful characters in CHRE.  If
CHRE is changed, NCIE should be changed accordingly.
.LE
