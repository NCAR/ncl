.\"
.\"	$Id: dashsmth.m,v 1.1.1.1 1992-04-17 22:30:21 ncargd Exp $
.\"
.TH DASHSMTH 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
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
.pn 189
.bp
.PH ""
.PF ""
.SK 1
.tr ~
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9DASHSMTH\s11@"
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
DASHSMTH  - Software dashed-line package with character capability and smoothing
.S 11
.R
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
DASHSMTH is a software dashed-line package with
smoothing capabilities.  DASHSMTH is DASHCHAR
with smoothing features added.
.H 3 "Usage"
First, either
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

LASTD is called only after the last
point of a line has been processed in VECTD.

The following may also be called, but no
smoothing will result:
.br
CALL LINED (XA,YA,XB,YB)
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
.VL 2i
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
DRAWPV, DASHBD
.H 3 "Common Blocks"
INTPR, DASHD1, DASHD2, DDFLAG, DCFLAG, DSAVE1,
DSAVE2, DSAVE3, DSAVE5, CFFLAG, SMFLAG, DFFLAG,
FDFLAG
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
Written in October 1973.
Made portable in September 1977 for use
with all machines that
support plotters with up to 15 bit resolution.
Converted to FORTRAN 77 and GKS in June 1984.
.H 3 "Algorithm"
Points for each line
segment are processed and passed to the
routines, KURV1S and KURV2S, which compute
splines under tension passing through these
points.  New points are generated between the
given points, resulting in smooth lines.
.H 3 "Accuracy"
Plus or minus .5 plotter address units per call.
There is no cumulative error.
.H 3 "Timing"
About three times as long as DASHCHAR.
.bp
.PH ""
.PF ""
.bp
