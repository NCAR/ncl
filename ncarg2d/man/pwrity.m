.\"
.\"	$Id: pwrity.m,v 1.1.1.1 1992-04-17 22:30:31 ncargd Exp $
.\"
.TH PWRITY 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " PWRITY - character plotting
.nrsN 0
.tr ~
.pn 355
.bp
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9PWRITY\s11@"
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
SUBROUTINE PWRITY (X,Y,ID,N,ISIZE,ITHETA,ICNT)                            
.R
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
PWRITY is a character-plotting routine.  It has
some features not found in WTSTR, but is not as
fancy as PWRITX.
.H 3 "Usage"
CALL PWRITY(X,Y,ID,N,ISIZE,ITHETA,ICNT)
.H 3 "ARGUMENTS"
.H 3 "On Input"
.VL .8i
.LI "\fBX,Y\fR"
Positioning coordinates for the characters to
be drawn.  X and Y are user world coordinates
and are scaled according to the current
normalization transformation.  Also, see ICNT.
.LI "\fBID\fR"
Character string to be drawn.
.LI "\fBN\fR"
The number of characters in ID.
.LI "\fBISIZE\fR"
Size of the character:
.BL
.LI
If between 0 and 3, ISIZE is chosen as
1., 1.5, 2., or 3. times an 8 plotter
address character width.
.LI
If greater than 3, ISIZE is the character
width in plotter address units.
.LE
.LI "\fBITHETA\fR"
Angle, in degrees, at which the characters are
plotted (counterclockwise from the positive
X axis.)
.LI "\fBICNT\fR"
Centering option:
.nf
= -1  (X,Y) is the center of the left edge
of the first character.
=  0  (X,Y) is the center of the entire
string.
=  1  (X,Y) is the center of the right edge
of the last character.
.fi
.LE
.H 3 "On Output"
All arguments are unchanged.
.H 3 "Common Blocks"
PWRCOM
.H 3 "Required Library Routines"
SPPS
.H 3 "Required GKS Level"
0A
.H 3 "I/O"
Plots characters.
.H 3 "Precision"
Single
.H 3 "Language"
FORTRAN 77
.H 3 "History"
Implemented for use in DASHCHAR.
Made portable in January 1977
for use on computer systems that
support plotters with up to 15 bits resolution.
Converted to FORTRAN 77 and GKS in July 1984.
.H 3 "Algorithm"
Digitizations of the characters are stored
internally and adjusted according to X, Y,
ISIZE and ICNT, then plotted.
.H 3 "Timing"
Slower than WTSTR, faster than PWRITX.
.H 3 "Portability"
FORTRAN 77
