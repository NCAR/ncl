.\"
.\"	$Id: pwrzs.m,v 1.1.1.1 1992-04-17 22:30:31 ncargd Exp $
.\"
.TH PWRZS 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " PWRZS - Plots characters in 3-d with SRFACE
.nrsN 0
.tr ~
.pn 363
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9PWRZS\s11@"
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
SUBROUTINE PWRZS (X,Y,Z,ID,N,ISIZE,LINE,ITOP,ICNT)
.R
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
PWRZS is a character-plotting routine for
plotting characters in three-space when using
SRFACE.  For a large class of
possible positions, the hidden character
problem is solved.
.H 3 "Usage"
CALL PWRZS (X,Y,Z,ID,N,ISIZE,LINE,ITOP,ICNT)
.br
Use CALL PWRZS after calling
SRFACE and before calling FRAME.
Note:  SRFACE will have to be changed
to suppress the FRAME call.  See IFR
in SRFACE internal parameters.
.H 3 "ARGUMENTS"
.H 3 "On Input"
.VL .6i
.LI "\fBX,Y,Z\fR"^
Positioning coordinates for the characters
to be drawn.  These are floating point
numbers in the same three-space as used in
SRFACE.
.LI "\fBID\fR"
Character string to be drawn.  ID is of type
CHARACTER.
.LI "\fBN\fR"
The number of characters in ID.
.LI "\fBISIZE\fR"
Size of the character:
.BL
.LI
If between 0 and 3, ISIZE is 1., 1.5,
2., or 3. times a standard width equal
to 1/128th of the screen width.
.LI
If greater than 3, ISIZE is the character
width in plotter address units.
.LE
.LI "\fBLINE\fR"^
The direction in which the characters are to
be written.
.nf
.na
1 = +X     -1 = -X
2 = +Y     -2 = -Y
3 = +Z     -3 = -Z
.ad
.fi
.LI "\fBITOP\fR"
The direction from the center of the first
character to the top of the first
character (the potential values for
ITOP are the same as those for LINE as
given above.)  Note that LINE cannot
equal ITOP even in absolute value.
.LI "\fBICNT\fR"
Centering option.
.br
-1~(X,Y,Z)  is the center of the left edge of
the first character.
.br
~0~(X,Y,Z)  is the center of the entire
string.
.br
~1~(X,Y,Z)  is the center of the right edge
of the last character.
.H 3 "On Output"
All arguments are unchanged.
.H 3 "Note"
The hidden character problem is solved
correctly for characters near (but not inside)
the three-space object.
.H 3 "Entry Points"
PWRZS, INITZS, PWRZOS, PWRZGS
.H 3 "Common Blocks"
PWRZ1S,PWRZ2S
.H 3 "I/O"
Plots character(s)
.H 3 "Precision"
Single
.H 3 "Required Library Routines"
SRFACE, the ERPRT77 package, and the SPPS
.H 3 "Required GKS Level"
0A
.H 3 "Language"
FORTRAN 77
.H 3 "History"
Implemented for use with SRFACE.

