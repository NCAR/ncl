.\"
.\"	$Id: pwrzi.m,v 1.1.1.1 1992-04-17 22:30:31 ncargd Exp $
.\"
.TH PWRZI 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " PWRZI - Plots characters in 3-d with ISOSRF
.nrsN 0
.tr ~
.pn 359
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9NAME\s11@"
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
SUBROUTINE PWRZI (X,Y,Z,ID,N,ISIZE,LINE,ITOP,ICNT)
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
PWRZI is a character-plotting routine for
plotting characters in three-space when using
ISOSRF.  For a large class of
possible positions, the hidden character
problem is solved.

PWRZI will not work with ISOSRFHR.

.H 3 "Usage"
CALL PWRZI (X,Y,Z,ID,N,ISIZE,LINE,ITOP,ICNT)
.sp
CALL PWRZI after calling
ISOSRF and before calling FRAME.
.H 3 "ARGUMENTS"
.H 3 "On Input"
.VL .6i
.LI "\fBX,Y,Z\fR"
Positioning coordinates for the characters
to be drawn.  These are floating point
numbers in the same three-space as used in
ISOSRF.
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
.LI "\fBLINE\fR"
The direction in which the characters are to
be written.
.na
.nf
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
given above).  Note that LINE cannot
equal ITOP even in absolute value.
.LI "\fBICNT\fR"
Centering option.
.nf
.na
-1~(X,Y,Z)  is the center of the left edge of the first character.
~0~(X,Y,Z)  is the center of the entire string.
~1~(X,Y,Z)  is the center of the right edge of the last character.
.fi
.ad
.H 3 "On Output"
All arguments are unchanged.
.H 3 "Note"
The hidden character problem is solved
correctly for characters near (but not inside)
the three-space object.
.H 3 "Entry Points"
PWRZI, INITZI, PWRZOI, PWRZGI
.H 3 "Common Blocks"
PWRZ1I,PWRZ2I
.H 3 "I/O"
Plots character(s)
.H 3 "Precision"
Single
.H 3 "Required Library Routines"
ISOSRF, the ERPRT77 package, and the SPPS
.H 3 "Required GKS Level"
0A
.H 3 "Language"
FORTRAN 77
.H 3 "History"
Implemented for use with ISOSRF
