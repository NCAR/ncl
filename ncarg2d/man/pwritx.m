.\"
.\"	$Id: pwritx.m,v 1.1.1.1 1992-04-17 22:30:30 ncargd Exp $
.\"
.TH PWRITX 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " PWRITX - Fancy character plotting
.nrsN 0
.tr ~
.pn 347
.bp
.PH ""
.PF ""
.SK 1
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9PWRITX\s11@"
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
SUBROUTINE PWRITX (X,Y,IDPC,NCHAR,JSIZE,JOR,JCTR)
.fi
.ad
.R
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
PWRITX is a character plotting routine.  It
produces high quality characters for annotating
graphs, making movie titles, etc.
.H 3 "Usage"
CALL PWRITX (X,Y,IDPC,NCHAR,JSIZE,JOR,JCTR)
.H 3 "ARGUMENTS"
.H 3 "Common Block PUSER"
By default, PWRITX uses the complex character
set.  A complete list of the characters
contained in this font appears as the
output from the PWRITX demo driver in
the "Examples" section of this manual.
PWRITX can be made to
access one other font, called the duplex
character set.  The characters in the
duplex font are somewhat simpler in
appearance (no serifs, fewer curves, etc.),
but are still of high-quality.  Only the
alphanumeric characters in the duplex
font are different from the alphanumeric
characters in the complex font.  All other
characters remain the same.  To make
PWRITX access the duplex character set, the user
has to define in his main program a common block
PUSER containing 1 integer variable. If this
variable is initialized to 1 before the first
call to PWRITX, the duplex character set is
used by PWRITX.
.sp
.nf
For example:

     .
     .
     .

COMMON /PUSER/ MODE

     .
     .
     .

MODE = 1

     .
     .
     .

CALL PWRITX(X,Y,IDPC,NCHAR,JSIZE,JOR,JCTR)

     .
     .
     .
.sp
.fi
Note:  The character set cannot be changed after
the first call to PWRITX.  To produce
examples of the duplex character set,
run the demonstration driver for PWRITX (as supplied)
with MODE set to 1 as described above.
The demo driver for PWRITX also serves
for examples of PWRITX calls (particularly
the final plot.)
.H 3 "On Input"
.VL .9i
.LI "\fBX,Y\fR"
Positioning coordinates for the characters to
be drawn.  These are world coordinates.  See
JCTR.
.LI "\fBIDPC\fR"
Characters to be drawn and FUNCTION CODES
(see below.)   IDPC is TYPE CHARACTER.
.LI "\fBNCHAR\fR"
The number of characters in IDPC, including
characters to be drawn and function codes.
.LI "\fBJSIZE\fR"
Size of the character.
.BL
.LI
If between 0 and 3, it is 1., 1.5, 2.,
or 3. times digitized character width.
(See "Function Codes" below for these
sizes.)
.LI
If greater than 3, it is the desired plotter
address units for principal character
height, such as principal characters will be
JSIZE plotter address units high, and
indexical and cartographic characters will
be scaled proportionally, such that
.br
Indexical = (13/21)*JSIZE  PLA units
.br
Cartographic = (9/21)*JSIZE  PLA units high
.LE
.LI "\fBJOR\fR"
Character string orientation in degrees
counterclockwise from the positive X axis.
.LI "\fBJCTR\fR"
.nf
Centering option.
~~=~~0 (X,Y) is the center of the entire string.
~~=~-1 (X,Y) is the center of the left edge of the first character.
~~=~~1 (X,Y) is the center of the right edge of the last character.
.fi
.LE
.H 3 "On Output"
All arguments are unchanged.
.H 3 "Function Codes"
Function codes may be included in the character
string IDPC to change font, case, etc. within a
string of text.  All function instructions must
be enclosed in apostrophes.
No punctuation is needed between functions
except for a comma between adjacent numbers;
however, commas may be used between functions
to improve readability.  The following are the
only legal function codes.  Any other
characters in a function string will be ignored
except that an error message will be printed
and, if more than 10 errors occur within a
string, control will be returned to the main
program. At the first call to PWRITX, size, type
and case are Principal, Roman, and Upper.
.sp
.nf
PLA = plotter address (for resolution 10)
UPA = user plotter address (resolution as defined by user)
.sp
\fBFont Definitions\fR
.br
R  Roman type characters
G  Greek type characters
.fi
.sp
\fBSize Definitions\fR
.VL 3
.sp -.5
.LI "P"
Principal size, digitized to be 21 PLA
units high.  The total character
including white space is 32 PLA units
high.  A carriage return or a Y
increment will space down 32 PLA units.
A blank or an X increment will space
across 16 PLA units.  
Note:  Characters
vary in width.
.sp -.5
.LI "I"
Indexical size, digitized to be 13 PLA
units high (20 PLA units high
including white space).  A carriage
return or a Y increment is 20 PLA units.
Blanks or X increments are 12 PLA units.
.sp -.5
.LI "K"
Cartographic size, digitized to be 9 PLA
units high (14 PLA units high
including white space).  Carriage return
or Y increments are 14 PLA units. Blanks
or X increments are 8 PLA units.
.sp
.LE
\fBCase Definitions\fR
.br
U or Un.  Upper case
.VL 3
.sp -.5
.LI "~"
If U is followed by a number n (not
separated by a comma) then n characters
will be drawn in upper case, subsequent
characters will be in lower case.  (The
Un option is particularly useful for
capitalizing sentences.)
.LE
L or Ln.  Lower case
.VL 3
.sp -.5
.LI "~"
If L is followed by a number n, then n
characters will be drawn in lower case
and subsequent characters will be in
upper case.
.LE
.sp
\fBLevel Definitions\fR
.nf
S or Sn.  Superscript level.
B or Bn.  Subscript level.
N or Nn.  Normal level.
.fi
.sp
.VL 3
.sp -.5
.LI "~"
When super or subscripting, the
character size will change depending on
the previous character drawn.  Principal
base characters will be subscripted or
superscripted with indexical characters,
with a 10 PLA unit shift (scaled to
JSIZE) up or down.  Indexical and
cartographic base characters will be sub
or superscripted with cartographic
characters with a 7 PLA unit shift.

The case of the indexing characters will
generally be the same as that of the base
character unless otherwise specified.
Exception: a lower case indexical base
will be super or subscripted with upper
case cartographic, as the cartographic
type has no lower case alphabetic or
numeric characters available.

If S,B, or N is followed by a number n,
then n characters will be drawn as
specified above, after which character
size, case, and position will be reset to
that of the base character.
If n is negative, its absolute value
will be used instead (n cannot be 0.)

Do not overlap level definitions given
for a specified number of characters.
The N option returns character case and
size to that of the base but maintains
the current character position.
.bp
.sf
.na
.nf
Example:~~~'U1'T'S1'EST
~~~~~~~~~~~~~~~~~~~~~~~~~~~~E
~~~~~~~~~~~Will be written TST
~~~~~~~~~~~'U1'T'S'E'N'ST
~~~~~~~~~~~~~~~~~~~~~~~~~~~~E
~~~~~~~~~~~Will be written T ST
.ad
.fi
.ef
.sp
.LE
\fBCoordinate Definitions\fR (Descriptions assume normal UPA unit space.)
.br
H,Hn,HnQ.  Increment in the X direction.
.VL 3
.sp -.5
.LI "~"
If this option appears without a number
n, n will be taken to be 1.  Hn will
shift the present X position n UPA
units.  If n is positive, the shift is to
the right; if n is negative the shift is
to the left.  If Hn is followed by a Q,
the X position will be incremented by n
character widths (i.e., n blanks) either
right or left.
.LE
V,Vn,VnQ.  Increment in the Y direction.
.VL 3
.sp -.5
.LI "~"
If this option appears without a number
n, n will be taken to be 1.  Vn will
shift the present Y position n UPA
units.  If n is positive the shift is
up, if n is negative the shift is down.
If Vn is followed by a Q, the Y position
will be incremented by n lines up or
down.
.LE
X,Xn.  Set X.
.VL 3
.sp -.5
.LI "~"
If X appears without a number n, this
will act as a do-nothing statement.
Otherwise, the character position in the
X direction will be set to the UPA
coordinate n, so that the next character
drawn will be centered on n and
subsequent characters will be drawn from
this position.
.LE
Y,Yn.  Set Y.
.VL 3
.sp -.5
.LI "~"
This works the same as set X.
.LE
C  Carriage return.
.VL 3
.sp -.5
.LI "~"
A carriage return will be done before
the next character is plotted.
.LE
.sp
\fBDirection Definitions\fR
.br
D,Dn.  Write down, rather than across the frame.
.VL 3
.sp -.5
.LI "~"
If D appears without an n or if n=0, all
characters will be written down, until
an 'A' function is encountered.  If D is
followed by a number n, n characters
will be written down and subsequent
characters will be written across the
frame.
If n is negative, the absolute value of
n is used instead.
.LE
A  Write across.
.VL 3
.sp -.5
.LI "~"
Escape from the D option.
.LE
.sp
\fBDirect Character Access\fR
.nf
NNN.  Where NNN stands for a numeric character.
Character number NNN will be drawn.
NNN is base 8.
.fi
.H 3 "Note"
.BL
.LI
All characters in a given call are drawn in
the same intensity. If JSIZE .LE. 2,
characters are in low intensity, otherwise
they are in high intensity.  Return to the
main program is always in high intensity.
.LI
On other than the first entry to PWRITX,
font, case, etc. are in the state they
were in when the program exited from the
the last PWRITX call.
.LI
Font, size, and case are reset to previous
values if they were only set for a specified
number of characters in the previous call.
.LI
The previous case is always reset to upper
for each new call to PWRITX.
.LI
The direction is always reset to across
for each new call to PWRITX.
.LI
Numbers for direct character access must
not be signed. All other numbers can be
signed.
.LE
.H 3 "Portability"
FORTRAN 77
.H 3 "Required Resident Routines"
SQRT, SIN, COS.
.H 3 "Required GKS Level"
0A
.H 3 "Entry Points"
PWRITX, GTNUM, GTNUMB, XTCH, PWRXBD, GTSIGN,
GTDGTS, HTABLE, GETHOL, CCHECK, DCECK, PWRX
MKMSK
.H 3 "Common Blocks"
PWRSV1, PSAV1, PSAV2, PUSER, PINIT, PINIT1,
HOLTAB, PWRC0, PWRC1, PWRC2
.H 3 "Required Library"
The ERPRT77 package and the SPPS.
.H 3 "I/O"
Plots characters.
.H 3 "Precision"
Single
.H 3 "Language"
FORTRAN 77
.H 3 "History"
.BL
.LI
Originally implemented to make the Hershey
character sets more accessible.
.LI
Made portable in 1978
for use on all computer systems which support
plotters with up to 15 bit resolution.
.LI
Made to be FORTRAN 77 and GKS compatible,
August 1984.
.LE
.H 3 "Internal Parameters"
All values below are for plotter address units.
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
Shift in case of super or subscripting for principal characters.
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
Shift in case of super or subscripting for indexical characters.
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
.nr 38 \wSPRIH
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSPRIW
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSINDH
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSINDW
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSCARH
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSCARW
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSSPR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSSIC
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wDefault
.if \n(81<\n(38 .nr 81 \n(38
.nr 31 0
.nr 32 0
.nr 38 \w32
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w16
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w20
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w12
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w14
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w8
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w10
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w.
.if \n(32<\n(38 .nr 32 \n(38
.nr 38 \w7
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
.nr 38 \wHeight of principal characters.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wWidth of principal characters.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wHeight of indexical characters.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wWidth of indexical characters.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wHeight of cartographic characters.-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wWidth of cartographic characters.-\n(82-3n-\n(83-3n-\n(84
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
\&\h'|\n(40u'Name\h'|\n(41u'Default\h'|\n(42u'Function
.R
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\h'|\n(42u'
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SPRIH\h'|\n(41u'32.\h'|\n(42u'Height of principal characters.
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SPRIW\h'|\n(41u'16.\h'|\n(42u'Width of principal characters.
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SINDH\h'|\n(41u'20.\h'|\n(42u'Height of indexical characters.
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SINDW\h'|\n(41u'12.\h'|\n(42u'Width of indexical characters.
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SCARH\h'|\n(41u'14.\h'|\n(42u'Height of cartographic characters.
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SCARW\h'|\n(41u'8.\h'|\n(42u'Width of cartographic characters.
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SSPR\h'|\n(41u'10.\h'|\n(42u'
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
.ta \n(80u \n(61u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SSIC\h'|\n(41u'7.\h'|\n(42u'
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
