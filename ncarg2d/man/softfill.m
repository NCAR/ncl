.\"
.\"	$Id: softfill.m,v 1.1.1.1 1992-04-17 22:30:43 ncargd Exp $
.\"
.TH SOFTFILL 3NCARG "NOVEMBER 1989" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE3
.dsNA " SOFTFILL - Fill the area inside a polygon in various ways
.dsS1 " CALL SFSGFA (XRA,YRA,NRA,DST,NST,IND,NND,ICI) ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Fills a polygon defined by world coordinates, using ~~~~~~~~~~~~~~~~~~~~~~~~ ~~ solid color fill (if available on your graphics device) ~~~~~~~~~~~~~~~~~~ ~~~~ or with a suitable pattern fill, otherwise
.dsS2 " CALL SFWRLD (XRA,YRA,NRA,DST,NST,IND,NND) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Fills a polygon defined by world coordinates with ~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~ evenly spaced parallel lines or with rectangular ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~ patterns of dots or characters
.dsS3 " CALL SFNORM (XRA,YRA,NRA,DST,NST,IND,NND) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~ Fills a polygon defined by normalized device coordinates ~~~~~~~~~~~~~~~~~~~ ~~ with evenly spaced parallel lines or with rectangular ~~~~~~~~~~~~~~~~~~~~~~ ~~ patterns of dots or characters
.dsS4 " CALL SFGETC (CNP,CVP) Retrieves current character values
.dsS5 " CALL SFGETI (CNP,IVP) Retrieves current integer values
.dsS6 " CALL SFGETP (IDP) Retrieves current 8x8 dot pattern
.dsS7 " CALL SFGETR (CNP,RVP) Retrieves current real values
.dsS8 " CALL SFSETC (CNP,CVP) Sets character values
.dsS9 " CALL SFSETI (CNP,IVP) Sets integer values
.dss1 " CALL SFSETP (IDP) Sets 8x8 dot pattern
.dss2 " CALL SFSETR (CNP,RVP) Sets real values
.nrsN 11
.ds f. man/softfill.l

./" revised 9/20/89 to add index macro definition
./" revised 9/18/89 w/new headers, footers, L1 heads, *i
./" revised 6/13/89 to include tab macro .tA
./" revised 6/27/89 to include correct headers & footers
./" revised 6/29/89 to include macro for right-adjusted headings (cg)
.\" Index Entry Macro Definition
.de *X
.if \\nJ .tm .IE\tENTRY\t\\$1\t\\$2\t\\$3\t\\$4\t\\$5\t\\$6\t\\n%
..
.tr ~
.PH ""
.PF ""
.EF "@\s124-%  \s9Area Fill\fR@@NCAR Graphics Guide to New Utilities@"
.OF "@\s9Version 3.00, October 1989@@\s9Area Fill~~\s124-%\s9@"
.EH "@\s9SOFTFILL@@@"
./"  Headers & footers: replace chapter name, utility, and digit before the %
./"  Put odd header command after first page of text so it won't print on pg1;
./"  Odd header:  .OH "@@@\s9TITLE@"
.ll 6.5i
.\"   heading macros for Graphics, Version 3.00, written by Nancy Dawson, 5/89
.EQ         \"  Makes all equations be 12 point, Roman font
.S 99
.EN
.de tA
.sp
.ta 2i-1n   \"  tab setup for Type:  Default: line
Type:  \\$1\tDefault:  \\$2
.sp
..
.de >>       \"  display for indented lines
.sp
.nf
..           \"  subsequent lines are indented
.de <<       \"  end of display for indented lines
.fi
.sp
..           \"  subsequent lines are not indented
.de sF       \"  begin FORTRAN (constant spacing font)
.ps 10
.vs 12
.nf
.ft L
..
.de eF       \"  end FORTRAN (resume previous font & prev. size & font)
.ft
.fi
.ps
.vs
..
.ft B
.ps 18
.*i "AREA FILL"
.sp 4
.L1 "Chapter Overview"
With the advent of the new SOFTFILL utility, you now have two
ways to fill polygons: through calls to 
the GKS routine GFA or through calls to the SOFTFILL routines 
SFSGFA, SFWRLD, and SFNORM.  The SOFTFILL utility is the subject
of this chapter, and color examples of its use are provided.
.sp
Before you can use any of the area fill routines, however, you must 
define the polygons to be filled.
Some sets of polygons are easily defined.  For instance, you
could easily write a program to generate a checkerboard and 
fill each square-shaped polygon with the desired color.  Other
sets of polygons, such as ones that might represent surface temperatures
on the ocean, are more difficult to define.
There are two utilities in the NCAR Graphics package that allow
you to recover useful sets of polygons: CONPACK lets you
obtain polygons that represent contour bands;
EZMAPA lets you obtain polygons that represent geographical entities such
as continents, islands, oceans, lakes, states, and so forth.
Both of these utilities depend on the utility AREAS, which
provides a general way of obtaining polygons (areas) in the X-Y plane
from one or more groups of "edge lines."  Example 8 of the
CONPACK documentation (Chapter 3 in this manual) 
illustrates how to coordinate the use of CONPACK, EZMAPA, and AREAS
to produce a map of one hemisphere of the earth, with color
fill of contour bands over Africa.
.sp
Except for the SOFTFILL utility, which is documented in this
chapter, and the CONPACK utility, which is documented in
Chapter 3, all other utilities and subroutines 
mentioned in this introduction are documented in the
\fINCAR Graphics User's Guide, Version 2.00\fR.
.SK 1
\&
.ft B
.ps 16
.*i "SOFTFILL"
.sp 3.5
.OH "@@@\s9SOFTFILL@"
.L1 "SOFTFILL Introduction"
The graphics utility SOFTFILL contains the user-callable subroutines SFSGFA,
SFWRLD, SFNORM, SFSETC, SFSETI, SFSETP, SFSETR, SFGETC, SFGETI, SFGETP, and
SFGETR.  These subroutines fall into three categories: subroutines
for area fill, subroutines for setting internal parameter values,
and subroutines for retrieving internal parameter values.
.L2 "Subroutines for Area Fill"
SFSGFA, SFWRLD, and SFNORM are SOFTFILL's area-fill subroutines.
You define the boundary of a polygon to be filled; it may be an
uncomplicated polygon or a complicated one with a boundary that
crosses itself or that has "holes" to remain unfilled.  Areas
\fIinside\fR the polygon are filled; areas \fIoutside\fR
the polygon are not filled.  (A point is inside a polygon if
a straight line emanating from that point intersects the
polygon's boundary an odd number of times; otherwise, it is
outside the polygon.)
By slightly changing the definition of the polygon, you can
fill what was previously unfilled and vice versa.
.sp
The subroutines SFWRLD and SFNORM fill a polygon
with evenly spaced, solid, parallel lines; with evenly spaced, rectangular
patterns of dots; or with evenly spaced, rectangular patterns of single
characters.  You can use internal parameters to control the angle of 
fill; the spacing of the
lines, dots, or characters used for fill; the character to be used,
if any; and the pattern to be used for dot or character fill.
(See Example 1 output.)
.sp
The routine SFSGFA fills a polygon
in a number of ways: you can use it to do hardware-controlled, solid color 
fill, if that
capability is available on your graphics device, or to do
a suitable, software-controlled, pattern fill otherwise.  (SFSGFA 
does software-controlled pattern fill by calling SFWRLD and SFNORM.)
If you do all area fills with SFSGFA, then you can easily change the way
in which they are done by modifying the value of a single internal parameter.
(See Example 2 output.)
.sp
If you are doing solid fill with SFSGFA, whether boundary lines
are drawn or not depends on your hardware device.
Boundary lines are not drawn when doing software-controlled
pattern fill.
.L2 "Subroutines for Setting Internal Parameter Values"
Internal parameters of SOFTFILL (variables whose scope is limited to 
the subroutines in the utility SOFTFILL) 
determine how the filling is to be done.
You can use the subroutines with names of the form SFSET\fIx\fR to give
new values to internal parameters.
For example, by using
SFSETR to set the internal parameters \&'SP' and \&'AN', you can
specify the distance between adjacent lines ('SP') and 
the angle at which
the lines are to be drawn ('AN').
You can use SFSETI to set the internal
parameter \&'DO', which specifies whether the lines are to be solid
or dotted.  If lines are to be dotted, you can use SFSETC to set
the internal parameter \&'CH', which specifies whether each
"dot" is to be just a dot or a selected symbol, and use SFSETP to
define an 8x8 dot pattern.
.L2 "Subroutines for Retrieving Internal Parameter Values"
The subroutines with names of the form SFGET\fIx\fR are 
used to retrieve the 
current values of SOFTFILL's internal parameters.  This capability
is furnished for consistency with other utilities in the NCAR
Graphics package and also for programmers who build 
interfaces to SOFTFILL and need to save and restore the values of
its internal parameters.
.L2 "Terms You Need to Know"
The following terms are used in explaining the utility SOFTFILL:
.L5 "Inside vs. Outside:"
A point is \fIinside\fR a polygon if a straight line emanating from
that point intersects the polygon's boundary lines an odd number of
times; otherwise, it is \fIoutside\fR the polygon.
.L5 "World coordinate system:  "
A coordinate system, also called the user coordinate system, in
which the position of a point in the X-Y plane is specified by 
X and Y coordinates in ranges that are meaningful to the user.
.L5 "Normalized device coordinate system:  "
A coordinate system, also called the fractional coordinate
system, in which the position of a point on a virtual plotting
device is specified by X and Y coordinates between 0. and 1.,
inclusive.
You can control how normalized device coordinates are obtained from world
coordinates by using the SPPS subroutine SET. 
.br
.ne 3
.L5 "Color index:  "
A pointer to an entry in a color table where the
color is specified using RGB intensity values.
.L5 "GFA:  "
GFA (GKS Fill Area) is a GKS subroutine that fills a polygon. 
.L1 "SOFTFILL Calls"
.ne 6
.in 0
.ft B
.S 14
SFWRLD and SFNORM
.S 12
.L2 "Purpose"
The subroutines SFWRLD and SFNORM fill a polygon with evenly spaced,
solid,
parallel lines; with evenly spaced, rectangular patterns of dots; or with
evenly spaced, rectangular patterns of single characters.
SFWRLD transforms a polygon's world coordinates to normalized
device coordinates and calls SFNORM.  SFNORM, using normalized
device coordinates, does the actual area fill.
Both subroutines have the same argument list.
.sp
.in -.5i
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
.nr 80 0
.nr 38 \w\s11CALL SFWRLD\fR\s12~~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\s11CALL SFNORM\fR\s12~~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(XRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~YRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~DST,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NST,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~IND,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NND)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(XRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~YRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~DST,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NST,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~IND,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NND)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fB
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNRA
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNRA
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNST
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNND
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wDimension\fB
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNRA
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNRA
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNST
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNND
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.if t .if \n(TW>\n(.li .tm Table at line 225 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fB
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\s11CALL SFWRLD\fR\s12~~~~~~\h'|\n(41u'(XRA,\h'|\n(42u'Real Array\h'|\n(43u'Input/Output\h'|\n(44u'NRA
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~YRA,\h'|\n(42u'Real Array\h'|\n(43u'Input/Output\h'|\n(44u'NRA
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NRA,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~DST,\h'|\n(42u'Real Array\h'|\n(43u'Input/Output\h'|\n(44u'NST
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NST,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~IND,\h'|\n(42u'Integer Array\h'|\n(43u'Input/Output\h'|\n(44u'NND
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NND)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.sp
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fB
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\s11CALL SFNORM\fR\s12~~~~~~\h'|\n(41u'(XRA,\h'|\n(42u'Real Array\h'|\n(43u'Input\h'|\n(44u'NRA
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~YRA,\h'|\n(42u'Real Array\h'|\n(43u'Input\h'|\n(44u'NRA
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NRA,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~DST,\h'|\n(42u'Real Array\h'|\n(43u'Input/Output\h'|\n(44u'NST
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NST,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~IND,\h'|\n(42u'Integer Array\h'|\n(43u'Input/Output\h'|\n(44u'NND
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NND)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.sp
.in +.5i
.VL 1.2i
.LI "\fBXRA\fR"
A real array containing the X coordinates of the points that define the
boundary of the polygon to be filled.
Use the world coordinate system when calling SFWRLD and the
normalized device coordinate system when calling SFNORM.
After a call to SFWRLD, this array contains
the X coordinates transformed to the normalized device coordinate system
and can be used with SFNORM.
.sp
You do not need to repeat point 1 at the end of the
boundary to close the polygon.  Other subroutines may require
this; for example, if you call CURVE to draw the boundary, you
will have to repeat the first point in that call.
.sp
.ne 3
Dimension this array to be at least as large as NRA (the number of
points defining the area to be filled).
.sp
.LI "\fBYRA\fR"
A real array containing the Y coordinates of the points that
define the boundary of the polygon to be filled.
Use the world coordinate system
when calling SFWRLD and the normalized device coordinate system
when calling SFNORM.  After a call to SFWRLD, this
array contains the Y coordinates transformed to the normalized device
coordinate system and can be used with SFNORM.
.sp
Dimension this array to be the same size as XRA.
.sp
.LI "\fBNRA\fR"
The number of points defining the area to be filled.  NRA must have
a value greater than 2.
.sp
.ne 3
.LI "\fBDST\fR"
A real scratch array, dimensioned NST (the next argument).
.sp
.LI "\fBNST\fR"
The dimension of DST.  Make NST greater than or 
equal to NRA+NIM, where NRA is the number of points defining the area to be 
filled, and NIM is the largest number of intersection points of any 
fill line with the boundary lines.
To be sure DST is dimensioned large enough, you can use NIM=NRA; in practice, 
NIM rarely needs to be that large.
For example, NIM=2 is sufficient for an octagon.
.sp
.LI "\fBIND\fR"
An integer scratch array, dimensioned NND (the next argument).
.sp
.LI "\fBNND\fR"
The dimension of IND.  Make NND greater than or equal 
to NRA+2*NIM, where NRA is the number of points defining the area 
to be filled, and NIM is the number of intersection points of 
any fill line with the
boundary lines.  To be sure IND is dimensioned large
enough, you can use NIM=NRA; in practice NIM rarely needs to be
that large.
.L2 "Usage"
Both SFWRLD and SFNORM fill the area defined by the points
(XRA(I),YRA(I)), for I from 1 to NRA.
The lines connecting point 1 to point 2, point 2 to point 3, ..., point NRA-1
to point NRA, and point NRA to point 1 bound the area to be filled.
The default values of SOFTFILL's internal parameters cause
fill to be done with solid, horizontal
lines .00125 normalized-device-coordinate units apart.
.sp
.ne 6
Use SFWRLD if the arrays XRA and YRA contain world coordinates.
Use SFNORM if XRA and YRA contain normalized device coordinates.
Since SFWRLD transforms XRA and YRA from world coordinates to
normalized device coordinates, any subsequent calls with those
arrays should be to SFNORM (for example, to create a
cross-hatched effect). 
.L3 "Drawing Boundaries"
SFWRLD and SFNORM do not draw the polygon boundary itself;
to do this, use the SPPS routines CURVE, PLOTIT, or PLOTIF.
.nh "PLOTIT"
Both CURVE and PLOTIT expect world coordinates.  After the first
call to SFWRLD, the arrays XRA and YRA contain normalized
device coordinates.
So, if you want to draw your boundaries after
doing area fill, either save copies of
XRA and YRA prior to the SFWRLD call, convert their contents back to 
world coordinates by using the SPPS conversion routines CFUX and CFUY,
or use PLOTIF. 
.L3 "Contents of the Arrays XRA and YRA"
For SFWRLD and SFNORM to fill an uncomplicated polygon (one without holes),
XRA and YRA should
contain the world coordinates of the polygon's vertices
in the order in which they are encountered as the boundary of the polygon is
traced.
.sp
To leave an unfilled hole in a polygon, do the following:  (1) add the 
vertices of the hole, in the proper order, to XRA and YRA;
(2) repeat the first vertex of the hole to close it; 
and (3) repeat the last vertex of the outer polygon boundary 
to tie the first point of the hole to the last point of the 
polygon's outer boundary.
.sp
To fill what was unfilled and vice versa, do the following:
(1) add the four coordinates of the 
frame corners; (2) repeat the coordinates of the first corner
of the frame; and (3) repeat the final point of the original
polygon.  In effect, this makes what was previously inside,
outside, and what was previously outside, inside.
Consecutive calls, using different fill angles, may be used to create a
cross-hatched effect.
.in 0
.ft B
.S 14
SFSGFA
.S 12
.L2 "Purpose"
The subroutine SFSGFA (which stands for "SOFTFILL - Simulate GFA") 
fills a user-defined polygon.
It provides a way to use hardware-controlled solid color fill,
if that capability is available on your graphics device, 
or a suitable software-controlled
pattern-fill substitute, otherwise.
(Software-controlled pattern fill is done by calling SFWRLD
and/or SFNORM.)  Using SFSGFA gives you the advantage of being
able to easily modify the way areas are filled \(em you need 
to change the value of only a single internal parameter ('TY').
.sp
.in -.5i
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
.nr 80 0
.nr 38 \w\s11CALL SFSGFA\fR\s12~~~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(XRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~YRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NRA,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~DST,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NST,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~IND,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~NND,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~ICI)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput/Output
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fB
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNRA
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNRA
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNST
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \wNND
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.if t .if \n(TW>\n(.li .tm Table at line 367 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fB
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\s11CALL SFSGFA\fR\s12~~~~~~~\h'|\n(41u'(XRA,\h'|\n(42u'Real Array\h'|\n(43u'Input/Output\h'|\n(44u'NRA
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~YRA,\h'|\n(42u'Real Array\h'|\n(43u'Input/Output\h'|\n(44u'NRA
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NRA,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~DST,\h'|\n(42u'Real Array\h'|\n(43u'Input/Output\h'|\n(44u'NST
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NST,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~IND,\h'|\n(42u'Integer Array\h'|\n(43u'Input/Output\h'|\n(44u'NND
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~NND,\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~ICI)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.in +.5i
.VL 1.2i
.LI "\fBXRA\fR"
A real array containing the X coordinates 
of the points that define the
boundary of the polygon to be filled.
Use the world coordinate system for input values.
After a call to SFSGFA, this array contains
the X coordinates transformed to the normalized 
device coordinate system.
.sp
You do not need to repeat point 1 at the end of the
boundary to close the polygon.  Other subroutines may require
this; for example, if you call CURVE to draw the boundary, you
will have to repeat the first point in that call.
.sp
Dimension this array to be at least as large as NRA (the number of
points defining the area to be filled).
.sp
.LI "\fBYRA\fR"
A real array containing the Y coordinates of the points that
define the boundary of the polygon to be filled.
Use the world coordinate system
for input values.  After a call to SFSGFA, this
array contains the Y coordinates transformed to the normalized device
coordinate system.
.sp
Dimension this array to be the same size as XRA.
.sp
.LI "\fBNRA\fR"
The number of points defining the area to be filled.  NRA must have
a value greater than 2.
.sp
.ne 3
.LI "\fBDST\fR"
A real scratch array, dimensioned NST (the next argument).
This array is used only when SFSGFA makes calls to SFWRLD and/or SFNORM
to generate fill lines.
.sp
.LI "\fBNST\fR"
The dimension of DST.  Make NST greater than or 
equal to NRA+NIM, where NRA is the number of points 
defining the area to be 
filled, and NIM is the largest number of intersection points of any 
fill line with the boundary lines.
To be sure DST is dimensioned large enough, 
you can use NIM=NRA; in practice, 
NIM rarely needs to be that large.
For example, NIM=2 is sufficient for an octagon.
.sp
.LI "\fBIND\fR"
An integer scratch array, dimensioned NND (the next argument).
This array is used only when SFSGFA makes calls to SFWRLD and/or 
SFNORM to generate fill lines.
.sp
.LI "\fBNND\fR"
The dimension of IND.  Make NND greater than or equal 
to NRA+2*NIM, where NRA is the number of points defining the area 
to be filled, and NIM is the number of intersection points of 
any fill line with the
boundary lines.  To be sure IND is dimensioned large
enough, you can use NIM=NRA; in practice NIM rarely needs to be
that large.
.sp
.LI "\fBICI\fR"
A fill area color index to be used when the internal parameter \&'TY' 
has the default value zero.  ICI is either a polyline color index
or a pattern index when the internal parameter \&'TY' has a value
other than zero.
The value of the internal parameter \&'TY' directly affects the
function of ICI.  See "How Fill is Done: ICI and Type of Fill
('TY')" below for information about the values to use for ICI.
.L2 "Usage"
SFSGFA fills the area defined by the points (XRA(I),YRA(I)), for I
from 1 to NRA.
The lines connecting point 1 to point 2, point 2 to point 3, . . ., 
point NRA-1 to point NRA, and point NRA to point 1 bound the area 
to be filled.
.br
.ne 6
.L3 "How Fill is Done:  ICI and Type of Fill (\&'TY\&')"
The values of the internal parameter \&'TY' (for \&'TYPE OF FILL') and the
argument ICI determine how the fill is done.
The function of ICI changes depending on
the value of \&'TY'.  ICI can determine the fill area color index, the
polyline color index, or the density of the fill
pattern.
.L4 "\&'TY' = 0"
When \&'TY' has the value 0 (the default), SFSGFA does 
color fill by calling GFA.
GFA does either hollow, solid, or
pattern fill.  Hollow fill (only boundaries are drawn) 
is GFA's default, but you can change the
type of fill by calling the GKS subroutine GSFAIS.
Notice that one of the first steps in the code for Example 2 
is to force solid fill by calling GSFAIS with the argument "1".
.sp 
ICI, if zero or greater, specifies the color index
of the fill area.
A negative value of ICI specifies that the fill area color
index is not to be set before calling GFA;
the last call to the GKS subroutine GSFACI then determines
the fill area color index. 
.L4 "\&'TY' = 1"
When \&'TY' has the value 1, SFSGFA fills the area with
parallel lines by calling SFWRLD.  ICI, if zero or
greater, specifies the 
polyline color index.
A negative value of ICI specifies that the
polyline color index is not to be set before calling SFWRLD;
the last call to the GKS subroutine GSPLCI determines 
the polyline color index.
.sp
Since SFWRLD is being called, you can use the
internal parameters \&'AN', \&'CH', \&'DO', and \&'SP' 
to further affect the nature of the fill.
.sp
\fBNote:\fR  If \&'CH' and \&'DO' are set to select dot fill
or character fill, the values of ICI will not affect the color of
the dots or characters.  The intended use of \&'TY' > 0 is to
do color fill using colored lines; no provision is made for the
use of colored dots or colored characters.  (The current values of the
polymarker and text color indices are used to determine the
color.)
.L4 "\&'TY' = 2"
When \&'TY' has the value 2, SFSGFA calls SFWRLD 
to fill the area with parallel lines
and calls SFNORM to fill the area again with parallel
lines perpendicular to the first set.
ICI, if zero or greater, specifies the polyline color index.  A negative
value of ICI specifies that the polyline color index is not to be set
before calling SFWRLD;
the last call to the GKS subroutine GSPLCI determines the
polyline color index.
.sp
As with \&'TY'=1, you can use 
the internal parameters \&'AN', \&'CH', \&'DO', and \&'SP' to
further affect the nature of the fill.  See the note above about ICI's
function with \&'CH' and \&'DO'.
.br
.ne 11
.L4 "\&'TY' = -4, -3, -2, or -1"
When \&'TY' has a negative value in the range from -4 to -1, SFSGFA fills
the area with line patterns by calling SFWRLD and/or SFNORM.
The absolute value of \&'TY' determines the
maximum number of fill-line angles used in a pattern.  ICI determines the
density of the lines drawn at each angle.
.sp
\fBAngles Used:\fR
When \&'TY' has one of the following values, fill is done
using lines drawn at the angles shown ('AN' is an internal
parameter that specifies an angle in degrees):
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
.nr 38 \w\fB\&'TY'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-3
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-4
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wAngles Used (in Degrees)\fR
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w'AN'
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w'AN', \&'AN'+90
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w'AN', \&'AN'+60, \&'AN'+120
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w'AN', \&'AN'+45, \&'AN'+90, \&'AN'+135
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
.if t .if \n(TW>\n(.li .tm Table at line 531 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\&'TY'\h'|\n(41u'Angles Used (in Degrees)\fR
.br
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-1\h'|\n(41u''AN'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-2\h'|\n(41u''AN', \&'AN'+90
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-3\h'|\n(41u''AN', \&'AN'+60, \&'AN'+120
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-4\h'|\n(41u''AN', \&'AN'+45, \&'AN'+90, \&'AN'+135
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-10
.sp
\fBLine Density:\fR
ICI is used to select the density of the lines in each direction.
A zero or negative value
of ICI selects a blank pattern.  Positive values of ICI select
patterns that increase in density as the value of ICI increases.
(Example 2 illustrates the changes in density as ICI increases
from 0 to 15.)
The largest usable value of ICI is approximately 
5*ABS('TY'); beyond that, the
pattern becomes essentially solid.  For example, if \&'TY' is -4,
20 is about the largest value of ICI that you can use and still
see a pattern.
.sp
For each increase in ICI, 
fill lines are added at one of the usable angles.  The first time
lines are added at a given angle, they are spaced 32*'SP' units
apart.  (The default value of the internal parameter \&'SP' 
is .00125.)  After the first time, each time lines are added at a
given angle, they are added between the existing lines so that
the distance between lines at that angle is halved.
An ICI value that is evenly divisible by the absolute
value of \&'TY' yields a pattern that is evenly dense at all
angles.
For example, if \&'TY' has the value -2, the patterns associated
with the first three values of ICI are formed as follows:  ICI=1 uses 
lines at the
angle \&'AN', spaced 32*'SP' units apart; ICI=2 uses lines at the
angles \&'AN' and \&'AN'+90, both spaced 32*'SP' units apart; ICI=3
uses lines at the angle \&'AN', spaced 32*'SP' units apart, and
lines at the angle \&'AN'+90, spaced 16*'SP' units apart.
.L4 "Types of Fill Not Mentioned"
Values of \&'TY' not specifically mentioned are reserved for future use.
At the moment, values greater than 2 are treated as equal to 2, and
values less than -4 are treated as equal to -4, but this may not be true
in the future.
.L3 "Drawing Boundaries"
SFSGFA does not draw the polygon boundary itself; to do this, use
the SPPS routines CURVE, PLOTIT, or PLOTIF.
Both CURVE and PLOTIT expect world coordinates.  After the first
call to SFSGFA, the arrays XRA and YRA contain normalized
device coordinates. So, if you want to draw your boundaries after
doing area fill, either save copies of
XRA and YRA prior to the SFSGFA call, convert their contents back to 
world coordinates by using the SPPS conversion routines CFUX and CFUY,
or use PLOTIF. 
.L3 "Contents of the Arrays XRA and YRA"
For SFSGFA to fill an uncomplicated polygon (one without holes),
XRA and YRA should
contain the world coordinates of the polygon's vertices
in the order in which they are encountered as the 
boundary of the polygon is traced.
.sp
To leave an unfilled hole in a polygon, do the following:  (1) add the 
vertices of the hole, in the proper order, to XRA and YRA;
(2) repeat the first vertex of the hole to close it; 
(3) repeat the last vertex
of the outer polygon boundary to tie the first point
of the hole to the last point of the polygon's outer boundary.
.sp
To fill what was unfilled and vice versa, do the following:  (1) add the 
four coordinates of the frame corners; (2) repeat the coordinates of the 
first corner of the frame; (3) repeat the final point of the original
polygon.  In effect, this makes what was previously inside,
outside, and what was previously outside, inside.
.L3 "Solid Fill of Polygons with Holes"
When a polygon contains holes, there are connecting lines between
the outer boundary of the polygon and the boundaries of the
holes.  When doing software fill (internal parameter \&'TY'NE  0), these
connecting lines cause no trouble; however, when doing solid
fill (internal parameter \&'TY'= 0), the hardware fill algorithms
will frequently display unfortunate edge effects along such
lines.  You can minimize these effects by using only horizontal
or vertical connecting lines and by ensuring they do not cross
any of the original boundary lines.
.L1 "SOFTFILL Parameter Access Subroutines"
There are five internal parameters that you can access to
further control area fill.  There are four subroutines to
set the value of internal parameters \(em one for each
type of parameter: character, integer, real, and integer
array.  Likewise, there are four subroutines to
retrieve the current value of internal parameters.  These
eight subroutines are described below, followed by a
description of each of the five internal parameters.
.L2 "Setting Internal Parameter Values"
Use the following subroutines to reset the current 
values of the five internal parameters \&'AN', \&'CH', \&'DO', \&'SP', 
and \&'TY'.
Call these subroutines before the call that you want to be
affected.  Changes remain in effect until you change them with
another call to one of these subroutines.
.sp
Only the first two characters of the internal parameter name 
are examined by these subroutines.
It is recommended that the rest of the character string be 
used to improve the readability of your code.
For example, instead of just \&'CH', use \&'CH - CHARACTER SPECIFIER'.
.sp
The subroutine SFSETI is passed an integer value \fIi\fR.
If you are setting an integer parameter, it
receives the value \fIi\fR.  If you are setting
a real parameter, it receives the value REAL(\fIi\fR).
.sp
The subroutine SFSETR is passed a real value \fIr\fR.  If
you are setting a real parameter, it
receives the value \fIr\fR.  If you are setting
an integer parameter, it
receives the value INT(\fIr\fR).  Note that the Fortran
intrinsic INT does truncation rather than rounding.
.sp
Thus, the subroutine SFSETR is the more general routine; it
provides access to all real and integer parameters.  SFSETI 
allows for more natural access to integer parameters
and to those real parameters whose values have no fractional
part.
.sp 2
.in -.5i
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
.nr 80 0
.nr 38 \w\fB\s11CALL SFSETC\fR\s12~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~CVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.if t .if \n(TW>\n(.li .tm Table at line 656 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SFSETC\fR\s12~~~~\h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~CVAL)\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character.  (\&'CH' is the only
internal parameter that has a character value.)
.LI "\fBCVAL\fR"
The character you select for the parameter.
.LE
.in 0
.sp 2
.ne 7
.in -.5i
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
.nr 80 0
.nr 38 \w\fB\s11CALL SFSETI\fR\s12~~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~IVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.if t .if \n(TW>\n(.li .tm Table at line 676 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SFSETI\fR\s12~~~~~~\h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~IVAL)\h'|\n(42u'Integer\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example, \&'AN').
.LI "\fBIVAL\fR"
The integer value you select for the parameter.
.LE
.in 0
.sp 2
.in -.5i
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
.nr 80 0
.nr 38 \w\fB\s11CALL SFSETR\fR\s12~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~RVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.if t .if \n(TW>\n(.li .tm Table at line 694 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SFSETR\fR\s12~~~~~\h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~RVAL)\h'|\n(42u'Real\h'|\n(43u'Input\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example, \&'SP').
.LI "\fBRVAL\fR"
The real value you select for the parameter.
.LE
.L2 "Setting the Dot Pattern"
Use the following subroutine to reset the current
contents of SOFTFILL's 8x8 internal parameter array that
specifies the dot pattern.  This array is used only when dot
fill is selected ('DO' NE  0) or when character fill is selected
('DO' NE  0 and
.br
\&'CH' NE  0).
.sp
.in -.5i
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
.nr 80 0
.nr 38 \w\fB\s11CALL SFSETP\fR\s12~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(IDP)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w8,8
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.if t .if \n(TW>\n(.li .tm Table at line 718 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SFSETP\fR\s12~~~~~\h'|\n(41u'(IDP)\h'|\n(42u'Integer Array\h'|\n(43u'Input\h'|\n(44u'8,8
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.sp
.VL 1.2i
.LI "\fBIDP\fR"
An 8x8 integer array of 0s and 1s 
specifying the dot pattern to be
transferred to an internal array for use by 
SFWRLD and SFNORM (SFWRLD and SFNORM do not directly use IDP).
Each dot is associated with a particular
element of IDP.
If that element is a 1, the dot is drawn; if it is a 0, the
dot is not drawn.
The association is done in such a way as to replicate the
pattern across the entire filled area.
.sp
The default dot pattern consists of all 1s.
.sp
When dot fill is selected, each fill line is drawn using 
dots. Dots along a fill line are the same distance apart as the
lines are from each other and fall at the 
intersection points of a square grid.
.sp
.ne 3
If the fill angle is 0 (the default), 
then incrementing the first subscript
of IDP corresponds to a horizontal motion 
across the plot from left to right
and incrementing the second subscript of IDP 
corresponds to a vertical motion
across the plot from top to bottom.
This allows the contents of IDP to be
declared in a DATA statement that creates a 
"picture" of the pattern.
(A fill angle other than zero yields the same pattern rotated by
that angle.)  For example,
.sp
.sF
 DATA IDP / 0,0,0,0,0,0,0,0,
+           0,1,1,1,1,1,1,0,
+           0,1,0,0,0,0,0,1,
+           0,1,0,0,0,0,0,1,
+           0,1,1,1,1,1,1,0,
+           0,1,0,0,0,1,0,0,
+           0,1,0,0,0,0,1,0,
+           0,1,0,0,0,0,0,1/
.eF
.sp
creates the letter "R", with the correct orientation.
The new dot pattern is used until a subsequent call 
to SFSETP again changes the contents of the internal array.
.L2 "Retrieving Current Internal Parameter Values"
If you need to recover the current value of one of the
internal parameters, use one of these calls.  Only the first two
characters of the internal parameter name are examined by these
subroutines.  It is recommended that the rest of the character
string be used to improve the readability of your code.  For
example, instead of just \&'AN', use \&'AN - ANGLE OF FILL'.
.sp
The subroutine SFGETI returns an integer value.
If you are getting the value of an integer parameter, SFGETI
returns that integer value.  If you are getting the value of a
real parameter with value \fIr\fR,
then the value returned is INT(\fIr\fR).  Note
that the Fortran intrinsic INT does truncation rather than
rounding.
.sp
The subroutine SFGETR returns a real value.  If
you are getting the value of a real parameter, SFGETR returns that real
value.  If you are getting the value of an integer parameter with
value \fIi\fR,
then the value returned is REAL(\fIi\fR).
.sp
Thus, the subroutine SFGETR is the more general routine; it
provides access to all real and integer parameters.  SFGETI 
allows for more natural access to integer parameters
and to those real parameters whose values have no fractional
part.
.sp 2
.in -.5i
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
.nr 80 0
.nr 38 \w\fB\s11CALL SFGETC\fR\s12~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~CVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.if t .if \n(TW>\n(.li .tm Table at line 805 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SFGETC\fR\s12~~~~\h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~CVAL)\h'|\n(42u'Character\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character.  (\&'CH' is the only
internal parameter that has a character value.)
.LI "\fBCVAL\fR"
A character variable in which the current value of the parameter
is to be returned.
.LE
.in 0
.sp 2
.ne 10
.in -.5i
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
.nr 80 0
.nr 38 \w\fB\s11CALL SFGETI\fR\s12~~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~IVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.if t .if \n(TW>\n(.li .tm Table at line 826 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SFGETI\fR\s12~~~~~\h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~IVAL)\h'|\n(42u'Integer\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example, \&'AN').
.LI "\fBIVAL\fR"
An integer variable in which the current value of the parameter
is to be returned.
.LE
.in 0
.sp 2
.in -.5i
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
.nr 80 0
.nr 38 \w\fB\s11CALL SFGETR\fR\s12~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(PNAM,
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w~RVAL)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCharacter
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wReal
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wOutput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.if t .if \n(TW>\n(.li .tm Table at line 845 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SFGETR\fR\s12~~~~\h'|\n(41u'(PNAM,\h'|\n(42u'Character\h'|\n(43u'Input\h'|\n(44u'
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'~RVAL)\h'|\n(42u'Real\h'|\n(43u'Output\h'|\n(44u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-7
.in +.5i
.VL 1.2i
.LI "\fBPNAM\fR"
The internal parameter name, of type character (for example, \&'SP').
.LI "\fBRVAL\fR"
A real variable in which the current value of the parameter is to
be returned.
.LE
.L2 "Retrieving the Dot Pattern"
Use the following subroutine to retrieve the current
contents of SOFTFILL's 8x8 internal parameter array that
specifies the dot pattern.
.sp
.in -.5i
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
.nr 80 0
.nr 38 \w\fB\s11CALL SFGETP\fR\s12~~~~
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \w\fB~Argument
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \w(IDP)
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wType
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wInteger Array
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 83 0
.nr 38 \wMode
.if \n(83<\n(38 .nr 83 \n(38
.nr 38 \wInput
.if \n(83<\n(38 .nr 83 \n(38
.83
.rm 83
.nr 84 0
.nr 38 \wDimension\fR
.if \n(84<\n(38 .nr 84 \n(38
.nr 38 \w8,8
.if \n(84<\n(38 .nr 84 \n(38
.84
.rm 84
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
.if t .if \n(TW>\n(.li .tm Table at line 866 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'\fB~Argument\h'|\n(42u'Type\h'|\n(43u'Mode\h'|\n(44u'Dimension\fR
.br
.ta \n(80u \n(81u \n(82u \n(83u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fB\s11CALL SFGETP\fR\s12~~~~\h'|\n(41u'(IDP)\h'|\n(42u'Integer Array\h'|\n(43u'Input\h'|\n(44u'8,8
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-6
.in +.5i
.sp
.VL 1.2i
.LI "\fBIDP\fR"
An 8x8 integer array in which the dot pattern is to be
returned.  (See the subroutine SFSETP above for further details
about IDP.)
.LE
.L1 "SOFTFILL Internal Parameter Descriptions"
Internal parameters of SOFTFILL are variables whose scope is limited to the
subroutines in the SOFTFILL utility.  They determine how area
fill is to be done.
This section details the use of each of the five internal
parameters available in SOFTFILL.  
.sp
.VL 1.2i
.LI "\fB\&'AN'\fR"
The angle, in degrees, at which fill lines are to be drawn.  Use
either SFSETI or SFSETR to change the value of \&'AN'.
.tA "Integer or Real" "0"
.sp
.LI "\fB\&'CH'\fR"
The character selector, which only becomes important
when you are doing dot fill (when \&'DO' is non-zero).  The character
you select will be drawn in place of each dot.  Use SFSETC to
give a character value to \&'CH'; use SFSETI or SFSETR to give an 
integer value to \&'CH'.
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
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The fill character is a dot.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The fill character is the one specified by CHAR(\fIn\fR), 
where \fIn\fR is the positive integer you specify.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 81
.br
.di c+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The fill character is a GKS polymarker of type ABS(\fIn\fR), where
\fIn\fR is the negative integer you specify.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 81
.br
.di d+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
The fill character is the ASCII character that you specify.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue/Range
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fIn\fR>0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fIn\fR<0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fICharacter\fR
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(d-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 917 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBValue/Range\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fIn\fR>0\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fIn\fR<0\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fICharacter\fR\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.d+
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-22
.tA "Integer or" "0"
~~~~~~~~~Character
.sp
.fi
.LI "\fB\&'DO'\fR"
The dot-fill selector.  The value 0 means solid
lines are to be used (no dots); in this case, any fill character
set by \&'CH' is ignored.  The value 1 means dotted lines are to 
be used; dots are arranged according to the pattern specified by 
an 8x8 internal parameter array \(em see the description 
of the routine SFSETP for details.
Use SFSETI or SFSETR to change the value of \&'DO'.
.tA "Integer" "0"
.sp
.LI "\fB\&'SP'\fR"
The distance between each fill line and the next, and, if dot fill
is selected, the distance
between any pair of adjacent dots along a fill line.
The real value is given in
the normalized device coordinate system and represents a fraction of the
distance across the plotter frame, in the range 0. to 1.  A value
outside the range 0. to 1. has the same effect as the default (.00125).
Use SFSETR to change the value of \&'SP'.
.tA "Real" ".00125"
.sp
.LI "\fB\&'TY'\fR"
The type of fill to be done by the routine SFSGFA.  See the
section in the SFSGFA documentation titled "How Fill is Done: ICI
and Type of Fill ('TY')" for a detailed explanation of \&'TY'.
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
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Fills the inside of a polygon area by calling the GKS
subroutine GFA.  GFA's
default is hollow fill \(em only boundary lines are
drawn.  To force GFA to do solid fill, call GSFAIS with the argument "1" 
prior to calling SFSGFA.
.br
.di
.nr a| \n(dn
.nr a- \n(dl
..
.ec \
.eo
.am 81
.br
.di b+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Fills the inside of a polygon with parallel lines at the
angle \&'AN', spaced \&'SP' units apart.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 81
.br
.di c+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Fills the inside of a polygon with parallel lines at the
angle \&'AN' and fills it again with
parallel lines at the angle \&'AN'+90;  all lines are spaced \&'SP'
units apart.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.eo
.am 81
.br
.di d+
.35
.ft \n(.f
.ll 3.7in
.if \n(.l<\n(81 .ll \n(81u
.in 0
Fills the inside of a polygon with line patterns drawn at up to
ABS('TY') angles.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBValue/Range
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w0
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w2
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w-4 LE  \&'TY' LE  -1
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 38 .9in
.if \n(80<\n(38 .nr 80 \n(38
.nr 81 0
.nr 38 \wDescription\fR
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 38 \n(a-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(b-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(c-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(d-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 3.7in
.if \n(81<\n(38 .nr 81 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr TW \n(81
.if t .if \n(TW>\n(.li .tm Table at line 977 file man/softfill.l is too wide - \n(TW units
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBValue/Range\h'|\n(41u'Description\fR
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'0\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.a+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.b+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.c+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'-4 LE  \&'TY' LE  -1\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.d+
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-29
.tA "Integer" "0"
./".bp
./".L1 "SOFTFILL Examples \(em Output"
./".sp -2
./".L2 ""
./".sp -1i
./"./" the next line commented out to put in Dave's better copy
./"./".MF softfill.example.1.metacode 1 7.5
./".SK 1
./".L2 ""
./".rs
./".sp 20
./".S 18
./"\fBReplace this page with a color plate\fR
./".S 12
./".MF softfill.example.2.metacode 1 3.0
./".SK 1
./".L2 ""
./".sp 20
./".S 18
./"\fBReplace this page with a color plate\fR
./".S 12
./".MF softfill.meta 1 3.0
.SK 1
.L1 "SOFTFILL Examples \(em Code"
.L2 "Example 1"
.in -.5i
This example shows how to use the basic subroutines SFWRLD and
SFNORM and the parameter access subroutines to produce various
kinds of fill patterns.
.nf
.sF
      PROGRAM TEST
C
C Declare required dimensioned arrays.
C
      DIMENSION XRA(200),YRA(200),DST(220),IND(240)
      DIMENSION ID1(8,8),ID2(8,8),ID3(8,8),ID4(8,8)
C
C Define four different dot patterns.
C
      DATA ID1 / 1,1,0,0,0,0,1,1,
     +           1,1,0,1,1,0,1,1,
     +           0,0,0,1,1,0,0,0,
     +           0,1,1,1,1,1,1,0,
     +           0,1,1,1,1,1,1,0,
     +           0,0,0,1,1,0,0,0,
     +           1,1,0,1,1,0,1,1,
     +           1,1,0,0,0,0,1,1/
      DATA ID2 / 0,0,0,0,0,0,0,0,
     +           0,1,1,1,1,1,1,0,
     +           0,1,1,1,1,1,1,0,
     +           0,1,1,0,0,1,1,0,
     +           0,1,1,0,0,1,1,0,
     +           0,1,1,1,1,1,1,0,
     +           0,1,1,1,1,1,1,0,
     +           0,0,0,0,0,0,0,0/
      DATA ID3 / 0,0,0,0,0,0,0,0,
     +           0,0,0,0,1,0,0,0,
     +           0,0,0,1,1,1,0,0,
     +           0,1,0,0,1,0,0,1,
     +           0,0,1,1,1,1,1,0,
     +           0,0,0,0,1,0,0,0,
     +           0,0,0,1,0,1,0,0,
     +           0,1,1,0,0,0,1,1/
      DATA ID4 / 0,0,0,0,0,0,0,0,
     +           0,1,1,0,0,1,1,1,
     +           0,1,1,0,0,1,1,0,
     +           0,1,1,0,1,1,0,0,
     +           0,1,1,1,1,0,0,0,
     +           0,1,1,0,1,1,0,0,
     +           0,1,1,0,0,1,1,0,
     +           0,1,1,0,0,1,1,1/
C
C Open GKS.
C
      CALL OPNGKS
C
C Double the size of the GKS dot.
C
      CALL GSMKSC (2.)
C
C This code creates a single frame showing nine circles filled in
C various ways.  The DO-loop variable I says which row of circles
C we're working on (1 => top, 2 => middle, 3 => bottom).  The
C DO-loop variable J says which column of circles we're working
C on (1 => left, 2 => center, 3 => right).  The variable K gives
C the number of the circle currently being drawn and is used in
C a computed GO TO to determine which block of code is executed.
C
      DO 111 I=1,3
        YCN=REAL(4-I)
        DO 110 J=1,3
          XCN=REAL(J)
          K=3*(I-1)+J
          DO 100 L=1,101
            XRA(L)=XCN+.48*SIN(.062831853071796*REAL(L))
            YRA(L)=YCN+.48*COS(.062831853071796*REAL(L))
  100     CONTINUE
C
C Draw the circle.
C
          CALL SET (0.,1.,0.,1.,0.,4.,0.,4.,1)
          CALL CURVE (XRA,YRA,101)
C
C Jump to the proper piece of code to fill the circle.
C
          GO TO (101,102,103,104,105,106,107,108,109) , K
C
C Fill the first circle with horizontal lines.
C
  101     CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the second circle in the same way, but add a diamond-shaped
C hole.
C
  102     XRA(101)=2.00
          YRA(101)=3.24
          XRA(102)=1.76
          YRA(102)=3.00
          XRA(103)=2.00
          YRA(103)=2.76
          XRA(104)=2.24
          YRA(104)=3.00
          XRA(105)=XRA(101)
          YRA(105)=YRA(101)
          XRA(106)=XRA(100)
          YRA(106)=YRA(100)
          CALL SFWRLD (XRA,YRA,106,DST,110,IND,114)
          GO TO 110
C
C Fill the third circle with lines in two different directions to
C create a cross-hatched effect and create a more complicated hole.
C
  103     XRA(101)=XRA( 40)
          YRA(101)=YRA( 40)
          XRA(102)=XRA( 80)
          YRA(102)=YRA( 80)
          XRA(103)=XRA( 20)
          YRA(103)=YRA( 20)
          XRA(104)=XRA( 60)
          YRA(104)=YRA( 60)
          XRA(105)=XRA(100)
          YRA(105)=YRA(100)
          CALL SFSETR ('SP - SPACING OF FILL LINES',.009)
          CALL SFSETI ('AN - ANGLE OF FILL LINES',45)
          CALL SFWRLD (XRA,YRA,105,DST,111,IND,117)
          CALL SFSETI ('AN - ANGLE OF FILL LINES',135)
          CALL SFNORM (XRA,YRA,105,DST,111,IND,117)
          GO TO 110
C
C Fill the fourth circle with the default dot pattern, increasing the
C inter-dot spacing considerably.
C
  104     CALL SFSETR ('SP - SPACING OF FILL LINES',.005)
          CALL SFSETI ('AN - ANGLE OF FILL LINES',0)
          CALL SFSETI ('DO - DOT-FILL FLAG',1)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the fifth circle with a combination of lines and dots.
C
  105     CALL SFSETR ('SP - SPACING OF FILL LINES',.012)
          CALL SFSETI ('DO - DOT-FILL FLAG',0)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          CALL SFSETR ('SP - SPACING OF FILL LINES',.006)
          CALL SFSETI ('DO - DOT-FILL FLAG',1)
          CALL SFNORM (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the sixth circle with a specified dot pattern.
C
  106     CALL SFSETR ('SP - SPACING OF FILL LINES',.004)
          CALL SFSETP (ID1)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the seventh circle with a different dot pattern, tilted at an
C angle.
C
  107     CALL SFSETI ('AN - ANGLE OF FILL LINES',45)
          CALL SFSETP (ID2)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the eighth circle with a different dot pattern, using characters.
C
  108     CALL GSCHH  (.004)
          CALL SFSETR ('SP - SPACING OF FILL LINES',.006)
          CALL SFSETI ('AN - ANGLE OF FILL LINES',0)
          CALL SFSETC ('CH - CHARACTER SPECIFIER','O')
          CALL SFSETP (ID3)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
C Fill the last circle with K's, both large and small.
C
  109     CALL GSCHH  (.008)
          CALL SFSETR ('SP - SPACING OF FILL LINES',.012)
          CALL SFSETC ('CH - CHARACTER SPECIFIER','K')
          CALL SFSETP (ID4)
          CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
          GO TO 110
C
  110   CONTINUE
C
  111 CONTINUE
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL CLSGKS
C
C Done.
C
      STOP
C
      END
.eF
.fi
.ad
.L2 "Example 2"
.in -.5i
This example shows how to use the subroutine SFSGFA.
.sp
.nf
.sF
      PROGRAM TEST
C
C Declare required dimensioned arrays.
C
        DIMENSION XCS(101),YCS(101),XRA(100),YRA(100),DST(102),IND(104)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IAS(13)
C
C Define a character variable for use in labelling the plot.
C
        CHARACTER*2 LBL
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IAS / 13*1 /
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IAS)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Define color indices.
C
        CALL DFCLRS
C
C Define the window and viewport to make it easy to do seven rows
C containing sixteen ovals apiece.
C
        CALL SET (0.,1.,0.,1.,-1.,16.5,-.5,7.5,1)
C
C For each of the possible values of the internal parameter \&'TY', fill
C a set of circles, one for each of sixteen values of ICI (0-15).
C
C
        DO 103 ITY=-4,2
C
          YCN=REAL(3-ITY)
C
          WRITE (LBL,'(I2)') ITY
          CALL PLCHHQ (CFUX(CUFX(.5)-.006),YCN,LBL,.012,0.,1.)
C
          CALL SFSETI ('TYPE OF FILL',ITY)
C
          DO 102 ICI=0,15
C
            XCN=REAL(ICI+1)
C
            DO 101 L=1,101
              XCS(L)=XCN+.48*SIN(.062831853071796*REAL(L))
              YCS(L)=YCN+.48*COS(.062831853071796*REAL(L))
              IF (L.LT.101) THEN
                XRA(L)=XCS(L)
                YRA(L)=YCS(L)
              END IF
  101       CONTINUE
C
            CALL SFSGFA (XRA,YRA,100,DST,102,IND,104,ICI)
C
            CALL CURVE (XCS,YCS,101)
C
  102     CONTINUE
C
  103   CONTINUE
C
C Finish the labelling.
C
        CALL PLCHHQ (CFUX(CUFX(.5)-.060),4.,'"TYPE OF FILL"',
     +                                                      .012,90.,0.)
C
        DO 104 ICI=0,15
          XCN=REAL(ICI+1)
          IF (ICI.LT.10) THEN
            WRITE (LBL,'(I1)') ICI
            CALL PLCHHQ (XCN,CFUY(CUFY(.5)-.024),LBL(1:1),.012,0.,0.)
          ELSE
            WRITE (LBL,'(I2)') ICI
            CALL PLCHHQ (XCN,CFUY(CUFY(.5)-.024),LBL(1:2),.012,0.,0.)
          END IF
  104   CONTINUE
C
        CALL PLCHHQ (8.5,CFUY(CUFY(.5)-.060),'"COLOR INDEX"',.012,0.,0.)
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL CLSGKS
C
C Done.
C
      STOP
C
      END
      SUBROUTINE DFCLRS
C
C Define a set of RGB color triples for colors 1 through 15.
C
        DIMENSION RGBV(3,15)
C
C Define the RGB color triples needed below.
C
        DATA RGBV / 1.00 , 1.00 , 1.00 ,
     +              0.70 , 0.70 , 0.70 ,
     +              0.75 , 0.50 , 1.00 ,
     +              0.50 , 0.00 , 1.00 ,
     +              0.00 , 0.00 , 1.00 ,
     +              0.00 , 0.50 , 1.00 ,
     +              0.00 , 1.00 , 1.00 ,
     +              0.00 , 1.00 , 0.60 ,
     +              0.00 , 1.00 , 0.00 ,
     +              0.70 , 1.00 , 0.00 ,
     +              1.00 , 1.00 , 0.00 ,
     +              1.00 , 0.75 , 0.00 ,
     +              1.00 , 0.38 , 0.38 ,
     +              1.00 , 0.00 , 0.38 ,
     +              1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
        CALL GSCR (1,0,0.,0.,0.)
C
        DO 101 I=1,15
          CALL GSCR (1,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
.eF
.fi
.ad
.L2 "Example 3"
.in -.5i
Note:  The user will have to provide a driver similar to DSOFTF below in
order to utilize test subroutine TSOFTF, which is on the distribution tape.
.sp
.nf
.sF
      PROGRAM DSOFTF
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL TSOFTF (IER)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END


=========================================================================
=========================================================================
 
      SUBROUTINE TSOFTF (IERR)
C
C PURPOSE               To provide a simple demonstration of software
C                       fill of polygons using the SOFTFILL package.
C
C USAGE                 CALL TSOFTF (IERR)
C
C ARGUMENTS (OUTPUT)    IERR
C
C                         An integer variable
C                         = 0, if the test was successful
C                         = 1, otherwise
C
C I/O                   If the test is successful, the message "SOFTFILL
C                       TEST EXECUTED--SEE PLOT TO CERTIFY" is printed
C                       on unit 6.  In addition, a frame is produced on
C                       the graphics device.  In order to determine if
C                       the test was successful, it is necessary to
C                       examine this frame.
C
C PRECISION             Single.
C
C LANGUAGE              FORTRAN 77.
C
C REQUIRED ROUTINES     SOFTFILL package.
C
C REQUIRED GKS LEVEL    0A.
C
C ALGORITHM             Nine circles are filled in various ways, using
C                       routines in the package.
C
C
C Declare required dimensioned arrays.
C
        DIMENSION XRA(200),YRA(200),DST(220),IND(240),XSV(101),YSV(101)
        DIMENSION ID1(8,8),ID2(8,8),ID3(8,8),ID4(8,8)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IAS(13)
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IAS / 13*1 /
C
C Define four different dot patterns.
C
        DATA ID1 / 1,1,0,0,0,0,1,1,
     +             1,1,0,1,1,0,1,1,
     +             0,0,0,1,1,0,0,0,
     +             0,1,1,1,1,1,1,0,
     +             0,1,1,1,1,1,1,0,
     +             0,0,0,1,1,0,0,0,
     +             1,1,0,1,1,0,1,1,
     +             1,1,0,0,0,0,1,1/
        DATA ID2 / 0,0,0,0,0,0,0,0,
     +             0,1,1,1,1,1,1,0,
     +             0,1,1,1,1,1,1,0,
     +             0,1,1,0,0,1,1,0,
     +             0,1,1,0,0,1,1,0,
     +             0,1,1,1,1,1,1,0,
     +             0,1,1,1,1,1,1,0,
     +             0,0,0,0,0,0,0,0/
        DATA ID3 / 0,0,0,0,0,0,0,0,
     +             0,0,0,0,1,0,0,0,
     +             0,0,0,1,1,1,0,0,
     +             0,1,0,0,1,0,0,1,
     +             0,0,1,1,1,1,1,0,
     +             0,0,0,0,1,0,0,0,
     +             0,0,0,1,0,1,0,0,
     +             0,1,1,0,0,0,1,1/
        DATA ID4 / 0,0,0,0,0,0,0,0,
     +             0,1,1,0,0,1,1,1,
     +             0,1,1,0,0,1,1,0,
     +             0,1,1,0,1,1,0,0,
     +             0,1,1,1,1,0,0,0,
     +             0,1,1,0,1,1,0,0,
     +             0,1,1,0,0,1,1,0,
     +             0,1,1,0,0,1,1,1/
C
C Initialize the error parameter.
C
        IERR=0
C
C Double the size of the GKS dot.
C
        CALL GSMKSC (2.)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IAS)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Define a bunch of color indices.
C
        CALL SFCLRS
C
C Do a set call allowing us to use X and Y coordinates in the range
C from 0 to 4.
C
        CALL SET (0.,1.,0.,1.,0.,4.,0.,4.,1)
C
C Put a label at the top of the frame.
C
        CALL WTSTR (2.,3.75,'DEMONSTRATION PLOT FOR SOFTFILL',2,0,0)
C
C The following code creates a single frame showing nine circles filled
C in various ways.  The DO-loop variable I says which row of circles
C we're working on (1 => top, 2 => middle, 3 => bottom).  The DO-loop
C variable J says which column of circles we're working C on (1 =>
C left, 2 => center, 3 => right).  The variable K gives the number of
C the circle currently being drawn and is used in a computed GO TO to
C determine which block of code is executed.
C
        DO 112 I=1,3
C
          YCN=REAL(4-I)
C
          DO 111 J=1,3
C
            XCN=REAL(J)
C
            K=3*(I-1)+J
C
C Generate the coordinates defining the circle.  Two sets of arrays are
C used, one set for use in calling the fill routines, which transform
C the contents of the arrays, and one set for use in drawing the circle.
C
            DO 100 L=1,101
              XRA(L)=XCN+.48*SIN(.062831853071796*REAL(L))
              XSV(L)=XRA(L)
              YRA(L)=YCN+.48*COS(.062831853071796*REAL(L))
              YSV(L)=YRA(L)
  100       CONTINUE
C
C Jump to the proper piece of code to fill the circle.
C
            GO TO (101,102,103,104,105,106,107,108,109) , K
C
C Fill the first circle in color number 5.
C
  101       CALL SFSETI ('TYPE OF FILL',0)
            CALL SFSGFA (XRA,YRA,100,DST,102,IND,104,5)
            GO TO 110
C
C Add a diamond-shaped hole to the circle and fill it in color number
C 9, using lines in two directions to effect the fill.
C
  102       XRA(101)=2.00
            YRA(101)=3.24
            XRA(102)=1.76
            YRA(102)=3.00
            XRA(103)=2.00
            YRA(103)=2.76
            XRA(104)=2.24
            YRA(104)=3.00
            XRA(105)=XRA(101)
            YRA(105)=YRA(101)
            XRA(106)=XRA(100)
            YRA(106)=YRA(100)
            CALL SFSETI ('TYPE OF FILL',2)
            CALL SFSGFA (XRA,YRA,106,DST,110,IND,114,9)
            GO TO 110
C
C Create a more complicated hole in the third circle and fill it with
C pattern number 11 of the 20 or so that can be created using \&'TY'=-4.
C
  103       XRA(101)=XRA( 40)
            YRA(101)=YRA( 40)
            XRA(102)=XRA( 80)
            YRA(102)=YRA( 80)
            XRA(103)=XRA( 20)
            YRA(103)=YRA( 20)
            XRA(104)=XRA( 60)
            YRA(104)=YRA( 60)
            XRA(105)=XRA(100)
            YRA(105)=YRA(100)
            CALL SFSETI ('TYPE OF FILL',-4)
            CALL SFSGFA (XRA,YRA,105,DST,111,IND,117,11)
            CALL SFSETI ('ANGLE OF FILL LINES',15)
            GO TO 110
C
C Fill the fourth circle with the default dot pattern, increasing the
C inter-dot spacing considerably.
C
  104       CALL SFSETR ('SPACING OF FILL LINES',.005)
            CALL SFSETI ('ANGLE OF FILL LINES',0)
            CALL SFSETI ('DOT-FILL FLAG',1)
            CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
            GO TO 110
C
C Fill the fifth circle with a combination of lines and dots.
C
  105       CALL SFSETR ('SPACING OF FILL LINES',.012)
            CALL SFSETI ('DOT-FILL FLAG',0)
            CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
            CALL SFSETR ('SPACING OF FILL LINES',.006)
            CALL SFSETI ('DOT-FILL FLAG',1)
            CALL SFNORM (XRA,YRA,100,DST,102,IND,104)
            GO TO 110
C
C Fill the sixth circle with a specified dot pattern.
C
  106       CALL SFSETR ('SPACING OF FILL LINES',.004)
            CALL SFSETP (ID1)
            CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
            GO TO 110
C
C Fill the seventh circle with a different dot pattern, tilted at an
C angle.
C
  107       CALL SFSETI ('ANGLE OF FILL LINES',45)
            CALL SFSETP (ID2)
            CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
            GO TO 110
C
C Fill the eighth circle with a different dot pattern, using characters.
C
  108       CALL GSCHH  (.004)
            CALL SFSETR ('SPACING OF FILL LINES',.006)
            CALL SFSETI ('ANGLE OF FILL LINES',0)
            CALL SFSETC ('CHARACTER SPECIFIER','O')
            CALL SFSETP (ID3)
            CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
            GO TO 110
C
C Fill the last circle with K's, both large and small.
C
  109       CALL GSCHH  (.008)
            CALL SFSETR ('SPACING OF FILL LINES',.012)
            CALL SFSETC ('CHARACTER SPECIFIER','K')
            CALL SFSETP (ID4)
            CALL SFWRLD (XRA,YRA,100,DST,102,IND,104)
            GO TO 110
C
C Draw the circle.
C
  110       CALL CURVE (XSV,YSV,101)
C
  111     CONTINUE
C
  112   CONTINUE
C
C Advance the frame.
C
        CALL FRAME
C
C Log execution message and return to caller.
C
        WRITE (6,1001)
        RETURN
C
 1001   FORMAT (' SOFTFILL TEST EXECUTED--SEE PLOT TO CERTIFY')
C
      END
      SUBROUTINE SFCLRS
C
C Define a set of RGB color triples for colors 1 through 15.
C
        DIMENSION RGBV(3,15)
C
C Define the RGB color triples needed below.
C
        DATA RGBV / 1.00 , 1.00 , 1.00 ,
     +              0.70 , 0.70 , 0.70 ,
     +              0.75 , 0.50 , 1.00 ,
     +              0.50 , 0.00 , 1.00 ,
     +              0.00 , 0.00 , 1.00 ,
     +              0.00 , 0.50 , 1.00 ,
     +              0.00 , 1.00 , 1.00 ,
     +              0.00 , 1.00 , 0.60 ,
     +              0.00 , 1.00 , 0.00 ,
     +              0.70 , 1.00 , 0.00 ,
     +              1.00 , 1.00 , 0.00 ,
     +              1.00 , 0.75 , 0.00 ,
     +              1.00 , 0.38 , 0.38 ,
     +              1.00 , 0.00 , 0.38 ,
     +              1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
        CALL GSCR (1,0,0.,0.,0.)
C
        DO 101 I=1,15
          CALL GSCR (1,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
.eF
.fi
.L1 "SOFTFILL Errors"
SOFTFILL uses the subroutine SETER in the
error handling package ERPRT77 to generate error messages.
(See Appendix A for more information about the NCAR Graphics Error
Handling Package.)  All SOFTFILL error messages indicate 
a fatal error \(em no recovery is possible.   
.sp
The following error messages may be written by SOFTFILL.
Where \fIx\fR appears, it will
be replaced by the offending parameter name.
.sp
SFSGFA - ERROR EXIT FROM GQFACI
GKS is probably closed.  Check that you have opened GKS
with the SPPS subroutine OPNGKS.
.sp
SFSGFA - ERROR EXIT FROM GQPLCI
GKS is probably closed.  Check that you have opened GKS
with the SPPS subroutine OPNGKS.
.sp
SFNORM - COORDINATE ARRAYS TOO SMALL
You have less than three points in your X-coordinate and
Y-coordinate arrays.  (Three is the minimum number of required
points.)  This message can also be generated by SFWRLD and
SFSGFA since they both make calls to SFNORM.
.sp
SFNORM - ARRAY DST IS TOO SMALL
You have dimensioned the array DST too small.  DST must be at
least NRA+NIM, where NRA is the number of points defining the
area to be filled, and NIM is the largest number of intersection
points of any fill line with the boundary lines.
.sp
SFNORM - ARRAY IND IS TOO SMALL
You have dimensioned the array IND too small.  IND must be at
least NRA+2*NIM, where NRA is the number of points defining the
area to be filled, and NIM is the largest number of intersection
points of any fill line with the boundary lines.
.sp
SFNORM - LOGIC ERROR - SEE SPECIALIST
A supposedly impossible situation has arisen.  This message indicates a
compiler problem or some sort of tampering with the code of the package.
Contact your NCAR Graphics site representative.
.sp
SFGETC - PARAMETER NAME TOO SHORT - \fIx\fR
Internal parameter names are at least two characters long.
Correct the name of the internal parameter.
.sp
.ne 3
SFGETC - PARAMETER NAME NOT KNOWN - \fIx\fR
The subroutine does not recognize the internal parameter name you
have used.
Correct the name of the internal parameter.
.sp
SFGETI OR SFGETR - PARAMETER NAME TOO SHORT - \fIx\fR
Internal parameter names are at least two characters long.
Correct the name of the internal parameter.
.sp
SFGETI OR SFGETR - PARAMETER NAME NOT KNOWN - \fIx\fR
The subroutine does not recognize the internal parameter name you
have used.
Correct the name of the internal parameter.
.sp
SFSETC - PARAMETER NAME TOO SHORT - \fIx\fR
Internal parameter names are at least two characters long.
Correct the name of the internal parameter.
.sp
SFSETC - PARAMETER NAME NOT KNOWN - \fIx\fR
The subroutine does not recognize the internal parameter name you
have used.
Correct the name of the internal parameter.
.sp
SFSETI OR SFSETR - PARAMETER NAME TOO SHORT - \fIx\fR
Internal parameter names are at least two characters long.
Correct the name of the internal parameter.
.sp
SFSETI OR SFSETR - PARAMETER NAME NOT KNOWN - \fIx\fR
The subroutine does not recognize the internal parameter name you
have used.
Correct the name of the internal parameter.
.sp
.L1 "SOFTFILL Reference Material"
.L2 "Algorithm"
The algorithm used by SFNORM for software fill is as follows:  The given 
points of the polygon boundary are sorted in order of 
increasing distance from a "baseline"
having the desired fill angle.  Then, a "fill line" is passed over the figure,
starting at the minimum distance found and stopping at the maximum distance
found.  Each time the fill line passes one of the points, a list
of boundary lines that the fill line intersects is updated.
At each desired
drawing position, a list of intersection points is generated, and
the points are sorted in the order of their projection on the X axis 
or the Y axis, depending on the fill
angle.
The segments of the fill line that fall inside the polygon
are drawn, using solid lines or dotted lines.  In the latter case,
individual dots are drawn or not drawn as specified by the 
current contents of an 8x8 array representing a square pixel; 
each "dot" may be just a dot or a selected symbol.
