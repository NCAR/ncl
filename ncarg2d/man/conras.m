.\"
.\"	$Id: conras.m,v 1.1.1.1 1992-04-17 22:30:18 ncargd Exp $
.\"
.TH CONRAS 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " CONRAS - Contours data, lines smoothed, crowded lines removed
.nrsN 0
./" USE tsi to PRINT THIS FILE!
.pn 139
.bp
.PH ""
.PF ""
.SK 1
.tr ~
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9CONRAS\s11@"
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
.nf
.na
SUBROUTINE CONRAS (XD,YD,ZD,NDP,WK,IWK,SCRARR)
.fi
.ad
.sp
.R
Super version of CONRAN
.H 3 "Dimension of Arguments"
XD(NDP),YD(NDP),ZD(NDP),WK(13*NDP)
.br
IWK((27+NCP)*NDP),SCRARR(RESOLUTION**2)
.br
where NCP = 4 and RESOLUTION = 40 by default.
.H 3 "Latest Revision"
August 1987
.H 3 "Overview"
CONRAS performs contouring of irregularly
distributed data.  It is the super
member of the CONRAN family.
This version will plot contours; smooth
them using splines under tension;
plot a perimeter or grid; title the plot;
print a message giving the contour intervals
below the map; plot the input data on the map;
label the contour lines and eliminate crowding
of contour lines.  It is the biggest and
slowest member of the family; it is intended
to produce publication-quality maps.
.H 3 "Purpose"
CONRAS plots contour lines using random,
sparse, or irregular data sets.  The data is
triangulated and then contoured.  Contouring
is performed using interpolation of the triangulated 
data.  There are two methods of
interpolation:  C1 surfaces and linear.
.H 3 "Usage"
CALL CONRAS(XD,YD,ZD,NDP,WK,IWK,SCRARR)
.sp
An option setting routine can also be invoked, 
see writeup below.  FRAME must be
called by the user.

If different colors (or intensities) are to be
used for normal intensity, low intensity, or
text output then the values in common block
RASINT should be changed:
.sp
IRASMJ  Color index for normal (major) intensity
lines.
.br
IRASMN  Color index for low intensity lines
.br
IRASTX  Color index for text (labels)
.sp
.H 3 "ARGUMENTS"
.H 3 "On Input"
.VL .6i
.LI \fBXD\fR
Array of dimension NDP containing the X-coordinates of the data points.
.LI \fBYD\fR
Array of dimension NDP containing the Y-coordinates of the data points.
.LI \fBZD\fR
Array of dimension NDP containing the
data values at the points.
.LI \fBNDP\fR
Number of data points (must be 4 or
greater) to be contoured.
.LI \fBWK\fR
Real work array of dimension at least
13*NDP
.LE
.VL .8i
.LI \fBIWK\fR
Integer work array.  When using C1 surfaces
the array must be at least IWK((27+NCP)*NDP).
When using linear interpolation the array
must be at least IWK((27+4)*NDP).
.LI \fBSCRARR\fR
Real work array of dimension at least
.hw RESOLUTION
(RESOLUTION**2) where RESOLUTION is
described in the SSZ option below.  RESOLUTION is 40 by default.
.LE
.sp
.H 3 "On Output"
All arguments remain unchanged except the
scratch arrays IWK, WK, and SCRARR, which have
been written into.  If making multiple runs
on the same triangulation, IWK and WK must be
saved and returned to the next invocation of
CONRAS.
.H 3 "Entry Points"
.na
.nf
CONRAS, CONDET, CONINT, CONCAL, CONLOC, CONTNG,
CONDRW, CONCLS, CONSTP, CONBDN,
CONTLK, CONPDV, CONOP1, CONOP2, CONOP3, CONOP4,
CONXCH, CONREO, CONCOM, CONCLD, CONPMS, CONSTP,
CONGEN, CONLOD, CONECD, CONOUT, CONOT2, CONCLD,
CONSLD, CONLCM, CONLIN, CONDSD, CONSSD
.H 3 "Common Blocks"
.na
.nf
CONRA1, CONRA2, CONRA3, CONRA4, CONRA5, CONRA6,
CONRA7, CONRA8, CONRA9, CONR10, CONR11, CONR12,
CONR13, CONR14, CONR15, CONR16, CONR17, RASINT
INTPR from the DASH package
.ad
.fi
.H 3 "I/O"
Plots the contour map and, via the ERPRT77
package, outputs messages to the message
output unit; at NCAR this unit is the
printer.  The option values are all listed on
standard ERPRT77 output unit; at NCAR this
unit is the printer.
.H 3 "Precision"
Single
.H 3 "Required Library Routines"
DASHSUPR, GRIDAL, CONCOM, CONTERP, the
ERPRT77 package, and the SPPS.
.H 3 "Required GKS Level"
0A
.H 3 "Note for NCAR Users"
This routine is NOT part of the default
libraries at NCAR.  CONRAS must be
be acquired, compiled, and loaded to be
used at NCAR.
.H 3 "Language"
FORTRAN 77
.H 3 "Algorithm"
The sparse data is triangulated, and a virtual
grid is laid over the triangulated area.
Each virtual grid point receives an interpolated 
value.  The grid is scanned once for
each contour level, and all contours at that
level are plotted.
There are two methods of interpolation. The
first is a smooth data interpolation
scheme based on Lawson's C1
surface interpolation algorithm, which has
been refined by Hirosha Akima.  Parts of
Akima's algorithm are used in this package.
.hw section
See the "REFERENCE" section below.
The second is a linear interpolation scheme.
When data is sparse, it is usually better to
use the C1 interpolation.  If you have dense
data (over 100 points), then the linear
interpolation will give the better results.
.H 3 "Portability"
ANSI FORTRAN
.H 3 "Operation"
CALL CONRAS (XD,YD,ZD,NDP,WK,IWK,SCRARR)
.sp
FRAME must be called by the user.

CONRAS has many options, each of which may
be changed by calling one of the four
subroutines CONOP1, CONOP2, CONOP3, or
CONOP4.  The number of arguments to each
CONOP routine is the same as the final
suffix character in the routine's name.

The CONOP routines are called before CONRAS
is called, and values set by these calls
continue to be in effect until they are
changed by another call to a CONOP routine.
.sp
All the CONOP routines have as their first
argument a character string to identify the
option being changed.  This is the only
argument to CONOP1.  CONOP2 has an integer
second argument.  CONOP3 has a real array (or
constant) as its second argument and an
integer (usually the dimension of the
array) as its third argument.  CONOP4 has a
character string as its second argument and
integers for the third and fourth arguments.

Only the first two characters on each side of
the equal sign are scanned.  Therefore only 2
characters for each option are required on
input to CONOP (i.e. 'SCA=PRI' and 'SC=PR'
are equivalent).

Remember, there must be at least 4 data points.
This is equal to the default number of
data points to be used for estimation of partial 
derivatives at each data point.
The estimated partial derivatives are
used for the construction of the interpolating 
polynomial's coefficients.

Listed below are options that can enhance
your plot.  An example of an appropriate
CONOP call is given for each option.  A
complete list of default settings follows
the last option.
.H 3 "Options"
.VL .6i
.LI \fBCHL\fR
This flag determines how the high and low
contour values are set.  These contour values
may be set by the program or by the user.  If
CHL=OFF, the program examines the user's input 
data and determines both the high and low
values.  If CHL=ON, the user must specify the
desired high (HI) and low (FLO) values.
The default is CHL=OFF.

If program set:  CALL CONOP3('CHL=OFF',0.,0)

If user set:  CALL CONOP3('CHL=ON',ARRAY,2)
.br
where ARRAY(1)=HI, ARRAY(2)=FLO

Note: The values supplied for contour increment 
and contour high and low values assume
the unscaled data values.  See the SDC flag,
below.
.VL .75i
.LI "Example:"
.hw ARRAY
CALL CONOP3('CHL=ON',ARRAY,2)
where ARRAY(1)=5020. (the desired
high contour value) and ARRAY(2)=
2000 (the desired low contour value).
These are floating point numbers.
.LE
.sp
.LI \fBCIL\fR
This flag determines how the contour increment 
(CINC) is set.  The increment is either
calculated by the program (CIL=OFF) using the
range of high and low values from the user's
input data, or set by the user (CIL=ON). The
default is CIL=OFF.

If program set:  CALL CONOP3('CIL=OFF',0.,0)

If user set:  CALL CONOP3('CIL=ON',CINC,1)

Note: By default, the program will examine
the user's input data and determine the contour
interval (CINC) at some appropriate range between
the level of high and low values supplied, usually
generating between 15 and 20 contour levels.
.VL .75i
.LI "Example:"
CALL CONOP3('CIL=ON',15.,1) where 15. represents the
contour increment desired by the user.
.LE
.sp
.LI \fBCON\fR
This flag determines how the contour levels
are set.  If CON=ON, the user must specify
the array of contour values and the number of
contour levels.  A maximum of 30 contour (NCL)
levels is permitted.  If CON=OFF, default
values are used.  In this case, the program
will calculate the values for the array and
NCL using input data.  The default is OFF.

If program set:  CALL CONOP3('CON=OFF',0.,0)

If user set:  CALL CONOP3('CON=ON',ARRAY,NCL)

Note: The array (ARRAY) contains the contour
levels (floating point only) and NCL is the
number of levels.  The maximum number of contour 
levels allowed is 30.  When assigning
the array of contour values, the values must
be ordered from smallest to largest.
.VL .75i
.LI "Example:"
DATA RLIST(1),...,RLIST(5)/1.,2.,3.,10.,12./

CALL CONOP3('CON=ON',RLIST,5) where \&'RLIST' 
contains the user specified
contour levels, and 5 is the
number of user specified contour
levels (NCL).
.LE
.sp
\fBWarning on contour options:\fR
.br
It is illegal to use the CON option when
either CIL or CHL are activated.  If
this is done, the option call that detected
the error will not be executed.
.LI \fBDAS\fR
This flag determines which contours are
represented by dashed lines.  The user sets
the dashed line pattern.  The user may specify 
that dashed lines be used for contours
whose value is less than, equal to, or
greater than the dash pattern breakpoint (see
the DBP option below), which is zero by
default.  If DAS=OFF (the default value), all
solid lines are used.
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
.nr 38 \wAll solid lines:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIf greater:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIf equal:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIf less:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIf all same:
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wCALL CONOP4('DAS=OFF',' ',0,0)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCALL CONOP4('DAS=GTR',PAT,0,0)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCALL CONOP4('DAS=EQU',PAT,0,0)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCALL CONOP4('DAS=LSS',PAT,0,0)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCALL CONOP4('DAS=ALL',PAT,0,0)
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'All solid lines:\h'|\n(41u'CALL CONOP4('DAS=OFF',' ',0,0)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'If greater:\h'|\n(41u'CALL CONOP4('DAS=GTR',PAT,0,0)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'If equal:\h'|\n(41u'CALL CONOP4('DAS=EQU',PAT,0,0)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'If less:\h'|\n(41u'CALL CONOP4('DAS=LSS',PAT,0,0)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'If all same:\h'|\n(41u'CALL CONOP4('DAS=ALL',PAT,0,0)
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-12
.sp
Note: PAT must be a ten character
string with a dollar sign ($) for solid and a
single quote (') for blank.  Recall that in
FORTRAN 77, in a quoted string a single quote
is represented by two single quotes (' ').
.VL .75i
.LI "Example:"
CALL CONOP4('DAS=GTR','$$$$$''$$$$',0,0)
.LE
.sp
.LI \fBDBP\fR
This flag determines how the dash pattern
break point (BP) is set.  If DBP=ON, BP must
be set by the user by specifying BP.  If
DBP=OFF the program will set BP to the
default value which is zero.

If program set:  CALL CONOP3('DBP=OFF',0.,0)

If user set:  CALL CONOP3('DBP=ON',BP,1)

Note: BP is a floating point number where the
break for GTR and LSS contour dash patterns
are defined.  BP is assumed to be given relative 
to the untransformed contours.
.VL .75i
.LI "Example:"
CALL CONOP3('DBP=ON',5.,1)
.br
where 5. is the user specified break point.
.LE
.LI \fBDEF\fR
Reset flags to default values.  Activating
this option sets all flags to the default
value.  DEF has no 'ON' or 'OFF' states.

To activate:  CALL CONOP1('DEF')

.LI \fBEXT\fR
Flag to set extrapolation. Normally all
CONRAN versions will only plot the boundaries
of the convex hull defined by the user's data.
To have the contours fill the rectangular
area of the frame, set the EXT switch ON.
The default is OFF.

To turn on:  CALL CONOP1('EXT=ON')

To turn off:  CALL CONOP1('EXT=OFF')

.LI \fBFMT\fR
Flag for the format of the plotted input data
values.  If FMT=OFF, the default values for
FT, L, and IF are used.  The default values
are:

FT = '(G10.3)'
.br
L  = 7 characters including the parentheses
.br
IF = 10 characters printed in the output
field by the format

If FMT=ON, the user must specify values for
FT, L, and IF.  All user specified values
must be given in the correct format.

If program set:  CALL CONOP4('FMT=OFF',' ',0,0)

If user set:  CALL CONOP4('FMT=ON',FT,L,IF)

Note: FT is a character string containing the
format.  The format must be enclosed in
parentheses.  Any format, up to 10 characters
that is allowed at your installation will be
accepted.  L is the number of characters in
FT.  IF is the length of the field created by
the format.

Example:  CALL CONOP4('FMT=ON','(G30.2)',7,30)

\fBWarning:\fR CONRAS will not test for a valid
format.  The format is only allowed to be
10 characters long.

.LI "\fBGRI\fR"
Flag to display the grid. GRI is OFF by default.

To turn on:  CALL CONOP1('GRI=ON')

To turn off:  CALL CONOP1('GRI=OFF')

Note: If GRI is ON, the virtual grid will
be superimposed over the contour plot.
The X and Y tick intervals will be displayed
under the map only if PER=ON (see PER).
.LI \fBINT\fR
Flag to determine the intensities of the contour 
lines and other parts of the plot.  If
INT=OFF, all intensities are set to the default
values.  If INT=ALL, all intensities are set
to the given value, IVAL.  If INT is set to
one of the other possible options (MAJ, MIN,
LAB, or DAT), the intensity level for that
option is set to the given value, IVAL.
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
.nr 38 \wIf program set:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wAll the same:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wMajor lines:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wMinor lines:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wTitle and message:
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wData values:
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wCALL CONOP2('INT=OFF',0)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCALL CONOP2('INT=ALL',IVAL)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCALL CONOP2('INT=MAJ',IVAL)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCALL CONOP2('INT=MIN',IVAL)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCALL CONOP2('INT=LAB',IVAL)
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCALL CONOP2('INT=DAT',IVAL)
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'If program set:\h'|\n(41u'CALL CONOP2('INT=OFF',0)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'All the same:\h'|\n(41u'CALL CONOP2('INT=ALL',IVAL)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Major lines:\h'|\n(41u'CALL CONOP2('INT=MAJ',IVAL)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Minor lines:\h'|\n(41u'CALL CONOP2('INT=MIN',IVAL)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Title and message:\h'|\n(41u'CALL CONOP2('INT=LAB',IVAL)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\h'|\n(41u'
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Data values:\h'|\n(41u'CALL CONOP2('INT=DAT',IVAL)
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-14
.sp
Note: 'INT=DAT' relates to the plotted data
values and the plotted maximums and minimums.

Note: IVAL is the intensity desired.  
IVAL values
range from 0 to 255 or the character strings
\&'LO' and 'HI'.  The default is \&'HI' except
for INT=MIN, which is set to 'LO'.

Example:  CALL CONOP2('INT=ALL',110)
.LI \fBITP\fR
Set the interpolation scheme.
There are two schemes \(em C1 surfaces and linear.
The C1 method takes longer but will give the
best results when the data is sparse (less
than 100 points).  The linear method will
produce a better plot when there is a dense
data set.  The default is C1 surface.

For C1 surface CALL CONOP1('ITP=C1')

For linear CALL CONOP1('ITP=LIN')

.LI \fBLAB\fR
This flag can be set to either label the contours 
(LAB=ON) or not (LAB=OFF).  The default
value is LAB=ON.

To turn on:  CALL CONOP1('LAB=ON')

To turn off:  CALL CONOP1('LAB=OFF')

.LI \fBLOT\fR
Flag to list options on the printer.  The default 
value is set to OFF, and no options
will be displayed.

To turn on:  CALL CONOP1('LOT=ON')

To turn off:  CALL CONOP1('LOT=OFF')

Note: If users want to print the option
values, they should turn this option ON.  The
option values will be sent to the standard
output unit as defined by the support
routine I1MACH.

.LI \fBLSZ\fR
This flag determines the label size.  If
LSZ=OFF, the default ISZLSZ value will be
used.  If LSZ=ON, the user should specify
ISZLSZ.  The default value is 9 plotter
address units.

If program set:  CALL CONOP2('LSZ=OFF',0)

If user set:  CALL CONOP2('LSZ=ON',ISZLSZ)

.bp
Note: ISZLSZ is the requested character
size in plotter address units.
.VL .75i
.LI Example:
CALL CONOP2('LSZ=ON',4)
where 4 is the user desired
integer plotter address
units.
.LE
.LI \fBMES\fR
Flag to plot a message. The default is ON.

To turn on:  CALL CONOP1('MES=ON')

To turn off:  CALL CONOP1('MES=OFF')

Note: If MES=ON, a message is printed below
the plot giving contour intervals and execution 
time in seconds.  If PER or GRI is ON,
the message also contains the X and Y tick
intervals.
.LI \fBNCP\fR
Flag to indicate the number of data points
used for the partial derivative
estimation.  If NCP=OFF, NUM is set to
4, which is the default value.  If NCP=ON,
the user must specify NUM greater than or
equal to 2.

If program set:  CALL CONOP2('NCP=OFF',0)

If user set:  CALL CONOP2('NCP=ON',NUM)

Note: NUM = number of data points used for
estimation.  Changing this value effects the
contours produced and the size of input array
IWK.

Example:  CALL CONOP2('NCP=ON',3)

.LI \fBPDV\fR
Flag to plot the input data values.  The
default value is PDV=OFF.

To turn on:  CALL CONOP1('PDV=ON')

To turn off:  CALL CONOP1('PDV=OFF')

Note: If PDV=ON, the input data values are
plotted relative to their location on the
contour map.  If you only wish to see the
locations and not the values, set PDV=ON and
change FMT to produce an asterisk (*) such as
(I1).

.LI \fBPER\fR
Flag to set the perimeter.  The default value
is PER=ON, which causes a perimeter to be
drawn around the contour plot.

To turn on:  CALL CONOP1('PER=ON')

To turn off:  CALL CONOP1('PER=OFF')

Note: If MES is ON, the X and Y tick intervals
will be given.  These are the intervals in user
coordinates that each tick mark represents.

.LI \fBPMM\fR
Flag to plot relative minimums and maximums.
This flag is OFF by default.

To turn off:  CALL CONOP1('PMM=OFF')

To turn on:  CALL CONOP1('PMM=ON')

.LI \fBPSL\fR
Flag which sets the plot shield option.
The outline of the shield will be drawn on
the same frame as the contour plot.
By default this option is OFF
(see SLD option).

Draw the shield:  CALL CONOP1('PSL=ON')

Don't draw it:  CALL CONOP1('PSL=OFF')

.LI \fBREP\fR
Flag indicating the use of the same data in
a new execution.  The default value is OFF.

To turn on:  CALL CONOP1('REP=ON')

To turn off:  CALL CONOP1('REP=OFF')

Note: If REP=ON, the same X-Y data and triangulation 
are to be used but it is assumed
the user has changed contour values or resolution
for this run.  Scratch arrays WK and IWK must
remain unchanged.

.LI \fBSCA\fR
Flag for scaling of the plot on a frame.
This flag is ON by default.
.br
User scaling:  CALL CONOP1('SCA=OFF')

Program scaling:  CALL CONOP1('SCA=ON')

Prior window:  CALL CONOP1('SCA=PRI')

Note:  With SCA=OFF, plotting instructions
will be issued using the user's input coordinates, 
unless they are transformed via FX and
FY transformations.  Users will find an
extended discussion in the "INTERFACING WITH
OTHER GRAPHICS ROUTINES" section below.  The SCA
option assumes that all input data falls into
the current window setting.  With SCA=ON, the
entry point will establish a viewport so that
the user's plot will fit into the center 90
percent of the frame.  When SCA=PRI, the
program maps the user's plot instructions into
the portion of the frame defined by the
current normalization transformation.  SCA=OFF
should be used to interface with EZMAP.

.LI \fBSDC\fR
Flag to determine how to scale the data on
the contours.  If SDC=OFF, the floating point
value is given by scale.  If SDC=ON, the user
may specify SCALE.  The default value for SCALE
is 1.

If program set:  CALL CONOP3('SDC=OFF',0.,0)

If user set:  CALL CONOP3('SDC=ON',SCALE,1)

Note: The data plotted on contour lines and
the data plotted for relative minimums and
maximums will be scaled by the floating point
value given by SCALE.  Typical SCALE values
are 10., 100., 1000., etc.  The original data
values are multiplied by SCALE.  SCALE must be
a floating point number and is displayed in the
message (see MES).

Example:  CALL CONOP2('SDC=ON',100.,1)

.LI \fBSLD\fR
Activate or deactivate the shielding option.
When this option is activated, only those
contours within the shield are drawn. The shield
is a polygon specified by the user and must
be given in the same coordinate range as the
data. It must define only one polygon.

To activate the shield:
.br
CALL CONOP3('SLD=ON',ARRAY,ICSD)

To deactivate the shield:
.br
CALL CONOP3('SLD=OFF',0.,0)

Note:  ARRAY is a real array ICSD elements long.
The first ICSD/2 elements are X coordinates and
the second ICSD/2 elements are Y coordinates.
ICSD is the length of entire array, the
number of (X + Y) shield coordinates. The polygon
must be closed; that is, the first and last
points describing it must be the same.
.VL .75i
.LI Example:
DIMENSION SHLD
.br
DATA SHLD/ 7.,10.,10.,7.,7.,7.,7.,10.,10.,7./
.br
CALL CONOP3 ('SLD=ON',SHLD,10)
.LE
.sp
.LI \fBSML\fR
Flag to determine the size of minimum and
maximum contour labels.  If SML=OFF, the
ISZSML default value of 15 is used.
If SML=ON, the user must specify ISZSML.

If program set:  CALL CONOP2('SML=OFF',0)

If user set:  CALL CONOP2('SML=ON',ISZSML)

Note: ISZSML is an integer number that is
the size of labels in plotter address units
as defined in the SPPS entry WTSTR.

Example:  CALL CONOP2('SML=ON',12)

.LI \fBSPD\fR
Flag for the size of the plotted input data
values.  If SPD=OFF, the value of ISZSPD is
8, which is the default.  If SPD=ON, the user
must specify ISZSPD.

If program set:  CALL CONOP2('SPD=OFF',0)

If user set:  CALL CONOP2('SPD=ON',ISZSPD)

Note: ISZSPD is an integer number giving the
size to plot the data values in plotter address
units as defined in the SPPS entry WTSTR.    

Example:  CALL CONOP2('SPD=ON',6)

.LI \fBSSZ\fR
Flag to determine the resolution (number of
steps in each direction).  If SSZ=ON, the
user sets ISTEP, or, if SSZ=OFF, the program
will automatically set ISTEP at the default
value of 40.

If program set:  CALL CONOP2('SSZ=OFF',0)

If user set:  CALL CONOP2('SSZ=ON',ISTEP)

Note: ISTEP is an integer specifying the density
of the virtual grid. In most cases, the default
value of 40 produces pleasing contours.  For
coarser but quicker contours, lower the
value.  For smoother contours at
the expense of taking longer time, raise
the value.  

Note:  For step sizes greater
than 200 in CONRAS, the arrays PV in common
CONRA1 and ITLOC in common CONRA9 must be
expanded to about 10 more than ISTEP.
See CONRA1 and CONRA9 comments below for more
information.
.VL .75i
.LI Example:
CALL CONOP2('SSZ=ON',25)
.br
This ISTEP value will produce a coarse contour.
.LE
.sp
.LI \fBSTL\fR
Flag to determine the size of the title.
ISZSTL may be set by the user (STL=ON), or
the program will set it to the default size
of 16 plotter address units (STL=OFF).

If program set:  CALL CONOP2('STL=OFF',0)

If user set:  CALL CONOP2('STL=ON',ISZSTL)

Note: When 30 or 40 characters are used for
the title, the default size of 16 plotter
address units works well.  For longer titles,
a smaller title size is required.

Example:  CALL CONOP2('STL=ON',13)

.LI \fBTEN\fR
Flag to determine the tension factor applied
when smoothing contour lines.  The user may
set TENS or allow the program to set the
value.  If user set, TENS must have a value
greater than zero and less than or equal to
30.  The default value is 2.5.

If program set:  CALL CONOP3('TEN=OFF',0.,0)

If user set:  CALL CONOP3('TEN=ON',TENS,1)

Smoothing of contour lines is accomplished
with splines under tension.  To adjust the
amount of smoothing applied, adjust the tension 
factor.  Setting TENS very large
(i.e. 30.), effectively shuts off smoothing.

Example:  CALL CONOP3('TEN=ON',14.,1)

.LI \fBTFR\fR
Flag to advance the frame before triangulation.
The default value is TFR=ON, which means that
the contours and the triangles will be plotted
on separate frames.

If program set:  CALL CONOP1('TFR=ON')

To turn off:  CALL CONOP1('TFR=OFF')

Note: Triangles are plotted after the contouring 
is completed.  To see the triangles
over the contours, turn this switch off.

.LI \fBTLE\fR
Flag to place a title at the top of the plot.
If  TLE=ON, the user must specify CHARS and
INUM.  CHARS is the character string containing
the title.  INUM is the number of characters
in CHARS.  The default value is OFF.

To turn on: 
.br
CALL CONOP4('TLE=ON',CHARS,INUM,0)

To turn off:  
.br
CALL CONOP4('TLE=OFF',' ',0,0)

Note: If longer than 64-character titles are
desired, the character variable ISTRNG found
in CONRA7 must be increased appropriately.

Example: CALL CONOP4('TLE=ON','VECTOR REVIEW',13,0)

.LI \fBTOP\fR
Flag to plot only the triangles.

To turn off:  CALL CONOP1('TOP=OFF')

To turn on:  CALL CONOP1('TOP=ON')

Note: The user may wish to overlay the triangles 
on some other plot.  'TOP=ON' will
allow that.  This option, when activated
(TOP=ON), will set TRI=ON, and TFR=OFF.  If
the user wants TFR=ON, it should be set after
TOP is set.  If the user sets TOP=OFF, it will
set TRI=OFF and TFR=ON. If the user wants TRI
or TFR different, set them after the
TOP call.

.LI \fBTRI\fR
Flag to plot the triangulation.  The default is
OFF and therefore the triangles are not drawn.

To turn on:  CALL CONOP1('TRI=ON')

To turn off:  CALL CONOP1('TRI=OFF')

Note: Plotting the triangles will indicate to
the user where good and bad points of interpolation 
are occurring in the contour map.
Equilateral triangles are optimal for interpolation.  
Quality degrades as triangles
approach a long and narrow shape.  The convex
hull of the triangulation is also a poor
point of interpolation.
.LE
.H 3 "Option Default Values"
Below are listed the default
values for the various options given above.
Unless the user specifies otherwise, these
values will be used in execution of the various 
options.
.>>
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
.nr 80 0
.nr 38 \wCHL=OFF
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCIL=OFF
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCON=OFF
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wDAS=OFF
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wDBP=OFF
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wEXT=OFF
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wFMT=OFF
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wGRI=OFF
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wITP=C1
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wLAB=ON
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \wLOT=OFF
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wLSZ=OFF
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wMES=ON
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wNCP=OFF
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPDV=OFF
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPER=ON
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wPMM=OFF
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wREP=OFF
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSCA=ON
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wSDC=OFF 
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
.nr 38 \wSLD=OFF-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wSML=OFF-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wSPD=OFF-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wSSZ=OFF-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wSTL=OFF-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wTEN=OFF-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wTFR=ON -\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wTOP=OFF-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
.nr 38 \wTRI=OFF-\n(82-3n-\n(83-3n-\n(84
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 82 +\n(38
.nr 83 +\n(38
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
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CHL=OFF\h'|\n(41u'LOT=OFF\h'|\n(42u'SLD=OFF
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CIL=OFF\h'|\n(41u'LSZ=OFF\h'|\n(42u'SML=OFF
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CON=OFF\h'|\n(41u'MES=ON\h'|\n(42u'SPD=OFF
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'DAS=OFF\h'|\n(41u'NCP=OFF\h'|\n(42u'SSZ=OFF
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'DBP=OFF\h'|\n(41u'PDV=OFF\h'|\n(42u'STL=OFF
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'EXT=OFF\h'|\n(41u'PER=ON\h'|\n(42u'TEN=OFF
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'FMT=OFF\h'|\n(41u'PMM=OFF\h'|\n(42u'TFR=ON 
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'GRI=OFF\h'|\n(41u'REP=OFF\h'|\n(42u'TOP=OFF
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ITP=C1\h'|\n(41u'SCA=ON\h'|\n(42u'TRI=OFF
.ta \n(80u \n(81u \n(84u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'LAB=ON\h'|\n(41u'SDC=OFF \h'|\n(42u'
.fc
.nr T. 1
.T# 1
.35
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-13
.<<
.H 3 "Default Values for User Specified Parameters"
The option default values given above, if
used, will set default values for the following parameters:
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
.rm 80 81 82 83
.nr 34 \n(.lu
.eo
.am 81
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*3u/5u
.if \n(.l<\n(81 .ll \n(81u
.in 0
Up to 30 contour levels allowed.
Values  are computed by the
program, based on input.
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
.ll \n(34u*3u/5u
.if \n(.l<\n(81 .ll \n(81u
.in 0
Computed by the program based on the
range of HI and LO values of the
input data.
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
.ll \n(34u*3u/5u
.if \n(.l<\n(81 .ll \n(81u
.in 0
Computed by the program based on the
lowest unscaled input data.
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
.ll \n(34u*3u/5u
.if \n(.l<\n(81 .ll \n(81u
.in 0
Computed by the program based on the
highest unscaled input data.
.br
.di
.nr d| \n(dn
.nr d- \n(dl
..
.ec \
.eo
.am 81
.br
.di e+
.35
.ft \n(.f
.ll \n(34u*3u/5u
.if \n(.l<\n(81 .ll \n(81u
.in 0
\&'HI' for all except minor contour   
lines, which are 'LO'.
.br
.di
.nr e| \n(dn
.nr e- \n(dl
..
.ec \
.eo
.am 81
.br
.di f+
.35
.ft \n(.f
.ll \n(34u*3u/5u
.if \n(.l<\n(81 .ll \n(81u
.in 0
Computed by the program based on
input data.  Up to 30 contour
levels are permitted.
.br
.di
.nr f| \n(dn
.nr f- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \wParameter
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wARRAY
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wBP
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCINC
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wParameter
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wFLO
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wFT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wHI
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wCHARS
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIF
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wINUM
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIPAT
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wISZLSZ
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wISZSML
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wISZSPD
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wISZSTL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wISTEP
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wIVAL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNCL
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wNUM
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wSCALE
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wTENS
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \wICSD
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.81
.rm 81
.nr 82 0
.82
.rm 82
.nr 83 0
.83
.rm 83
.nr 38 \wDefault-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w0.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \wDefault-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w(G10.3) Parentheses must be included.-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \wNo title-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w10 characters-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \wNo title-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w'$$$$$$$$$$' (This is a 10 character string.)-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w9 plotter address units-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w15 plotter address units-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w8 plotter address units-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w16 plotter address units-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w40-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w7 characters (including both parentheses)-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w4 data points-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w1. (no scaling performed)-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w2.5-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
.nr 38 \w0 (no shield)-\n(81-3n-\n(82-3n-\n(83
.if \n(38>0 .nr 38 \n(38/2
.if \n(38<0 .nr 38 0
.nr 81 +\n(38
.nr 82 +\n(38
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
.nr TW \n(83
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
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Parameter\h'|\n(41u'Default
.R
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ARRAY\h'|\n(41u'
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
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'BP\h'|\n(41u'0.
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CINC\h'|\n(41u'
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
.bp
.T&
.B
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Parameter\h'|\n(41u'Default
.R
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'FLO\h'|\n(41u'
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
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'FT\h'|\n(41u'(G10.3) Parentheses must be included.
.sp
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'HI\h'|\n(41u'
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
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'CHARS\h'|\n(41u'No title
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IF\h'|\n(41u'10 characters
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'INUM\h'|\n(41u'No title
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IPAT\h'|\n(41u''$$$$$$$$$$' (This is a 10 character string.)
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ISZLSZ\h'|\n(41u'9 plotter address units
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ISZSML\h'|\n(41u'15 plotter address units
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ISZSPD\h'|\n(41u'8 plotter address units
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ISZSTL\h'|\n(41u'16 plotter address units
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ISTEP\h'|\n(41u'40
.sp
.ne \n(e|u+\n(.Vu
.if (\n(e|+\n(#^-1v)>\n(#- .nr #- +(\n(e|+\n(#^-\n(#--1v)
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'IVAL\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.e+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'L\h'|\n(41u'7 characters (including both parentheses)
.sp
.ne \n(f|u+\n(.Vu
.if (\n(f|+\n(#^-1v)>\n(#- .nr #- +(\n(f|+\n(#^-\n(#--1v)
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NCL\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.f+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'NUM\h'|\n(41u'4 data points
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'SCALE\h'|\n(41u'1. (no scaling performed)
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'TENS\h'|\n(41u'2.5
.sp
.ta \n(80u \n(83u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'ICSD\h'|\n(41u'0 (no shield)
.fc
.nr T. 1
.T# 1
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.rm f+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-79
.sp
.H 3 "Options Which Effect the Contours"
The shape of the contours may be modified by
changing NCP  and SSZ.  NCP controls the
number of data points to be used in the
interpolation.  Increasing NCP causes more
of the  surrounding  data  to influence the
point of interpolation.  Some  datasets cause
difficulty  when  trying  to produce meaningful
contours (triangles that are long and  narrow).
By modifying NCP a user can fine-tune a
plot.   Increasing  ISTEP, the density of the
virtual grid, will smooth out the contour
lines and pick up more detail (new contours
will appear as ISTEP increases and old ones will
sometimes break into more distinct units).
ISTEP is changed by the SSD option.
.sp
Note:  If NCP.GT.25, arrays DSQ0 and IPC0 in CONDET
must be adjusted accordingly.  Also NCPSZ in
CONBDN (25 by default), must be increased to
NCP.   The default value of NCP, which is 4,
produces pleasing pictures in most cases.
However, fine-tuning of the interpolation can
be obtained by increasing the size of NCP,
with a corresponding linear increase in work
space.

The interpolation method used will also cause
different-looking contours.  The C1 method
is recommended when the data is sparse.  It
will smooth the data and add trends (false
hills and valleys).  The linear method is
recommended when data is dense (GT 50 to 100)
it will not smooth the data or add trends.
.H 3 "Interfacing with Other Graphics Routines"
Normally the scaling factor will be set to OFF.
In most cases mapping can be performed before
calling the CONRAS entry point, thus saving the
user from modifying the file.  If reasonable
results cannot be obtained, the statement
functions, FX and FY, will have to be replaced.
The routines having these statement functions
are:

CONDRW, CONPDV, CONTLK, CONPMS, CONGEN
.H 3 "References"
Akima, Hirosha:  "A Method of Bivariate Interpolation and
Smooth Surface Fitting for Irregularly
Distributed Data Points."
\fIACM Transactions on Mathematical Software\fR,
vol 4, no. 2, June 1978, pages 148-159.
.sp
Lawson, C.L.:  "Software for C1 Surface Interpolation,"
\fIJPL Publication\fR 77-30,
August 15, 1977.
.H 3 "CONRAS Error Messages"
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
.rm 80 81 82
.nr 34 \n(.lu
.eo
.am 82
.br
.di a+
.35
.ft \n(.f
.ll \n(34u*1u/4u
.if \n(.l<\n(82 .ll \n(82u
.in 0
ILLEGAL USE OF CON OPTION
WITH CIL OR CHL OPTIONS
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
.ll \n(34u*1u/4u
.if \n(.l<\n(82 .ll \n(82u
.in 0
CONTOUR STORAGE EXHAUSTED
.br
This error is trapped and
nullified by CONRAN.  It
serves to signal the user
that a contour level may not
be complete.
.br
.di
.nr b| \n(dn
.nr b- \n(dl
..
.ec \
.eo
.am 82
.br
.di c+
.35
.ft \n(.f
.ll \n(34u*1u/4u
.if \n(.l<\n(82 .ll \n(82u
.in 0
ASPECT RATIO OF X AND Y
GREATER THAN 5 TO 1.
This error may cause a poor
quality plot.  Usually this
can be fixed by multiplying
X or Y by a constant factor.
If this solution is
unacceptable then increasing
SSZ to a very large value
may help.  
Note:  This can be
expensive.
.br
.di
.nr c| \n(dn
.nr c- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \wError
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
.nr 38 \w7
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w8
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w9
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \wError
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w10
.if \n(31<\n(38 .nr 31 \n(38
.nr 38 \w11
.if \n(31<\n(38 .nr 31 \n(38
.80
.rm 80
.nr 60 \n(31
.nr 38 \n(60+\n(32
.if \n(38>\n(80 .nr 80 \n(38
.if \n(38<\n(80 .nr 60 +(\n(80-\n(38)/2
.nr 81 0
.nr 38 \wRoutine
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONRAS
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONRAS
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONTNG
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONTNG
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONOP
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONCLS
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONOP
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONOP
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONOP
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wRoutine
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONDRW
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wCONSTP
.if \n(81<\n(38 .nr 81 \n(38
.81
.rm 81
.nr 82 0
.nr 38 \wMessage
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wINPUT PARAMETER NDP LT NCP
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wNCP GT MAX SIZE OR LT 2
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wALL COLINEAR DATA POINTS
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wIDENTICAL INPUT DATA POINTS FOUND
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wUNDEFINED OPTION
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wCONSTANT INPUT FIELD
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wINCORRECT CONOP CALL USED
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wNUMBER OF CONTOUR LEVELS EXCEEDS 30
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \wMessage
.if \n(82<\n(38 .nr 82 \n(38
.82
.rm 82
.nr 38 \n(a-
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \n(b-
.if \n(82<\n(38 .nr 82 \n(38
.nr 38 \n(c-
.if \n(82<\n(38 .nr 82 \n(38
.35
.nf
.ll \n(34u
.nr 38 1n
.nr 79 0
.nr 40 \n(79+(0*\n(38)
.nr 80 +\n(40
.nr 60 +\n(40
.nr 41 \n(80+(3*\n(38)
.nr 81 +\n(41
.nr 42 \n(81+(3*\n(38)
.nr 82 +\n(42
.nr TW \n(82
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
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Error\h'|\n(41u'Routine\h'|\n(42u'Message
.R
.sp
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'1\h'|\n(41u'CONRAS\h'|\n(42u'INPUT PARAMETER NDP LT NCP
.sp
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'2\h'|\n(41u'CONRAS\h'|\n(42u'NCP GT MAX SIZE OR LT 2
.sp
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'3\h'|\n(41u'CONTNG\h'|\n(42u'ALL COLINEAR DATA POINTS
.sp
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'4\h'|\n(41u'CONTNG\h'|\n(42u'IDENTICAL INPUT DATA POINTS FOUND
.sp
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'5\h'|\n(41u'CONOP\h'|\n(42u'UNDEFINED OPTION
.sp
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'6\h'|\n(41u'CONCLS\h'|\n(42u'CONSTANT INPUT FIELD
.sp
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'7\h'|\n(41u'CONOP\h'|\n(42u'INCORRECT CONOP CALL USED
.sp
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'8\h'|\n(41u'CONOP\h'|\n(42u'
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
.sp
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'9\h'|\n(41u'CONOP\h'|\n(42u'NUMBER OF CONTOUR LEVELS EXCEEDS 30
.bp
.T&
.B
.ta \n(80u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'Error\h'|\n(41u'Routine\h'|\n(42u'Message
.R
.sp
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'10\h'|\n(41u'CONDRW\h'|\n(42u'
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
.sp
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(60u \n(81u \n(82u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'11\h'|\n(41u'CONSTP\h'|\n(42u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(42u
.in +\n(37u
.c+
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
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-60
.sp
The errors listed above are defined as recoverable
errors should the user wish to use them in that
fashion.  The documentation on the ERPRT77 package
explains how to recover from an error.
.LE
