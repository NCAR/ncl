.\"
.\"	$Id: histgr.m,v 1.1.1.1 1992-04-17 22:30:30 ncargd Exp $
.\"
.TH HISTGR 3NCARG "MARCH 1988" NCAR "NCAR GRAPHICS"
.so MACRO_SOURCE
.dsNA " HISTGR - Plots histograms
.dsS1 " CALL HSTOPL (STRING) set options
.dsS2 " CALL HSTOPR (STRING,ARRAY,LARR) specify real arrays
.dsS3 " CALL HSTOPC (STRING,STRING2,NUMBER,ILCH) specify characters
.dsS4 " CALL HSTOPI (STRING,PARAM1,PARAM2,ICOL,LCOL) specify integers
.nrsN 4
./" USE tsi to PRINT THIS FILE!
.pn 323
.bp
.PH ""
.PF ""
.SK 1
.tr ~
.po -.25i
.ll 6.5i
.PH ""
.EH "@\s9NCAR Graphics User's Guide\s11@@@"
.OH "@@@\s9HISTGR\s11@"
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
.ha
.na
SUBROUTINE HISTGR (DAT1,NDIM,NPTS,IFLAG,CLASS,NCLASS,WRK,NWRK)
.ad
.hy
.fi
.R
.H 3 "Latest Revision"
August 1987
.H 3 "Purpose"
HISTGR plots a histogram with various options
including specification of class values,
spacing between histogram bars, shading of
bars, windowing (i.e. scaling), specification
of color, labels, titles, etc.  Data values
are partitioned into classes; histogram bars
represent either number of occurrences within
each class, or a Y-value associated with
that class (user choice).  Options are set
by calls to subroutines HSTOPL, HSTOPR, HSTOPC,
and HSTOPI before the call to HISTGR.
.H 3 "Usage"
To draw a basic histogram (one utilizing
the default features):

.nf
.na
 CALL HISTGR (DAT1,NDIM,NPTS,IFLAG,CLASS,
+~~~~~~NCLASS,WRK,NWRK)
.fi
.ad

Error handling is performed by the ERPRT77
centralized error-handling package.  To
recover from a warning (recoverable error),
call ENTSR(IDUM,1) before calling HISTGR.
Otherwise an error message is printed and
the run is terminated.
.H 3 "ARGUMENTS"
.H 3 "On Input"
.VL .9i
.LI "\fBDAT1\fR"
Two dimensional real array containing data of one
of two types, either values to be collected into
class intervals before plotting, or values which
have already been assigned to class intervals and
only need to be displayed.  See argument IFLAG
for a more complete description of HISTGR input
data options.  DAT1 is dimensioned: DAT1(NDIM,2).
.LI "\fBNDIM\fR"
The size of the first dimension of DAT1 as set
in the dimension statement of the calling program.
.LI "\fBNPTS\fR"
Number of values actually stored into DAT1 on this
call.  NPTS must always be less than or equal to
NDIM.
.LI "\fBIFLAG\fR"
An integer flag which selects one of four options
provided by the HISTGR utility.  The options are:
.VL .9i
.LI "\fBIFLAG = 0\fR"
A single array of length NPTS is loaded
into the DAT1 array.  HISTGR computes
NCLASS equally sized class intervals
that vary from the minimum value in
DAT1 to the maximum value in steps of
(MAX-MIN)/NCLASS.

All values of DAT1 that fall in each
class interval are separately accumulated
for that interval.  The final tabulations
are plotted as a histogram of NCLASS bars.
The bar height can be labeled with the
number of points that fall within this
particular class interval (bin size),
or it can be given as a percentage of
the number of values input, NPTS.

Note that under this option the user
has no control over the range of the
class intervals.  They are internally
determined from the range of the data.
.LI "\fBIFLAG = 1\fR"
This option is similar to the IFLAG = 0
option except that the user can select
the range of the class intervals into
which the data are collected.  For
example, say the user wants to collect
the number of occurrences of the DAT1
values that fall within 5 equally spaced
intervals in the value range from 0. to
10.  The user would then input NCLASS+1
class interval end points into array
CLASS, namely 0., 2., 4., 6., 8., and
10.  These values need not be entered in
monotonically increasing order and need not
be equally spaced.
.LI "\fBIFLAG = 2\fR"
This option allows the user to enter
and display data which has already
been accumulated into class intervals,
i.e., already available histograms.
The data input to DAT1 thus have
percentage of total, or number of
occurrences values.  In this case the
number of points in DAT1, NPTS, is
equal to the number of class intervals
(histogram bars), NCLASS.  The NCLASS
class interval midpoints are loaded
into array CLASS.  They do not have to
be of equal width.
.LI "\fBIFLAG = 3\fR"
This option is the same as option
IFLAG = 2 except that two histograms
can be displayed for comparison purposes.
The first histogram is loaded into
DAT1(NPTS,1).  The second histogram is
loaded into DAT1(NPTS,2).  The first
histogram can partially shade or obscure
the second histogram by the appropriate
selection of the SPAC and OVERLP options.
.sp
.LE
Note that NPTS = NCLASS when IFLAG = 2 or 3.
.LI "\fBCLASS\fR"
Real array containing class values, dimensioned
(NCLASS+1).  This array has the following IFLAG
dependencies:
.VL .9i
.LI "\fBIFLAG = 0\fR"
CLASS is not used.
.LI "\fBIFLAG = 1\fR"
NCLASS+1 class interval end points
are loaded into array CLASS in a
monotonically increasing order.  The
intervals need not be of equal width.
.LI "\fBIFLAG = 2\fR"
NCLASS midpoint intervals are loaded
into array CLASS.  They must be in
monotonically increasing order, but
need not be of equal widths.  The
histogram bars will however be displayed
with equal widths.
.LI "\fBIFLAG = 3\fR"
Same as for IFLAG = 2.
.LE
.LI "\fBNCLASS\fR"
Number of class  intervals (histogram bars) specified.
NCLASS must be .GE. 1.
.LI "\fBWRK\fR"
Real scratch array, dimensioned by NWRK
in the dimension statement of the calling program.
.LI "\fBNWRK\fR"
The dimension size of array WRK determined from:
NDIM + 3 * (NCLASS + 1)
.LE
.sp
Note: The frequency axis label values will be integers
if the maximum Y-value (calculated within HISTGR)
is an integer multiple of 4.  Otherwise, real
values with format F11.1 are used.
.H 3 "On Output"
All arguments remain unchanged except the scratch
array WRK.
.H 3 "Entry Points"
HSTBKD, HSTEXP, HSTMED, HSTOPC, HSTOPL, HSTOPR,
HSTOPI, HSTSTR, HISTGR, HSTLST, NWTSTR
.H 3 "Common Blocks"
HSTGC1, HSTGC2
.H 3 "I/O"
Plots histograms using various options.
.H 3 "Precision"
Single
.H 3 "Required Library Files"
The ERPRT77 package and the SPPS.
.H 3 "Required GKS Level"
0A
.H 3 "Language"
FORTRAN 77
.H 3 "History"
First written in the fall of 1984.  Revised summer
of 1987.
.H 3 "Algorithm"
Calculates occurrence of Y-values within classes, or
assigns Y-values to X-values (class mid-values),
depending on which flag is chosen by the user.
If Y-values are assigned to classes, class interval
boundaries are handled as follows.  Y-values which
fall on the left boundary of a class interval are
counted in that interval.  This accounts for all
Y-values at an interval boundary except for the right
boundary of the rightmost class interval.  Such
occurrences are added to the rightmost interval,
thus only this interval contains Y-values which
fall on either boundary.
Draws a histogram incorporating various user-defined
options.
.H 3 "Portability"
FORTRAN 77
.sp
.ti -1.5i
A description of the option-setting entries of HISTGR follows.
.ti -1.5i
.hD "SUBROUTINE HSTOPL"
.H 3 "Purpose"
To set various options for the package.
.H 3 "Usage"
CALL HSTOPL (STRING)
.H 3 "Arguments"
.VL .9i
.LI "\fBSTRING\fR"
A character string (which may have up
to seven characters) where the first
2 or 3 characters are abbreviations for
the option followed by \&'=', followed by
\&'ON' or \&'OFF'.  \&'OFF' may be abbreviated
to \&'OF'.   Embedded spaces are not allowed.

Example:  CALL HSTOPL('PER=OFF')

The following options are turned \&'ON' or \&'OFF' by this
routine.
The default values are listed at the end of the HISTGR
documentation.
.VL .6i
.LI "HOR"
Horizontal.
.br
The histogram bars are drawn horizontally if \&'HOR=ON'.
If 'HOR=OFF', they are drawn vertically.
.LI "PER" 
Percent axis.
.br
If 'PER=ON', a labeled percent axis is drawn on the
right side of the histogram (or on top if
horizontal.)  If  'PER=OFF', the percent axis is
not drawn.
.LI "MID" 
Midvalues.
.br
If 'MID=ON', the class labels are put at the
midpoint of each interval. If 'MID=OFF', the class
interval end points are labeled.  MID defaults
to ON if IFLAG = 2 or 3.
.LI "SHA"
Shading.
.br
If 'SHA=ON', the histogram rectangles are shaded.
If 'SHA=OFF', the bars are not shaded.  Laser printers
may vary as to how they interpret shading;
some will shade the bars black, others
will only draw the outline, etc., depending
on whether or not they support FILL AREA in
hardware.  On color terminals, the shading
will correspond to whatever the RGB color
table assigns to color index 1, even when
the color option is 'COL=OFF' (and all
other portions of the histogram are white).
Terminals that do not \%support FILL AREA or
color will show the outline of the "shaded"
histogram bars in white (they will appear to
be unshaded).
.LI "DRL"
Draw lines.
.br
If 'DRL=ON', lines are drawn through the histogram
rectangles where Y-axis tick marks would occur.
When IFLAG = 3 (when comparing two datasets
in one histogram), lines are drawn through
the bars associated with the first dataset
only.
If 'DRL=OFF', lines are not drawn.
.LI "MED"
Median.
.br
If 'MED=ON', a line is drawn through the median
of all points.  If 'MED=OFF', this line is not drawn.
MED does not apply when assigning Y-values to
X-values; it is valid only for IFLAG = 1 or 2.
.LI "PRM"
Perimeter.
.br
If 'PRM=ON', a perimeter is drawn around the
histogram.  If 'PRM=OFF', no perimeter is drawn.
.LI "FRA"
Frame advance.
.br
If 'FRA=ON', the frame is advanced automatically
after the histogram is drawn.
If 'FRA=OFF', the frame is not advanced, and the
user must call FRAME.
.LI "LIS"
List options.
.br
If 'LIS=ON', all the options along with their values
are printed on the standard output.
If 'LIS=OFF', nothing is printed on the standard output.
.LI "DEF"
Global default.
.br
If 'DEF=ON', all the options are set to their default
values; see list of default values below. 'DEF=OFF' has no effect.
.LE
.LE
.ti -1.5i
.hD "SUBROUTINE HSTOPR"
.H 3 "Purpose"
To specify various REAL arrays to be used by the
HISTGR package.
.H 3 "Usage"
CALL HSTOPR (STRING,ARRAY,LARR)
.H 3 "Arguments"
.VL .9i
.LI "\fBSTRING\fR"
A character string specifying the option
to be set.
Valid options are as follows:
.sp
\&'WIN=ON' or 'WIN=OFF'
.br
\&'SPA=ON' or 'SPA=OFF'
.sp
WIN defines the portion of the frame that will receive the histogram.
SPA determines the spacing between histogram bars.
.LI "\fBARRAY\fR"
A real array of length LARR.
.LI "\fBLARR\fR"
Dimension of ARRAY.
.LE
.ti -1.5i
.sp
The following arrays may be defined by this routine:
.sp
Windowing:
.>>
STRING is 'WIN=ON'
LARR = 4
ARRAY(1) = XMIN
ARRAY(2) = XMAX
ARRAY(3) = YMIN
ARRAY(4) = YMAX
.<<
.ne 3
.sp
Assumptions:  These coordinates define a rectangular region of the 
total frame where 
the current histogram is to be plotted.  The range of allowed values is as 
follows:
.sp
0. \(<= XMIN < XMAX \(<= 1.
.br
0. \(<= YMIN < YMAX \(<= 1.
.sp
.hw data-set
For example, XMIN=0., XMAX=.5, YMIN=.5, and YMAX=1. would specify the
upper left quadrant of the frame.
.sp
IF 'WIN=OFF', the default window for the histogram will be the entire frame.
.sp
Example:
.>>
.sf
REAL WINDOW(4)
DATA WINDOW /.3,.7,.3,.7/
CALL HSTOPR(\&'WIN=ON',WINDOW,4)
.<<
.ef
Spacing:
.>>
STRING is 'SPA=ON'
LARR = 2
ARRAY(1) = SPAC
ARRAY(2) = OVERLP
.<<
SPAC \(em Real value used to set spacing of histogram bars; valid
values are 0.0 (no spacing) to 4.0 (maximum spacing).
Default spacing is SPAC = 2.0.  If \&'SPA=OFF', the result
is the same as if SPAC = 0.0, and the value supplied in
ARRAY(1) is ignored by HISTGR.  Lines will be drawn
around the histogram bars when \&'SPA=OFF' by default.
These may not be visible if \&'SHA=ON' and the first
color index is set to white in the color table; it is
a good idea to set color index 1 to gray if \&'SPA=OFF',
and \&'SHA=ON' (for terminals without color capability, this
does not apply; see notes on \&'SHA' above). If IFLAG = 3,
there is a minimum amount of spacing supplied, even if
SPAC = 0.0, to allow room for the second dataset histogram
bars.
.sp
OVERLP \(em Real value used to set overlap of adjacent histogram
bars when comparing two datasets in one histogram;
valid values are -4.0 (maximum overlap) to 4.0 (little
or no overlap).  OVERLP applies only when IFLAG = 3.
Default overlap is OVERLP = 0.0.  If \&'SPA=OFF',
OVERLP is ignored by HISTGR.  If no overlap is desired,
set OVERLP to 4.0, and SPAC to 3.0 or greater.
.sp
Example:
.>>
.sf
REAL ARRAY (2)
IFLAG = 3
ARRAY(1) = 2.0
ARRAY(2) =-1.5
CALL HSTOPR('SPA=ON',ARRAY,2)
.<<
.ef
The above example would cause two sets of histograms to have overlapping
bars for comparison purposes.
.ti -1.5i
.hD "SUBROUTINE HSTOPC"
.H 3 "Purpose"
To specify various CHARACTER variables to be used by the
HISTGR package.
.H 3 "Usage"
CALL HSTOPC (STRING,STRNG2,NUMBER,ILCH)
.H 3 "Arguments"
.VL .9i
.LI "\fBSTRING\fR"
A character string specifying which
option is to be set.
Valid options are as follows:
.nf
.sp
\&'FOR=ON' or 'FOR=OFF'
\&'TIT=ON' or 'TIT=OFF'
\&'LAB=ON' or 'LAB=OFF'
\&'FQN=ON' or 'FQN=OFF'
\&'CHR=ON', or 'CHR=OFF'
.fi
.sp
By choosing the ON form for an option, the user can override the
default setting of that option.
A subsequent call to HSTOPC with the OFF form for an option
returns the option to the default setting.
All defaults are listed at the end of the HISTGR documentation.
.LI "\fBSTRNG2\fR"
A character string up to 45 characters long.
.sp
This argument depends upon the ON form being selected for an option.
For example, if 'TIT=ON',
a main title is input through argument STRNG2.
.LI "\fBNUMBER\fR"
Integer variable, which applies to the following three options:
.sp
\&'FOR=ON'   The maximum number of class intervals (histogram
bars) that will be labeled.
.sp
\&'FOR=OFF'  Defaults to 9 labels for vertical bars and to 15 labels
for horizontal bars.
.sp
\&'CHR=ON'   Must be set to NCLASS, an argument of the subsequent call 
to be made to HISTGR.
.sp
NUMBER is not used under any other option setting.
.sp
Calls to HSTOPC with both 'FOR=ON' and 'CHR=ON'
may be performed in any order; the parameters set by NUMBER are
mutually exclusive in either case.
.LI "\fBILCH\fR"
An integer variable specifying the number of
characters in each label of a class interval (histogram bar).
This argument is only used with 
the \&'CHR=ON' option. 
ILCH cannot be greater than 15.
.LE
.sp
.ti -1.5i
The following options are defined by this subroutine:
.VL .6i
.LI "FOR"
Format for class labels.
.br
The 'FOR=OFF' default is \&'(G10.3)'.  Although class values
are real numbers, integer formats are allowed,
in which case HISTGR will convert from real to
integer before plotting labels.
.LI "TIT"
A main title.
.br
The 'TIT=OFF' default is no title.
.LI "LAB"
A label for class interval (histogram bar) axis.
.br
The 'LAB=OFF' default value is \&'CLASS INTERVALS' 
when the HSTOPL option 'MID=OFF'
is selected, and
\&'CLASS MIDVALUES' otherwise.
.sp
In order to delete this axis label,
select 'LAB=ON' for STRING and 'NOLABEL' for STRNG2.
.LI "FQN"
The frequency axis label.
.br
The 'FQN=OFF' default value is \&'FREQUENCY'.
.sp
In order to delete this axis label, select 'FQN=ON'
for STRING and 'NOLABEL' for STRNG2.
.LI "CHR"
Character labels.
.br
Use a character string containing ILCH*NUMBER
characters to specify alphanumeric class
intervals (histogram bars).
This is a packed string of ILCH characters per class interval label.
.sp
The character string must contain NUMBER (=NCLASS)
class interval labels, even though all may not be used.
See the definition of argument NUMBER.
.LE
.H 3 Example
.sf
PARAMETER (NCLASS=12, ILCH=3)
CHARACTER*27 LABEL
CALL HSTOPC (\&'TIT=ON',\&'MONTHLY PRECIPITATION in 1987',12,3)
LABEL = \&'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'
CALL HSTOPC (\&'CHR=ON',LABEL,12,3)
CALL HSTOPC (\&'FOR=ON','(F3.0)',12,3)
.ef

In the above example, there will be 12
alphanumeric class labels, each containing
3 characters to specify the months of
the year. The title indicates the histogram
will depict monthly precipitation in 1987.
.sp
The call to HSTOPC with \&'FOR=ON' and NUMBER = 12
overrides the default number (9) of labels that would be plotted.
Note that the '(F3.0)' format is ignored because 'CHR=ON'.
.ti -1.5i
.hD "SUBROUTINE HSTOPI"
.H 3 "Purpose"
To specify various INTEGER variables to be used by the
HISTGR package.
.H 3 "Usage"
CALL HSTOPI (STRING, PARAM1, PARAM2, ICOL, LCOL)
.H 3 "Arguments"
.VL .9i
.LI "\fBSTRING\fR"
A character string specifying which option is to be set.
Valid options are as follows:
.sp
\&'COL=ON' or 'COL=OFF'
.br
\&'CLA=ON' or 'CLA=OFF'
.sp
By choosing the ON form for an option,
the user can override the default setting of that option.
A subsequent call to HSTOPI with the OFF form for an option
returns the option to the default setting.
All defaults are listed at the end of the HISTGR
documentation.
.LI "\fBPARAM1\fR"
Integer variable used to set character height of
class labels when \&'CLA=ON';
1 = small, 2 = medium, 3 = large;
default is 2 when 'CLA=OFF'.
.LI "\fBPARAM2\fR"
Integer variable used to set orientation of class
labels, from 0 (horizontal) to 90 (vertical) degrees
when \&'CLA=ON'.
Default is 0 degrees when 'CLA=OFF'.
.sp
Not used when 'COL=ON' or "COL=OFF'.
.LI "\fBICOL\fR"
Integer array containing values defining color
indices 1-8, for use with \&'COL=ON'.
.sp
The eight components of the plot for which color indices can be set
are as follows:
.sp
.nf
ICOL(1) = color index used for shading rectangles.
ICOL(2) = color index used for shading second set of rec-
~~~~~~~~~~~~~~~tangles (comparing two datasets when IFLAG = 3).
ICOL(3) = color index used for rectangle outlines.
ICOL(4) = color index used for drawing axes.
ICOL(5) = color index used for drawing median line.
ICOL(6) = color index used for text output (labels).
ICOL(7) = color index used for title.
ICOL(8) = color index used for drawing perimeter.
.sp
The default color index is 1 for all (when \&'COL=OFF').
.fi
.sp
If 'COL=ON', the color indices and their associated colors are
as follows. (These may be changed by specifying an RGB color table
prior to your call to HSTOPI.)
.br
.>>
.sp -1
~~0 = BLACK
~~1 = WHITE
~~2 = RED
~~3 = GREEN
~~4 = BLUE
~~5 = CYAN
~~6 = MAGENTA
~~7 = YELLOW
~~8 = ORANGE
.<<
.sp -1
ICOL is not used when 'CLA=ON' or "CLA=OFF'.
.LI "\fBLCOL\fR"
Integer variable specifying length of array ICOL.
ICOL must be set to 8.
.sp .5
LCOL is not used when 'CLA=ON' or 'CLA=OFF'.
.LE
.sp
.ti -1.5i
The default values for all the options are as follows:
.sp .5
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
.ll \n(34u*1u/3u
.if \n(.l<\n(81 .ll \n(81u
.in 0
a labeled percent axis is drawn on the right side of the histogram (or on top if horizontal).
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
.ll \n(34u*1u/3u
.if \n(.l<\n(81 .ll \n(81u
.in 0
labels are placed at the midpoint of each histogram bar.
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
.ll \n(34u*1u/3u
.if \n(.l<\n(81 .ll \n(81u
.in 0
lines corresponding to plotted scale values are not
drawn on the histogram.
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
.ll \n(34u*1u/3u
.if \n(.l<\n(81 .ll \n(81u
.in 0
a line denoting the median of all points is not drawn on 
the histogram.
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
.ll \n(34u*1u/3u
.if \n(.l<\n(81 .ll \n(81u
.in 0
the histogram will be drawn within the maximum viewport 
of 0.0 to 1.0 in both the horizontal and vertical dimensions.
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
.ll \n(34u*1u/3u
.if \n(.l<\n(81 .ll \n(81u
.in 0
default values: spacing = 2.0, overlap = -1.0
This will yield spacing between bars on a single histogram
and overlap of bars in the comparison of two histograms.
.br
.di
.nr f| \n(dn
.nr f- \n(dl
..
.ec \
.eo
.am 81
.br
.di g+
.35
.ft \n(.f
.ll \n(34u*1u/3u
.if \n(.l<\n(81 .ll \n(81u
.in 0
label = 'CLASS INTERVALS' or 'CLASS MIDVALUES' if 'MID=ON'.
.br
.di
.nr g| \n(dn
.nr g- \n(dl
..
.ec \
.35
.nf
.ll \n(34u
.nr 80 0
.nr 38 \w\fBHSTOPL\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'HOR=OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PER = ON'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'MID = ON'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'SHA = ON'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'DRL = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'MED = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'PRM = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'FRA = ON'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LIS = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBHSTOPR\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'WIN = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'SPA = ON'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBHSTOPC\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'FOR = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'TIT = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'LAB = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'FQN = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CHR = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\fBHSTOPI\fR
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'COL = OFF'
.if \n(80<\n(38 .nr 80 \n(38
.nr 38 \w\&'CLA = ON'
.if \n(80<\n(38 .nr 80 \n(38
.80
.rm 80
.nr 81 0
.nr 38 \whistogram bars will be vertical.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wthe histogram rectangles are shaded.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wa perimeter around the histogram is not drawn.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wa frame advance will occur after the call to HISTGR.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnothing is printed on the standard output unit.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wclass label format = \&'(G10.3)'.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wno title.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wfrequency label = 'FREQUENCY'.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wnumeric (real or integer) class labels.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wcolor index 1 is used for all output.
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \wdefault values: 2 = medium, 0 = horizontal labels.
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
.nr 38 \n(e-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(f-
.if \n(81<\n(38 .nr 81 \n(38
.nr 38 \n(g-
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
.nr #I \n(.i
.in +(\n(.lu-\n(TWu-\n(.iu)/2u
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
.in -.25i
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBHSTOPL\fR\h'|\n(41u'
.in +.25i
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'HOR=OFF'\h'|\n(41u'histogram bars will be vertical.
.ne \n(a|u+\n(.Vu
.if (\n(a|+\n(#^-1v)>\n(#- .nr #- +(\n(a|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PER = ON'\h'|\n(41u'
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
.ne \n(b|u+\n(.Vu
.if (\n(b|+\n(#^-1v)>\n(#- .nr #- +(\n(b|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'MID = ON'\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'SHA = ON'\h'|\n(41u'the histogram rectangles are shaded.
.ne \n(c|u+\n(.Vu
.if (\n(c|+\n(#^-1v)>\n(#- .nr #- +(\n(c|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'DRL = OFF'\h'|\n(41u'
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
.ne \n(d|u+\n(.Vu
.if (\n(d|+\n(#^-1v)>\n(#- .nr #- +(\n(d|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'MED = OFF'\h'|\n(41u'
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
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'PRM = OFF'\h'|\n(41u'a perimeter around the histogram is not drawn.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'FRA = ON'\h'|\n(41u'a frame advance will occur after the call to HISTGR.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LIS = OFF'\h'|\n(41u'nothing is printed on the standard output unit.
.sp .5
.in -.25i
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBHSTOPR\fR\h'|\n(41u'
.in +.25i
.ne \n(e|u+\n(.Vu
.if (\n(e|+\n(#^-1v)>\n(#- .nr #- +(\n(e|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'WIN = OFF'\h'|\n(41u'
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
.ne \n(f|u+\n(.Vu
.if (\n(f|+\n(#^-1v)>\n(#- .nr #- +(\n(f|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'SPA = ON'\h'|\n(41u'
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
.sp .5
.in -.25i
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBHSTOPC\fR\h'|\n(41u'
.in +.25i
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'FOR = OFF'\h'|\n(41u'class label format = \&'(G10.3)'.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'TIT = OFF'\h'|\n(41u'no title.
.ne \n(g|u+\n(.Vu
.if (\n(g|+\n(#^-1v)>\n(#- .nr #- +(\n(g|+\n(#^-\n(#--1v)
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'LAB = OFF'\h'|\n(41u'
.mk ##
.nr 31 \n(##
.sp |\n(##u-1v
.nr 37 \n(41u
.in +\n(37u
.g+
.in -\n(37u
.mk 32
.if \n(32>\n(31 .nr 31 \n(32
.sp |\n(31u
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'FQN = OFF'\h'|\n(41u'frequency label = 'FREQUENCY'.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CHR = OFF'\h'|\n(41u'numeric (real or integer) class labels.
.sp .5
.in -.25i
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\fBHSTOPI\fR\h'|\n(41u'
.in +.25i
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'COL = OFF'\h'|\n(41u'color index 1 is used for all output.
.ta \n(80u \n(81u 
.nr 31 \n(.f
.nr 35 1m
\&\h'|\n(40u'\&'CLA = ON'\h'|\n(41u'default values: 2 = medium, 0 = horizontal labels.
.fc
.nr T. 1
.T# 1
.in \n(#Iu
.35
.rm a+
.rm b+
.rm c+
.rm d+
.rm e+
.rm f+
.rm g+
.TE
.if \n-(b.=0 .nr c. \n(.c-\n(d.-55
