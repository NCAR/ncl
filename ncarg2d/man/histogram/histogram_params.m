.TH Histogram_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Histogram_params - This document briefly describes all histogram
internal parameters.  The Histogram utility has four parameter
setting routines: HSTOPC sets parameters of type CHARACTER;
HSTOPI sets parameters of type INTEGER; HSTOPL sets parameters
of type LOGICAL; HSTOPR sets parameters of type REAL.
.SH DESCRIPTION 
Parameters of type CHARACTER set in calls to entry HSTOPC:
.IP FOR 12
Format for class labels.  Default is \'(G10.3)\'.
.IP TIT 12
A main title of up to 96 characters.  Only 45 characters
are written per line so up to 3 lines may be written.
The default is no title.
.IP LAB 12
A label for the class interval (histogram bar) axis.
Default value is \'CLASS INTERVALS\' when
the HSTOPL option \'MID=OFF\' is selected, and \'CLASS
MIDVALUES\' when \'MID=ON\'.
.IP PTI 12
The percent axis label.  Default value when \'PTI=OFF\'
is \'PERCENT OCCURRENCE\' when IFLAG = 0, or 1, and
\'PERCENT of MAXIMUM\' when IFLAG = 2, or 3.
.IP FQN 12
The frequency axis label.  Default value is \'FREQUENCY\'.
.IP CHR 12
A concatenated string of class interval labels.
Default is that class labels will be internally computed
numeric values (real or integer.)
.SH
Parameters of type INTEGER set in calls to entry HSTOPI:
.IP COL 12
Eight components of the graphic can be assigned colors.
Default is that all components of the graphic are
assigned the color white (background color black.)
.IP CLA 12
Allows the size and orientation of class labels to be
altered.  Defaults are medium sized characters with a
horizontal alignment.
.SH
Parameters of type LOGICAL set in calls to entry HSTOPL:
.IP HOR 12
Defines whether histogram bars will be draw horizontally
or vertically.  Default is vertical bars.
.IP PER 12
Determines whether a percentage axis is drawn as a
second axis on the opposite side of the frame from
the normal frequency axis.  Default is a second axis.
.IP MID 12
Determines if class labels are placed at the midpoint
of each class interval, or if the class interval end
points are labeled.  Default is labels at the midpoints.
.IP SHA 12
Determines whether histogram bars are to be shaded.
Default is shaded bars.
.sp
Devices will vary as to how they interpret shading.
.IP DRL 12
Determines if lines are to be drawn through the
histogram bars where Y-axis tick marks would occur.
Default is no line.
.IP MED 12
Determines if a line is drawn through the median
value of all points.  Default is no median line.
.IP NMV 12
Determines if missing values are to be subtracted
from the number of input points before bin percentages
are computed.  The default is to subtract out the
missing values.  HSTOPR option MVA must be ON to
activate this option.
.IP PMV 12
Determines if the number of input points and the
number of missing values are to be printed on the
plot.  The default is to not print.
HSTOPR option MVA must be ON to
activate this option.
.IP PRM 12
Determines if a perimeter is to be drawn around the
histogram.  Default is no perimeter.
.IP FRA 12
Determines whether the frame is advanced after the
histogram is drawn.  Default is a frame advance.
.IP LIS 12
Determines if all the options along with their values
are to be printed on the standard output.
Default is no printed output.
.IP DEF 12
A global default switch which allows all parameters
to be returned to their default states in one call.
All parameters begin at their default values.
.SH
Parameters of type REAL set in calls to entry HSTOPR:
.IP WIN 12
Defines a rectangular region of the total frame in
NDC\'s where the current histogram is to be plotted.
For example, XMIN=0., XMAX=.5, YMIN=.5, and YMAX=1.
would specify the upper left quadrant of the frame.
Default is the total frame is used.
.IP SPA 12
Defines the amount of spacing between histogram bars and
and the amount of overlap of histogram bars.
Default values: spacing = 2.0, overlap = -1.0
This will yield spacing between bars on a single
histogram and overlap of bars in the comparison
of two histograms.
.IP MVA 12
Defines a special value which can be inserted in the data
stream to indicate that the true data value is missing, and
a testing tolerance which allows the special values to be
detected.  See also parameters PMV, and NMV.
Default values: special value = -9999., tolerance = 1.E-10.
.SH SEE ALSO
Online:
histogram, histgr, hstopc, hstopi, hstopl, hstopr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
