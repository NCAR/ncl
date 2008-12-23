.TH HSTOPL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
HSTOPL - Turns options off and on for the Histogram utility.
.SH SYNOPSIS
CALL HSTOPL (STRING)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_hstopl (char *string)
.SH DESCRIPTION
.IP STRING 12
Character, input -- Selects an internal parameter.  The
possibilities are:
.sp
   \'HOR=ON\' or \'HOR=OFF\'
.br
   \'PER=ON\' or \'PER=OFF\'
.br
   \'MID=ON\' or \'MID=OFF\'
.br
   \'SHA=ON\' or \'SHA=OFF\'
.br
   \'DRL=ON\' or \'DRL=OFF\'
.br
   \'MED=ON\' or \'MED=OFF\'
.br
   \'NMV=ON\' or \'NMV=OFF\'
.br
   \'PMV=ON\' or \'PMV=OFF\'
.br
   \'PRM=ON\' or \'PRM=OFF\'
.br
   \'FRA=ON\' or \'FRA=OFF\'
.br
   \'LIS=ON\' or \'LIS=OFF\'
.br
   \'DEF=ON\' or \'DEF=OFF\'
.sp
The following options are defined by this subroutine:
.RS
.IP HOR 8
The histogram bars are drawn horizontally if \'HOR=ON\',
vertically if \'HOR=OFF\'.  The default is \'HOR=OFF\'.
.IP PER 8
If \'PER=ON\', a labeled percent axis is drawn on the
right side of the histogram (top if \'HOR=ON\'.)
If \'PER=OFF\', no percent axis is drawn.
The default is \'PER=ON\'.
.sp
This places a second axis on the opposite side of the
frame from the normal frequency axis.
.IP MID 8
If \'MID=ON\', class labels are placed at the midpoint
of each class interval. If \'MID=OFF\', the class
interval end points are labeled. (MID defaults to ON if
IFLAG = 2 or 3.)  The default is \'MID=ON\'.
.IP SHA 8
If \'SHA=ON\', the histogram bars are shaded.
If \'SHA=OFF\', the bars are not shaded.
The default is \'SHA=ON\'.
.sp
Devices will vary as to how they interpret shading.
.IP DRL 8
If \'DRL=ON\', lines are drawn through the histogram
bars where Y-axis tick marks would occur.  When
comparing two datasets (IFLAG=3), lines are only
drawn through the bars associated with the first
dataset.  If \'DRL=OFF\', no lines are drawn.
The default is \'DRL=OFF\'.
.IP MED 8
If \'MED=ON\', a line is drawn through the median
value of all points. If \'MED=OFF\', this line is not
drawn. MED does not apply when assigning Y-values to
X-values; it is valid only for IFLAG = 1 or 2.
The default is \'MED=OFF\'.
.IP NMV 8
If \'NMV=ON\', histogram bar percentages will be
calculated with respect to the number of input
data values (NPTS) minus the number of detected
missing values (MISS), or NMVA = NPTS - MISS.
If \'NMV=OFF\', histogram bar percentages will be
normalized to NPTS.
.IP PMV 8
If \'PMV=ON\', missing value counts will be written
on the plot.  If \'NMV=OFF\', they will not.
.IP PRM 8
If \'PRM=ON\', a perimeter is drawn around the histogram.
If \'PRM=OFF\', no perimeter is drawn.
The default is \'PRM=OFF\'.
.IP FRA 8
If \'FRA=ON\', the frame is advanced automatically after
the histogram is drawn.  If  \'FRA=OFF\', the frame is not
advanced, and the user must call FRAME.
The default is \'FRA=ON\'.
.IP LIS 8
If \'LIS=ON\', all the options along with their values
are printed on the standard output. If \'LIS=OFF\',
nothing is printed on the standard output.
The default is \'LIS=OFF\'.
.IP DEF 8
If \'DEF=ON\', ALL the options are reset to their default
values.  \'DEF=OFF\' has no effect.  All parameters begin
at their default values.
.RE
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
HSTOPL is called to toggle histogram options before entry
HISTGR is called to generate the histogram.
.SH EXAMPLES
Use the command "ncargex thstgr" to generate a three frame example
of various types of histograms.  "ncargex thstmv" will show three
examples of histograms with missing values in the input data.
.SH ACCESS
To use HSTOPL or c_hstopl, load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_c, preferably in that order.  
.SH MESSAGES
See the histogram man page for a description of all Histogram error
messages and/or informational messages.
.SH SEE ALSO
Online:
histogram, histogram_params, histgr, hstopc, hstopi, hstopr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
