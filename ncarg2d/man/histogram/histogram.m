.TH Histogram 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Histogram - This utility is used to generate histograms.
Histograms or bar charts are used to show the distribution
of values in a like-sample dataset.  The individual
values are sorted by size into a number of value
ranges called class intervals.  The number of samples
out of the total which fall into a class interval are
represented as a histogram bar height.  The height of
each histogram bar is proportional to the number of
samples in that class interval.
.sp
Various options are available including the
specification of class values, spacing between histogram bars,
shading of bars, windowing (i.e. scaling), specification of
color, labels, titles, etc.  One can also detect and ignore
missing data which has been assigned a special value.
Options are set by calls to
routines HSTOPL, HSTOPR, HSTOPC, and HSTOPI.
A call to routine HISTGR then generates the histogram.
.SH SYNOPSIS
HISTGR - Generates histograms with many options available via
input arguments and internal parameter selections.
.sp
HSTOPC - Sets values of internal parameters of type character.
.sp
HSTOPI - Sets values of internal parameters of type integer.
.sp
HSTOPL - Sets values of internal parameters of type logical.
Specifically, HSTOPL is used to turn options "OFF", or "ON".
.sp
HSTOPR - Sets values of internal parameters of type real.
.SH C-BINDING SYNOPSIS
c_histgr
.br
c_hstopc
.br
c_hstopi
.br
c_hstopl
.br
c_hstopr
.SH ACCESS 
To use the Histogram C or Fortran routines, load the NCAR Graphics 
libraries ncarg, ncarg_gks, and
ncarg_c, preferably in that order.
.SH MESSAGES
The possible error messages are as follows:
.IP "HISTGR - NPTS LESS THAN 1"
The argument NPTS, number of binable values, has been entered in error.
.IP "HISTGR - NCLASS LESS THAN 1"
NCLASS, the number of histogram bars, has been entered in error.
.IP "HISTGR - NPTS .GT. NDIM"
The argument NPTS, number of binable values, is larger than the array
dimension size.  Reduce NPTS or modify the code to increase the
array dimension size.  Set NDIM in the code to the new array dimension size.
.IP "HISTGR - NWRK .LT. NDIM+3*NCLASS+3"
The argument NWRK, the dimension size of the work array, is incorrect.
Recompute it according to the above formula.
.IP "HISTGR - NCLASS .NE. NPTS (IFLAG=2)"
IFLAG = 2 signals a special case in which the input data has already
been accumulated into class intervals.  Therefore, the number of
data values, NPTS, and the number of histogram bars, NCLASS, must be equal.
.IP "HISTGR - NCLASS .NE. NPTS (IFLAG=3)"
IFLAG = 3 signals a special case in which the input data has already
been accumulated into class intervals.  Therefore, the number of
data values, NPTS, and the number of histogram bars, NCLASS, must be equal.
(IFLAG = 3 differs from the IFLAG = 2 case only in the fact that two
histograms are to be displayed instead of one.)
.IP "HISTGR - NCLASS MUST EQUAL NPTS"
This is a redundant test on whether the number of input data values, NPTS,
equals the number of bars, NCLASS, when the IFLAG option is 2 or 3.
.IP "HISTGR - IFLAG NOT SPECIFIED OR INVALID"
HISTGR argument IFLAG is not one of the legal values of 0, 1, 2, or 3.
.IP "HISTGR - MIN. X-VALUE .EQ. MAX. X-VALUE"
A histogram has no width.  This probably means the values of HISTGR argument
CLASS have been entered in error.
.IP "HISTGR - MAXIMUM Y-VALUE .EQ. 0.0"
A histogram has no data.  The histogram is plotted with a Y range of .02.
This probably means the HISTGR argument DAT1 had all zero values.
.IP "HISTGR - WINDOW OPTION ERROR, RANGE IS 0. - 1."
The internal parameter options that define the part of the frame where
the histogram is to be drawn have been entered incorrectly.
.IP "HSTOPC - UNDEFINED OPTION"
.IP "HSTOPI - UNDEFINED OPTION"
.IP "HSTOPL - UNDEFINED OPTION"
.IP "HSTOPR - UNDEFINED OPTION"
Either an internal parameter name or value is incorrect.  Check
the spelling in the calls to whichever routine is cited.
.IP "HSTOPI - LCOL MUST EQUAL 8"
The number of color indices input to HSTOPI cannot exceed 8.
.IP "HSTOPR - INVALID SPACING PARAMETER"
Either the spacing of histogram bars, or the overlap of bars from
two histograms, is outside of the legal range.  Valid values for
spacing of bars are 0.0 (no spacing) to 4.0 (maximum
spacing.)  Valid values
used to set overlap of adjacent
histogram bars when comparing two datasets in one
histogram are -4.0 (maximum overlap) to
4.0 (little or no overlap.)
.SH SEE ALSO
Online:
histgr, histogram_params, hstopc, hstopi, hstopl, hstopr, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
