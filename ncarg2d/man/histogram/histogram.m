.TH HISTOGRAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Histogram - this utility is used to generate histograms.
Various options are available including the
specification of class values, spacing between histogram bars,
shading of bars, windowing (i.e. scaling), specification of
color, labels, titles, etc.  Data values are partitioned into
classes; histogram bars represent either number of occurrences
within each class, or a Y-value associated with that class
(user choice).  Options are set by calls to subroutines HSTOPL,
HSTOPR, HSTOPC, and HSTOPI before the call to HISTGR generates
the histogram.
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
To use this utility load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_loc, preferably in that order.  To use the C bindings, load the
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, and
ncarg_loc, preferably in that order.
.SH MESSAGES
The possible error messages are as follows:
.sp
HISTGR - NPTS LESS THAN 1
.sp
The argument NPTS, number of binable values, has been entered in error.
.sp
HISTGR - NCLASS LESS THAN 1
.sp
NCLASS, the number of histogram bars, has been entered in error.
.sp
HISTGR - NPTS .GT. NDIM
.sp
The argument NPTS, number of binable values, is larger than the array
dimension size.  Reduce NPTS or modify the code to increase the
array dimension size.  Set NDIM in the code to the new array dimension size.
.sp
HISTGR - NWRK .LT. NDIM+3*NCLASS+3
.sp
The argument NWRK, the dimension size of the work array, is incorrect.
Recompute it according to the above formula.
.sp
HISTGR - NCLASS .NE. NPTS (IFLAG=2)
.sp
IFLAG = 2 signals a special case in which the input data has already
been accumulated into class intervals.  Therefore, the number of
data values, NPTS, and the number of histogram bars, NCLASS, must be equal.
.sp
HISTGR - NCLASS .NE. NPTS (IFLAG=3)
.sp
IFLAG = 3 signals a special case in which the input data has already
been accumulated into class intervals.  Therefore, the number of
data values, NPTS, and the number of histogram bars, NCLASS, must be equal.
(IFLAG = 3 differs from the IFLAG = 2 case only in the fact that two
histograms are to be displayed instead of one.)
.sp
HISTGR - NCLASS MUST EQUAL NPTS
.sp
This is a redundant test on whether the number of input data values, NPTS,
equals the number of bars, NCLASS, when the IFLAG option is 2 or 3.
.sp
HISTGR - IFLAG NOT SPECIFIED OR INVALID
.sp
HISTGR argument IFLAG is not one of the legal values of 0, 1, 2, or 3.
.sp
HISTGR - MIN. X-VALUE .EQ. MAX. X-VALUE
.sp
A histogram has no width.  This probably means the values of HISTGR argument
CLASS have been entered in error.
.sp
HISTGR - MAXIMUM Y-VALUE .EQ. 0.0
.sp
A histogram has no data.  The histogram is plotted with a Y range of .02.
This probably means the HISTGR argument DAT1 had all zero values.
.sp
HISTGR - WINDOW OPTION ERROR, RANGE IS 0. - 1.
.sp
The internal parameter options that define the part of the frame where
the histogram is to be drawn have been entered incorrectly.
.sp
HSTOPC - UNDEFINED OPTION
.br
HSTOPI - UNDEFINED OPTION
.br
HSTOPL - UNDEFINED OPTION
.br
HSTOPR - UNDEFINED OPTION
.sp
Either an internal parameter name or value is incorrect.  Check
the spelling in the calls to whichever routine is cited.
.sp
HSTOPI - LCOL MUST EQUAL 8
.sp
The number of color indices input to HSTOPI cannot exceed 8.
.sp
HSTOPR - INVALID SPACING PARAMETER
.sp
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
histgr, hstopc, hstopi, hstopl, hstopr, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
