.TH HISTGR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
HISTGR - Plots a histogram with various options including
specification of class values, spacing between histogram bars,
shading of bars, windowing (i.e. scaling), specification of
color, labels, titles, etc.  Data values are partitioned into
classes; histogram bars represent either number of occurrences
within each class, or a Y-value associated with that class
(user choice).  Options are set by calls to subroutines HSTOPL,
HSTOPR, HSTOPC, and HSTOPI before the call to HISTGR.
.SH UTILITY
This routine is part of the Histogram utility in NCAR Graphics. To see
the overview man page for this utility, type "man histogram".
.SH SYNOPSIS
CALL HISTGR (DAT1,NDIM,NPTS,IFLAG,CLASS,NCLASS,WRK,NWRK)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_histgr (float *dat1, int ndim, int npts, 
.br
int iflag, float *class, int nclass, float *wrk, int nwrk)
.SH DESCRIPTION
.IP DAT1 12
Two dimensional real array containing data of one of
two types, either values to be collected into class
intervals before plotting, or values which have already
been assigned to class intervals and only need to be
displayed. See argument IFLAG for a more complete
description of HISTGR input data options. DAT1 is
dimensioned: DAT1(NDIM,2).
.IP NDIM 12
The size of the first dimension of DAT1 as set in the
dimension statement of the calling program.
.IP NPTS 12
Number of values actually stored into DAT1 on this
call. NPTS must always be less than or equal to NDIM.
.IP IFLAG 12
An integer flag which selects one of four options
provided by the HISTGR utility. The options are:
.sp
0    A single array of length NPTS is loaded
     into the DAT1 array. HISTGR computes NCLASS
     equally sized class intervals that vary
     from the minimum value in DAT1 to the
     maximum value in steps of (MAX-MIN)/NCLASS.
.sp
     All values of DAT1 that fall in each
     class interval are separately accumulated
     for that interval. The final tabulations
     are plotted as a histogram of NCLASS bars.
     The bar height can be labeled with the
     number of points that fall within this
     particular class interval (bin size), or
     it can be given as a percentage of the
     number of values input, NPTS.
.sp
     Note that under this option the user
     has no control over the range of the
     class intervals.  They are internally
     determined from the range of the data.
.sp
1    This option is similar to the IFLAG = 0
     option except that the user can select
     the range of the class intervals into
     which the data are collected. For example,
     say the user wants to collect the number
     of occurrences of the DAT1 values that
     fall within 5 equally spaced intervals
     in the value range from 0. to 10. The
     user would then input NCLASS+1 class
     interval end points into array CLASS,
     namely 0., 2., 4., 6., 8., and 10.
     These values need not be entered in
     monotonically increasing order and
     need not be equally spaced.
.sp
2    This option allows the user to enter
     and display data which has already
     been accumulated into class intervals,
     i.e., already available histograms.
     The data input to DAT1 thus have
     percentage of total, or number of
     occurrences values. In this case the
     number of points in DAT1, NPTS, is
     equal to the number of class intervals
     (histogram bars), NCLASS. The NCLASS
     class interval midpoints are loaded
     into array CLASS. They do not have
     to be of equal width.
.sp
3    This option is the same as option
     IFLAG = 2 except that two histograms
     can be displayed for comparison
     purposes. The first histogram is
     loaded into DAT1(NPTS,1). The second
     histogram is loaded into DAT1(NPTS,2).
     The first histogram can partially
     shade or obscure the second histogram
     by the appropriate selection of the
     SPAC and OVERLP options.  Note that
     NPTS = NCLASS when IFLAG = 2 or 3.
.IP CLASS 12
Real array containing class values, dimensioned
(NCLASS+1). This array has the following IFLAG
dependencies:
.sp
IFLAG = 0 CLASS is not used.
.sp
IFLAG = 1 NCLASS+1 class interval end points
        are loaded into array CLASS in a
        monotonically increasing order.  The
        intervals need not be of equal width.
.sp
IFLAG = 2 NCLASS midpoint intervals are loaded
        into array CLASS. They must be in
        monotonically increasing order, but
        need not be of equal widths. The
        histogram bars will however be
        displayed with equal widths.
.sp
IFLAG = 3 Same as for IFLAG = 2.
.IP NCLASS 12
Number of class intervals (histogram bars) specified.
NCLASS must be .GE. 1.
.IP WRK 12
Real scratch array, dimensioned by NWRK in the
dimension statement of the calling program.
.IP NWRK 12
The dimension size of array WRK determined from:
NDIM + 3 * (NCLASS + 1)
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exceptions:
.sp
.IP dat1 12
Two dimensional real array dimensioned: dat1(2,ndim).
.IP ndim 12
The size of the second dimension of dat1 as set in the
dimension statement of the calling program.
.SH USAGE
Many parameters which affect the output histogram can be set before this
routine is called.  See the histogram_params man page for a short
functional description of all available parameters.  For more detail
on a specific option, see the man pages of the parameter setting routines
(HSTOPC, HSTOPI, HSTOPL, or HSTOPR) used to set the parameters defining
that option.
.SH EXAMPLES
Use the command "ncargex thstgr" to generate a three frame example
of various types of histograms.  "ncargex thstmv" will show three
examples of histograms with missing values in the input data.
.SH ACCESS
To use HISTGR or c_histgr, load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_c, preferably in that order.  
.SH MESSAGES
See the histogram man page for a description of all Histogram error
messages and/or informational messages.
.SH SEE ALSO
Online:
histogram, histogram_params, hstopc, hstopi, hstopl, hstopr, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
