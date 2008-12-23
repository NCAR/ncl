.TH HSTOPR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
HSTOPR - Specifies various REAL arrays to be used by the
the Histogram utility.
.SH SYNOPSIS
CALL HSTOPR (STRING,ARRAY,LARR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_hstopr (char *string, float *array, int larr)
.SH DESCRIPTION
.IP STRING 12
Character, input -- Selects an internal parameter.  The
possibilities are:
.sp
  \'WIN=ON\' or \'WIN=OFF\'
.br
  \'SPA=ON\' or \'SPA=OFF\'
.br
  \'MVA=ON\' or \'MVA=OFF\'
.sp
WIN defines the portion of the frame that will receive
the histogram. SPA determines the spacing between
histogram bars.  MVA requests a test to detect input
special values which denote missing data.
.IP ARRAY 12
Real, input -- It has a different use depending upon
which parameter is specified by argument STRING.  See the
examples following argument LARR.
.IP LARR 12
Integer, input -- Dimension of ARRAY.
.sp
The following arrays may be defined by this routine:
.sp
Windowing:
.sp
.in 20
.nf
STRING is \'WIN=ON\'
LARR = 4
ARRAY(1) = XMIN
ARRAY(2) = XMAX
ARRAY(3) = YMIN
ARRAY(4) = YMAX
.fi
.in -20
.IP ""
These coordinates define a rectangular
region of the total frame in NDC\'s where the current
histogram is to be plotted. The range of allowed values
is between 0. and 1. in both dimensions.
.sp
For example, XMIN=0., XMAX=.5, YMIN=.5, and YMAX=1.
would specify the upper left quadrant of the frame.
.sp
IF \'WIN=OFF\', the default window for the histogram will
be the entire frame.
.sp2
Spacing:
.sp
.nf
.in 20
STRING is \'SPA=ON\'
LARR = 2
ARRAY(1) = SPAC
ARRAY(2) = OVERLP
.fi
.in -20
.IP "" 
SPAC determines the spacing of histogram bars between 0.
(no spacing) and 4.  (maximum spacing).  Default spacing is
SPAC = 2.0.
.sp
OVERLP determines the overlap of adjacent histogram
bars when two datasets are compared.  Valid values are
\-4.0 (maximum overlap) to 4.0 (little or no overlap).
The Default is 0.
.sp
When STRING = \'SPA=OFF\', there is no spacing or overlap.
.sp2
Detecting missing values:
.sp
.nf
.in 20
STRING is \'MVA=ON\'
LARR = 2
ARRAY(1) = SETMVA
ARRAY(2) = SETEPS
.fi
.in -20
.IP "" 
SETMVA is the special value used to denote missing data.
The default value is -9999.
.sp
SETEPS is an epsilon to use in the special value test.
If the difference between a data point and SETMVA is
less than SETEPS, then the data point is assumed to
be a missing value.
The Default is 1.E-10.
.sp
When STRING = \'MVA=OFF\', there is no testing for missing values.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
HSTOPR is called to set parameters of type REAL before
entry HISTGR is called to generate the histogram.  Options
are the portion of the frame to receive the graphic,
and the amount of spacing and overlap of histogram bars.
.sp
For a complete list of parameters available
in this utility, see the histogram_params man page.
.SH EXAMPLES
Use the command "ncargex thstgr" to generate a three frame example
of histogram options.  The following code defined the window and
bar spacing of plot 1 on frame 3.
.sp2
.nf
C
C  Plot 1 goes into the top left quadrant of the frame.
C
C       ARR7 coordinates are XMIN, XMAX, YMIN, YMAX.
C
      ARR7(1) = 0.
      ARR7(2) = .5
      ARR7(3) = .5
      ARR7(4) = 1.
      CALL HSTOPR(\'WIN=ON\',ARR7,4)
C
      SPAC(1) = 3.0
      SPAC(2) = 0.0
      CALL HSTOPR(\'SPA=ON\',SPAC,2)
.fi
.sp
Example  "ncargex thstmv" shows three
examples of histograms with missing values in the input data.
.SH ACCESS
To use HSTOPR or c_hstopr, load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_c, preferably in that order.  
.SH MESSAGES
See the histogram man page for a description of all Histogram error
messages and/or informational messages.
.SH SEE ALSO
Online:
histogram, histogram_params, hstopc, hstopi, hstopl, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
