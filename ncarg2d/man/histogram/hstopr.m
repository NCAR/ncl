.TH HSTOPC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
HSTOPR - specifys various REAL arrays to be used by the
the Histogram utility.
.SH SYNOPSIS
CALL HSTOPR (STRING,ARRAY,LARR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_hstopr (char *string, float *array, int larr)
.SH DESCRIPTION
.IP STRING 12
A character string specifying whether one of the two
possible HSTOPR options is to be set, or defaulted.
The possibilities are:
.sp
  'WIN=ON' or 'WIN=OFF'
.br
  'SPA=ON' or 'SPA=OFF'
.sp
WIN defines the portion of the frame that will receive
the histogram. SPA determines the spacing between
histogram bars.
.IP ARRAY 12
A real array of length LARR.
.IP LARR 12
Dimension of ARRAY.
.sp
The following arrays may be defined by this routine:
.sp
Windowing:
.sp
STRING is 'WIN=ON'
LARR = 4
ARRAY(1) = XMIN
ARRAY(2) = XMAX
ARRAY(3) = YMIN
ARRAY(4) = YMAX
.sp
Assumptions: These coordinates define a rectangular
region of the total frame in NDC's where the current
histogram is to be plotted. The range of allowed values
is as follows:
.sp
0. _ XMIN < XMAX _ 1.
0. _ YMIN < YMAX _ 1.
.sp
For example, XMIN=0., XMAX=.5, YMIN=.5, and YMAX=1.
would specify the upper left quadrant of the frame.
.sp
IF 'WIN=OFF', the default window for the histogram will
be the entire frame.
.sp
SPAC - Real value used to set spacing of histogram
bars; valid values are 0.0 (no spacing) to 4.0 (maximum
spacing). Default spacing is SPAC = 2.0. If 'SPA=OFF',
the result is the same as if SPAC = 0.0, and the value
supplied in ARRAY(1) is ignored by HISTGR. Lines will
be drawn around the histogram bars when 'SPA=OFF' by
default. These may not be visible if 'SHA=ON' and the
first color index is set to white in the color table;
it is a good idea to set color index 1 to gray if
\&'SPA=OFF', and 'SHA=ON' (for terminals without color
capability, this does not apply; see notes on 'SHA'
above). If IFLAG = 3, there is a minimum amount of
spacing supplied even if SPAC = 0.0, to allow room for
the second dataset histogram bars.
.sp
OVERLP - Real value used to set overlap of adjacent
histogram bars when comparing two datasets in one
histogram; valid values are -4.0 (maximum overlap) to
4.0 (little or no overlap).  OVERLP applies only when
IFLAG = 3. Default overlap is OVERLP = 0.0. If
\&'SPA=OFF', OVERLP is ignored by HISTGR. If no overlap
is desired, set OVERLP to 4.0, and SPAC to 3.0 or
greater.
.sp
The default values are as follows:
.sp
.IP 'WIN 12
= OFF'   the histogram will be drawn within the maximum
viewport of 0.0 to 1.0 in both the horizontal and
vertical dimensions.
.sp
.IP 'SPA 12
= ON'    default values: spacing = 2.0, overlap = -1.0
This will yield spacing between bars on a single
histogram and overlap of bars in the comparison
of two histograms.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
HSTOPR is called to set parameters of type REAL before
entry HISTGR is called to generate the histogram.  Options
are the portion of the frame to receive the graphic,
and the amount of spacing and overlap of histogram bars.
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
      CALL HSTOPR('WIN=ON',ARR7,4)
C
      SPAC(1) = 3.0
      SPAC(2) = 0.0
      CALL HSTOPR('SPA=ON',SPAC,2)
.fi
.SH ACCESS
To use HSTOPR load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_loc, preferably in that order.  To use the C bindings, load the
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, and
ncarg_loc, preferably in that order.
.SH MESSAGES
See the histogram man page for a description of all histogram error
messages and/or informational messages.
.SH SEE ALSO
Online:
histogram, histogram_params, hstopc, hstopi, hstopl, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
