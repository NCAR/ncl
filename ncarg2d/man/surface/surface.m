.TH Surface 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Surface - A utility for drawing perspective picture of a function of
two variables with hidden lines removed.
.SH SYNOPSIS
EZSRFC - Draws a perspective picture of a function of two
variables with hidden lines removed. The function is
approximated by a two-dimensional array of heights. Use EZSRFC
only if the entire array is to be drawn, the data points are
equally spaced in the X-Y plane, there are no stereo pairs, and
scaling is chosen internally.
.sp
PWRZS - A character-plotting routine for plotting characters in
three-space when using SRFACE.
.sp
SETR - Establishes certain constants so that SRFACE
produces a picture whose size changes with respect to the
viewer's distance from the object.  It can also be used
when making a movie of an object evolving in time to keep
it positioned properly on the screen, saving computer time
in the bargin.  Call it with r0 negative to turn off this
feature.
.sp
SRFACE - Draws a perspective picture of a function of
two variables with hidden lines removed. The function is
approximated by a two-dimensional array of heights.
.SH C-BINDING SYNOPSIS
c_ezsrfc
.br
c_pwrzs
.br
c_setr
.br
c_srface
.SH ACCESS 
To use Surface routines, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
surface_params,
ezsrfc,
pwrzs,
setr,
srface.
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
