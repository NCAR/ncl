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
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
