.TH Isosurface_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Isosurface_params - This document briefly describes all 
Isosurface internal parameters.
.SH DESCRIPTION 
Parameter descriptions, in alphabetical order, of all Isosurface
internal parameters follow. Each description begins with a line
giving the parameter name and the intrinsic FORTRAN type of the
parameter.
.IP "'IU' - Integer"
Number of extra slabs to be interpolated
between each pair of slabs perpendicular
to the U axis. Using a non-zero value
takes longer, but makes a better
picture. Negate the value to interpolate
the extra slabs but not draw them, which
has the effect of reducing errors due to
being able to see between slabs.
Attempting to give 'IU' a value less
than -10 will give it the value -10;
similarly, trying to give it a value
greater than 10 will give it the value
10. The default value is 0.
.IP "'IV' - Integer"
Like 'IU', but applies to extra slabs
perpendicular to the V axis. The default
value is 0.
.IP "'IW' - Integer"
Like 'IU', but applies to extra slabs
perpendicular to the W axis. The default
value is 0.
.IP "'RF' - Integer"     
Controls drawing of reference planes and
axes. Zero means "don't draw them";
non-zero means "draw them". The default
value is 1.
.IP "'RS' - Real"
Zero turns off the "relative size"
feature; the 3D grid box will be scaled
to fill almost the entire plotter frame.
Give 'RS' a non-zero value to turn on
the "relative size" feature and to
specify that distance from which the
box, when viewed from the direction
which makes its image largest, almost
fills the frame. Views from closer in
will give a bigger image and views from
farther out will give a smaller image.
This is useful when making movies with
the "camera" flying around the box. The
default value is 0.
.IP "'SL' - Real"
Segment length. When contours are
smoothed, this parameter specifies the
approximate length of the line segments
used to draw the smooth curves. Values
outside the range from 0.0001 to 0.1 will
be mapped to the nearer end of that
range. The default value is 0.01.
.IP "'SM' - Integer"
Screen model selector. Zero selects the
coarse screen model (128 x 128); 
non-zero selects a finer screen model (256 x
256). Using the former is faster, but
using the latter gives better pictures.
The default value is 0.
.IP "'ST' - Real"
Spline tension. If zero, turns off the
smoothing of contours. If non-zero,
turns the smoother on and determines the
tension on the splines used to do the
smoothing. Use of values greater than
about 15. has been observed to cause
overflow on some machines. The default
value is 0.
.IP "'SV' - Integer"
Special value. The value zero turns off
the special-value feature. A non-zero
value turns on the feature and specifies
the special value itself. Regions filled
with this value are treated as being
outside the volume bounded by the
isosurface. Preliminary experiments seem
to indicate that sometimes the use of
special values works well and sometimes
it doesn't (depending on the pattern of
the special values in the field). The
default value is 0.
.IP "'VB' - Real"
The position of the bottom edge of the
viewport in which the isosurface is to
be displayed, expressed as a fraction
between 0 (the bottom edge of the frame)
and 1 (the top edge of the frame). The
default value is 0.
.IP "'VL' - Real"
The position of the left edge of the
viewport in which the isosurface is to
be displayed, expressed as a fraction
between 0 (the left edge of the frame)
and 1 (the right edge of the frame). The
default value is 0.
.IP "'VR' - Real"
The position of the right edge of the
viewport in which the isosurface is to
be displayed, expressed as a fraction
between 0 (the left edge of the frame)
and 1 (the right edge of the frame). The
default value is 1.
.IP "'VT' - Real"
The position of the top edge of the
viewport in which the isosurface is to
be displayed, expressed as a fraction
between 0 (the bottom of the frame) and
1 (the top edge of the frame). The
default value is 1.
.SH SEE ALSO
Online:
isgeti, isgetr, isseti, issetr
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
