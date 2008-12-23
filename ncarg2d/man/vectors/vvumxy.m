.TH VVUMXY 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VVUMXY - 
The user may modify this routine to define a custom mapping
of vectors from a data coordinate system aligned with the
natural boundaries of the vector field to the uniform
normalized device coordinate (NDC) system suitable for
generating a plot on an output device. It has same
parameters as the internal Vectors routine, VVMPXY, used
for the predefined mappings employed when the MAP parameter
has a value between 0 and 2.
.SH SYNOPSIS
CALL VVUMXY (X,Y,U,V,UVM,XB,YB,XE,YE,IST) 
.SH DESCRIPTION 
.IP X 12
(REAL, input) Location of the vector along the first
dimensional axis in the data coordinate system. When MAP is
0, this is the X Axis. If MAP is 1, it is the longitudinal
axis, and if MAP is 2, it is the radial axis. For other
values of MAP, those that cause VVUMXY to be invoked, the
interpretation is up to the author of the mapping routine.
.IP Y 12
(REAL, input) Location of the vector along the second
dimensional axis in the data coordinate system. When MAP is
0, this is the Y Axis. If MAP is 1, it is the latitudinal
axis, and if MAP is 2, it is the angular axis. For other
values of MAP, those that cause VVUMXY to be invoked, the
interpretation is up to the author of the mapping routine.
.IP U 12
(REAL, input) U component of the vector. If TRT is set to
1, the direction of the U component is tangent to the
direction of the first dimensional axis in the data
coordinate system at the location of the vector. If TRT is
set to 0, and MAP has a value of 0 or 2, the direction of
the U component is parallel to the horizontal (X axis) in
NDC space.
.IP V 12
(REAL, input) V component of the vector. If TRT is set to
1, the direction of the V component is normal to the
direction of the first dimensional axis in the data
coordinate system at the location of the vector. If TRT is
set to 0, and MAP has a value of 0 or 2, the direction of
the V component is parallel to the vertical (Y axis) in NDC
space.
.IP UVM 12
(REAL, input) Magnitude of the U and V components,
SQRT(U*U+V*V). Although this value could be calculated
within the routine, it is more efficient for the calling
routine to supply the value as an argument, since it is
needed for other purposes at a higher level.
.IP XB 12
(REAL, output) Location of the vector starting point
along the horizontal (X axis) in NDC space, before
adjustment based on the value of the vector positioning
parameter, VPO.
.IP YB 12
(REAL, output) Location of the vector starting point
along the vertical (Y axis) in NDC space, before adjustment
based on the value of the vector positioning parameter, VPO
.IP XE 12
(REAL, output) Location of the vector ending point along
the horizontal (X axis) in NDC space, before adjustment
based on the value of the vector positioning parameter, VPO
.IP YE 12
(REAL, output) Location of the vector ending point along
the vertical (Y axis) in NDC space before adjustment based
on the value of the vector positioning parameter, VPO
.IP IST 12
(REAL, output) Status of the vector mapping operation:
0 indicates success, negative values indicate that the
mapping failed; positive values are reserved and should not
be used by the implementor of a mapping routine.
.SH USAGE
The user does not call VVUMXY. Vectors calls it only when the
parameter MAP has a value other than 0, 1, or 2, the mappings handled
by Vectors internally. Note that unlike other user-modifiable mapping
routines in NCAR Graphics, such as CPMPXY, that map a single point
into the user coordinate system, this routine returns two points,
representing both ends of the vector, scaled for magnitude, in the
normalized device coordinate (NDC) system. The NDC system is used for
output because, as a coordinate system guaranteed to be rectangular
and uniform, it serves as a convenient reference system to help map
both vector magnitude and direction correctly. The term uniform, as
used in this discussion, means that an arbitrary numerical increment
along either the X or Y axis has the same length given any offset from
the coordinate system origin. The user coordinate system does not
qualify, because it may be log-scaled, or the X units may have a
different size from the Y units.
.sp
In order to implement a custom mapping, you must pick a unique mapping
code (a positive integer greater than 2), and then modify VVUMXY to
recognize and respond to the chosen code. In the standard distribution
of NCAR Graphics, this routine resides in the file, \'vvumxy.f\'.
VVUMXY has access to a common block called VVMAP that contains a
number of variables used to record the current transformation state.
In order to accommodate a variety of mapping implementations, VVMAP
provides more information than normally required. Consider the values
stored in VVMAP as strictly read-only.  One essential member of this
common block is IMAP, which contains the value currently assigned to
the MAP parameter.
.sp
When implementing a non-linear mapping, an iterative differential
technique will most likely be required. Look at the routine, VVMPXY,
in \'vvmpxy.f\', which handles the pre-defined mappings, for examples
of the method. Both the default transformation (MAP set to 0), in
order to account for possible log scaling of the user coordinate axes,
and also the Ezmap projection (MAP set to 1) use such a technique.
Basically the idea is that the vector components must be
proportionally reduced in size enough that an effectively
"instantaneous" angle can be calculated, although they must not become
so small that the calculation is adversely affected by the floating
point precision available for the machine. Additionally, checks must
be put in place to prevent the increment from stepping off the edge of
the coordinate system space. The pre-defined mappings step in the
opposite direction to find the angle whenever an increment in the
original direction would fall off the edge.
.SH ACCESS
To use VVUMXY, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
vectors,
vectors_params,
vvectr,
vvgetc,
vvgeti,
vvgetr,
vvinit,
vvrset,
vvsetc,
vvseti,
vvsetr,
vvudmv,
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
