.TH VVUMXY 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
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
The user does not call VVUMXY. Vectors calls it only when
the parameter MAP has a value other than 0, 1, or 2, the
mappings handled by Vectors internally. Note that unlike
other user-modifiable mapping routines in NCAR Graphics,
such as CPMPXY, that map a single point into the user
coordinate system, this routine returns two points,
representing both ends of the vector, scaled for magnitude,
in the normalized device coordinate (NDC) system. The NDC
system is used for output because, as a coordinate system
guaranteed to be rectangular and uniform, it serves as a
convenient reference system to help map both vector
magnitude and direction correctly. The term uniform, as
used in this discussion, means that an arbitrary numerical
increment along either the X or Y axis has the same length
given any offset from the coordinate system origin. The
user coordinate system does not qualify, because it may be
log-scaled, or the X units may have a different size from
the Y units.
.sp
Mappings the user chooses to implement are bound to be 
non-linear, since the linear mappings are handled by the
default setting of the MAP parameter. Under these
conditions, one cannot simply project both ends of the
vector. Essentially it is necessary to find the direction
of the vector at a point on a curve. The internal mapping
routine, VVMPXY, uses an iterative differential technique
to accomplish this task. The default version of VVUMXY
contains commented code offering a suggested approach that
could be adopted by the implementor of a custom mapping.
.sp
Note that the non-linear mappings can occur over two
separate transformations. Each must be handled separately.
The transformation between the data coordinate system and
the user coordinate system is the first. Pre-defined
mappings 1 and 2, the Ezmap and polar projections, are
examples. The second occurs in the transformation between
user and NDC space whenever the user system is not uniform,
as defined above. The default linear transformation can
handle vector mapping over log-scaled or unequal unit user
coordinates. However, the polar coordinate mapping will not
properly project vectors if log scaled user coordinates are
in effect. It is not an issue for the map projections,
since Ezmap always sets up a uniform user coordinate system.
.sp
A common block called VVMAP is made available to VVUMXY,
allowing access to a number of variables used to record the
current transformation state. More information is provided
than normally required to perform the mapping, but perhaps
in certain situations it may be useful. The values stored
in VVMAP should be treated as strictly read-only, or the
results will be, as the saying goes, "undefined".
Particularly important variables contained in this common
block include IMAP, which contains the value currently
assigned to the MAP parameter, and ITRT, which stores the
value currently assigned to the transformation type
parameter TRT.
.SH ACCESS
To use VVUMXY, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  
.SH SEE ALSO
Online:
vectors,
ezvec,
fx,
fy,
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
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
