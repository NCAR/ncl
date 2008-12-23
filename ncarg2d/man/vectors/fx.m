.TH FX 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
FX - 
Given the X and Y coordinates of a point in the grid
coordinate system, the function FX returns the X coordinate
of the point in user coordinate space.
.sp
FY - Given the X and Y coordinates of a point in the grid coordinate
system, the function FY returns the Y coordinate of the point in user
coordinate space. 
.SH STATUS
FX and FY are obsolete, and are supported only to provide
compatibility with old NCAR Graphics codes. These functions are
internally callable from four NCAR Graphics utilities -- the Conrec
and Conran families, and Vectors and Streamlines. Since the Conpack
utility has replaced Conrec and Conran for the most part, little needs
to be said about the use of FX and FY in those utilities. 
.sp
For Vectors and Streamlines, however, these routines have been
required in order to perform custom mappings of the data coordinate
space until Version 3.2, when new mapping routine were developed to
deal more robustly with non-linear transformations of vector
components. In order to ease the transition for users of the old
packages who depend on existing custom mappings using FX and FX,
however, both utilities have implemented a number of compatibility
levels. By modifying the compatibility mode parameter, CPM, you can
choose to map coordinates either using the old FX and FY routines, or
using the new Version 3.2 routines.
.sp
If you need to create new custom coordinate space mappings, you should
use the new mapping methods and not use FX and FY, since the internal
support for mappings based on these functions is frozen at the present
level. Any improvements in the code will be directed towards users of
the new mapping routines only.
.SH SYNOPSIS
USERX = FX (X,Y)
.br
USERY = FY (X,Y) 
.SH DESCRIPTION 
.IP X 12
(REAL, input): The X coordinate of a vector location in
the grid coordinate system.
.IP Y 12
(REAL, input) The Y coordinate of a vector location in
the grid coordinate system.
.SH USAGE
The user does not invoke the functions, FX and FY, directly. Instead
the functions are invoked internally by an NCAR Graphics utility in
order to convert a position in the grid coordinate system into user
coordinates. By default, calls to any of the primary entry points for
Vectors and Streamlines utilities that predate Version 3.2 (that is,
EZVEC, VELVEC, VELVCT, EZSTRM or STRMLN) use FX and FY to map from
grid to user coordinates. However, by appropriately setting the
compatibility mode parameter, CPM, the user can choose whether the FX
and FY functions or the new mapping routines are invoked when using
any of the primary entry points, current or obsolete, supported by
Vectors and Streamlines.
.sp
Unlike the Version 3.2 mapping routines, whose input coordinates are
in the data coordinate system, FX and FY take input in the grid
coordinate system. Therefore, any required conversions into the data
coordinate system must be performed within the function prior to the
mapping into user coordinates. Vectors and Streamlines both provide
common blocks (VVMAP and STMAP, respectively) that may be included by
the FX and FY functions to supply the information necessary to perform
the conversion into data coordinate space. No pre-defined mappings are
supported using FX and FY. The default versions of these functions
simply perform an identity mapping from grid to user coordinate space.
.sp
When Vectors employs FX and FY, it (alone) also requires the use of
two other functions, called MXF and MYF. Their usage is described
below:
.SH NAME
MXF - MXF is a user modifiable function that, given one end point of a
vector in both grid and metacode coordinates, returns the X coordinate of
the other end of the vector in metacode coordinates.
.sp
MYF - MYF is a user modifiable function that, given one end point of a
vector in both grid and metacode coordinates, returns the Y coordinate
of the other end of the vector in metacode coordinates.
.SH STATUS
Like FX and FY, MXF and MYF are obsolete and should not be employed when
creating new custom mapping code.
.SH SYNOPSIS
METAX = MXF (X,Y,U,V,SFX,SFY,MX,MY)
.br
METAY = MYF (X,Y,U,V,SFX,SFY,MX,MY)
.SH DESCRIPTION 
.IP X 12
(REAL, input): The X coordinate of a vector location in the grid
coordinate system.
.IP Y 12
(REAL, input) The Y coordinate of a vector location in the grid
coordinate system.
.IP U 12
(REAL, input) The U component of the vector at the data point
specified by arguments X and Y.
.IP V 12
(REAL, input) The V component of the vector at the data point
specified by arguments X and Y.
.IP SFX 12
(REAL, input) Scale factor used to convert the vector magnitude to a
length in metacode coordinates.
.IP SFY 12
(REAL, input) Scale factor used to convert the vector magnitude to a
length in metacode coordinates. In the current implementation this
value is the same as the value of SFX.
.IP MX 12
(INTEGER, input) X coordinate of the vector location in metacode
coordinates.
.IP MX 12
(INTEGER, input) Y coordinate of the vector location in metacode
coordinates.
.SH USAGE
The user does not invoke the functions, MXF and MYF, directly.
However, any time the Vectors utility is accessed with the
compatibility mode parameter set such that the FX and FY routines are
used to map the first endpoint of the vector, the functions MXF and
MYF are used to determine the second endpoint.  First FX and FY are
invoked to determine the vector location in user coordinates, then
this point is converted into metacode coordinates; MXF and MYF are
passed the coordinates of the point both in grid space and in metacode
space, along with the vector components and a scale factor used to
convert the vector magnitude into a length in the metacode coordinate
system.  
.sp
The default version of MXF simply multiplies the U component of the
vector (the component parallel to the X grid axis) by the scale
factor, SFX, and adds it to the X coordinate of the first point in
metacode coordinates, MX. MYF uses the same scale factor, SFX, but
multiplies by the V component and adds to the Y coordinate in metacode
coordinates. These values are respectively returned as the function
value by each routine. Note that if a mapping is anywhere non-linear,
the vector directional angle may change across the transformation, and
an iterative differential technique must be employed to map the second
endpoint of the vector. When creating a new mapping, the user is
strongly urged to use the user-modifiable routine VVUMXY, rather than
attempting to work with MXF and MYF.
.SH ACCESS
To use FX, FY, MXF, and MYF load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
vectors,
streamlines,
vectors_params,
streamlines_params,
vvumxy,
stuixy,
stumta,
stumxy.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
