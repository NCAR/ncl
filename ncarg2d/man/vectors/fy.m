.TH FY 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
FY - 
Given the X and Y coordinates of a point in the grid
coordinate system, the function FY returns the Y coordinate
of the point in user coordinate space.
.SH SYNOPSIS
CALL FY (X,Y) 
.SH DESCRIPTION 
.IP X 12
(REAL, input): The X coordinate of a vector location in
the grid coordinate system.
.IP Y 12
(REAL, input): The Y coordinate of a vector location in
the grid coordinate system.
.SH USAGE
The user does not invoke the function, FY, directly. In
older versions of NCAR Graphics, the user modified the
functions, FX and FY, in order to implement custom mappings
from grid coordinates to user coordinates. These functions
were used throughout NCAR Graphics, not just in the
Streamlines utility. For compatibility with existing code,
calls to any of the Streamlines primary entry points
predating Version 3.2 (EZSTRM or STRMLN), by default use FX
and FY to map from grid to user coordinates. However, by
appropriately setting the compatibility mode parameter,
CPM, the user can choose whether the FX and FY functions or
the new mapping routines are invoked when using any of the
primary entry points supported by Streamlines.
.sp
Unlike the Version 3.2 mapping routines, whose input
coordinates are in the data coordinate system, FY takes
input in the gird coordinate system. Therefore, any
required conversions into the data coordinate system must
be performed within the function prior to the mapping into
user coordinates. The common block STMAP, available for
inclusion within the FY routine, supplies the information
necessary to perform the conversion into data coordinate
space, as well as for the mapping from data to user
coordinates. The default version of FY simply performs an
identity mapping from grid to user coordinate space.
.SH ACCESS
To use FY, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
vectors,
ezvec,
fx,
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
vvumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
