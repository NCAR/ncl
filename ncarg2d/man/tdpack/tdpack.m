.TH Tdpack 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Tdpack - A set of routines allowing one to draw representations of
three-dimensional objects.
.SH SYNOPSIS
TDPACK (which stands for "Three Dimensional PACKage") is a collection of
routines for projecting objects from a 3-dimensional coordinate system
having U, V, and W axes to a 2-dimensional projection plane having X
and Y axes and/or for drawing the projections of those objects.  This
can be referred to somewhat loosely as "drawing objects in three dimensions".
.sp
Tdpack consists of the following routines:
.IP "Initialization Routines:" 6
.IP "  TDINIT -" 11
Define eye position, line of sight, up direction, and stereo flag.
.IP "  TDPARA -" 11
Define the reference parallelogram.
.IP "  TDCLRS -" 11
Define colors for TDPACK.
.IP "Parameter-Access Routines:" 6
.IP "  TDGETI -" 11
Get the integer value of an internal parameter.
.IP "  TDGETR -" 11
Get the real value of an internal parameter.
.IP "  TDGTRS -" 11
Get the definition of a specified rendering style.
.IP "  TDSETI -" 11
Set the integer value of an internal parameter.
.IP "  TDSETR -" 11
Set the real value of an internal parameter.
.IP "  TDSTRS -" 11
Define a specified rendering style.
.IP "Point-Transforming Routines:" 6
.IP "  TDPRPT -" 11
Project from 3-space to the projection plane.
.IP "  TDPRPA -" 11
Project from 3-space to the plane of the reference parallelogram.
.IP "  TDPRPI -" 11
Project from the plane of the reference parallelogram to 3-space.
.IP "Line-Drawing Routines:" 6
.IP "  TDLINE -" 11
Draw the projection of a line in 3-space.
.IP "  TDLNPA -" 11
Draw the projection of a line in the plane of the reference parallelogram.
.IP "Grid-Drawing Routines:" 6
.IP "  TDGRDS -" 11
Draw the grids on all the faces of a box in 3-space.
.IP "  TDGRID -" 11
Draw the grid on a particular face of a box in 3-space.
.IP "Label-Drawing Routines:" 6
.IP "  TDLBLS -" 11
Draw labels for all faces of a box in 3-space.
.IP "  TDLBLA -" 11
Draw labels for a particular face of a box in 3-space.
.IP "  TDPLCH -" 11
Draw characters in the plane of the reference parallelogram.
.IP "Surface-Drawing Routines:" 6
.IP "  TDDTRI -" 11
Draw triangles defined by a triangle list.
.IP "  TDSTRI -" 11
Add triangles defining a simple surface to a triangle list.
.IP "  TDITRI -" 11
Add triangles defining an isosurface to a triangle list.
.IP "  TDMTRI -" 11
Add triangles defining a 3D marker to a triangle list.
.IP "  TDCTRI -" 11
Order the triangles in a triangle list for proper rendering.
.IP "  TDSORT -" 11
Cut triangles with a plane perpendicular to one of the axes.
.IP "  TDOTRI -" 11
Generic sorting routine
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_tdinit
.br
c_tdpara
.br
c_tdclrs
.br
c_tdgeti
.br
c_tdgetr
.br
c_tdgtrs
.br
c_tdseti
.br
c_tdsetr
.br
c_tdstrs
.br
c_tdprpt
.br
c_tdprpa
.br
c_tdprpi
.br
c_tdline
.br
c_tdlnpa
.br
c_tdgrds
.br
c_tdgrid
.br
c_tdlbls
.br
c_tdlbla
.br
c_tdplch
.br
c_tddtri
.br
c_tdstri
.br
c_tditri
.br
c_tdmtri
.br
c_tdotri
.br
c_tdctri
.br
c_tdsort
.SH ACCESS
To use Tdpack, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH MESSAGES
When error conditions are detected, the support routine SETER
is called. By default, SETER writes a message to the standard
error file (as defined by I1MACH(4)) and then terminates
execution.  It is possible to put SETER into recovery mode and
regain control after a recoverable error (which includes
all of the possible errors).
.sp
The possible error messages are listed below.  All errors are recoverable
in the sense that a user program which has called ENTSR to set recovery
mode will get control back after one of these errors occurs.
.sp
.in +5
TDGETI - UNCLEARED PRIOR ERROR
.br
TDGETR - UNCLEARED PRIOR ERROR
.br
TDGETR - UNRECOGNIZED PARAMETER NAME
.br
TDINIT - UNCLEARED PRIOR ERROR
.br
TDPARA - UNCLEARED PRIOR ERROR
.br
TDPRPA - UNCLEARED PRIOR ERROR
.br
TDPRPI - UNCLEARED PRIOR ERROR
.br
TDPRPT - UNCLEARED PRIOR ERROR
.br
TDSETI - UNCLEARED PRIOR ERROR
.br
TDSETR - UNCLEARED PRIOR ERROR
.br
TDSETR - UNRECOGNIZED PARAMETER NAME
.in -5
.sp
.SH SEE ALSO
Online:
tdclrs, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
