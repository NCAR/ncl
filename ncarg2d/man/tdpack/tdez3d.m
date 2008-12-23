.TH TDEZ3D 3NCARG "June 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDEZ3D - Provides a simplified interface to TDPACK for drawing isosurfaces.
.SH SYNOPSIS
CALL TDEZ3D(NX,NY,NZ,X,Y,Z,U,VALUE,RMULT,THETA,PHI,IST)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdez3d (int nx, int ny, int nz, float *x, float *y, 
.br
               float *z, float *u, float value, float rmult, 
.br
               float theta, float phi, int ist)
.SH DESCRIPTION 
The arguments of TDEZ3D are as follows:
.IP "NX" 8
(an input constant or variable of type INTEGER) -
The size of the input array X below.
.IP "NY" 8
(an input constant or variable of type INTEGER) -
The size of the input array Y below.
.IP "NZ" 8
(an input constant or variable of type INTEGER) -
The size of the input array Z below.
.IP "X" 8
(an input array variable of type REAL) -
A 1-dimensional array specifying X-coordinate values.
.IP "Y" 8
(an input array variable of type REAL) -
A 1-dimensional array specifying Y-coordinate values.
.IP "Z" 8
(an input array variable of type REAL) -
A 1-dimensional array specifying Z-coordinate values.
.IP "U" 8
(an input array variable of type REAL) -
An array NX by NY by NZ containing functional values;
Z(I,J,K) is a functional value at coordiante (X(I),Y(J),Z(K)) for
I=1,NX, J=1,NY, and K=1,NZ.
.IP "VALUE" 8
(an input array variable of type REAL) - An isovalue.
.IP "RMULT" 8
(an input constant or variable of type REAL) -
A multiplier of the diagonal length (DL) of the box defined
by the limits of the X, Y, and Z arrays.  Together with THETA and 
PHI (defined below),
RMULT is used to determine the eye position (see USAGE).
.IP "THETA" 8
(an input constant or variable of type REAL) -
An angle (in degrees) in the XY plane measured positive counter-clockwise 
from the X axis.  Together with RMULT (defined above) and PHI (defined below),
THETA is used to determine the eye position (see USAGE).
.IP "PHI" 8
(an input constant or variable of type REAL) -
An angle (in degrees) measured from the positive Z axis toward the XY plane.
Together with RMULT and THETA (defined above),
PHI is used to determine the eye position (see USAGE).
.IP "IST" 8
(an input constant or variable of type INTEGER) -
A style index defining the colors used to shade the isosurface as per:
.sp 
1  -  wire frame
.br
2  -  gray shades underneath; gray shades on top.
.br
3  -  gray shades underneath; red shades on top.
.br
4  -  gray shades underneath; green shades on top.
.br
5  -  gray shades underneath; blue shades on top.
.br
6  -  gray shades underneath; cyan shades on top.
.br
7  -  gray shades underneath; magenta shades on top.
.br
8  -  gray shades underneath; yellow shades on top.
.br

.sp
IST is positive, then black is used for the background
or and white for the foreground color; if IST is
negative of any of the above values, then white
used for the background color and black for the
eground color.
.sp
If the value for IST falls outside the legal range, it is
defaulted to 6.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The coordinate (RMULT*DL,THETA,PHI) is the spherical
coordinate for the eye position.  If RMULT = THETA = PHI = 0.,
a default eye position ( 2.5,-55.,70.) is chosen.  The point
looked at is calculated to be the center of the box defined
by the limits of the X, Y, and Z arrays.
.sp
TDEZ3D does not call FRAME.
.sp
If the image is too small, decrease the size of RMULT; if the
image is too large, increase the size of RMULT.
.sp
When TDEZ3D is called, a color table is defined for all open
workstations that implements IST as described above.  *This
color table will supersede any color table that has been
previously defined.*  The color table that is defined is:

.nf

      Color
      index   Colors
    -------   ----------------------------------------------------
          0   black if IST is positive; white if IST is negative
          1   white if IST is positive; black if IST is negative
          2   red
          3   green
          4   blue
          5   cyan
          6   magenta
          7   yellow
      8- 37   grayscale from white to black.
     38- 67   shades of gray
     68- 97   shades of red
     98-127   shades of green
    128-157   shades of blue
    158-187   shades of cyan
    188-217   shades of magenta
    218-247   shades of yellow
.fi

.SH ACCESS
To use TDEZ3D or c_tdez3d, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params,
tdpara, tdplch, tdprpa, tdprpi, tdprpt, tdsetr, tdsort, tdstri, tdstrs,
tdez2d
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
