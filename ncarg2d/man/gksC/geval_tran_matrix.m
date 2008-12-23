.\"
.\"	$Id: geval_tran_matrix.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GEVAL_TRAN_MATRIX 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
geval_tran_matrix (Evaluate transformation matrix) - Constructs a GKS segment
transformation matrix starting from a given point, a shift vector, a rotation
angle, and X and Y scale factors.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void geval_tran_matrix(const Gpoint *point, const Gvec *shift, Gdouble angle, const Gvec *scale, Gcoord_switch coord_switch, Gtran_matrix tran_matrix);
.SH DESCRIPTION
.IP point.x 12
(Gfloat, Input) - An X coordinate value for a fixed point to be used for
the scaling and rotation parts of the output transformation.  point.x is 
either in world coordinates or normalized device coordinates depending on the 
setting of the argument coord_switch described below.
.IP point.y 12
(Gfloat, Input) - A Y coordinate value for a fixed point to be used for
the scaling and rotation parts of the output transformation.  point.y is 
either in world coordinates or normalized device coordinates depending on the 
setting of the argument coord_switch described below.
.IP shift.delta_x 12
(Gfloat, Input) - The X component of a shift vector to be used for the
scaling part of the output transformation.  shift.delta_x is either in world
coordinates or normalized device coordinates depending on the 
setting of the argument coord_switch described below.
.IP shift.delta_y 12
(Gfloat, Input) - The Y component of a shift vector to be used for the
scaling part of the output transformation.  shift.delta_y is either in world
coordinates or normalized device coordinates depending on the 
setting of the argument coord_switch described below.
.IP angle 12
(Input) - The rotation angle, in radians, to be used for the
rotation part of the output transformation.
.IP scale.delta_x 12
(Gfloat Input) - An X coordinate scale factor to be used in the scaling
part of the output transformation.
.IP scale.delta_y 12
(Gfloat Input) - A Y coordinate scale factor to be used in the scaling
part of the output transformation.
.IP coord_switch 12
(Input) - A coordinate switch to indicate whether the
values for the arguments shift and scale (described above) are in
world coordinates or normalized device coordinates.  Possible values
include:
.RS
.IP GCOORD_WC
World coordinates
.IP GCOORD_NDC
Normalized device coordinates
.IP tran_matrix 12
(Gfloat, Output) - A 2x3 array that contains the GKS transformation matrix in 
a form that can be used as input to other GKS functions such as gset_seg_tran.
.SH USAGE
If world coordinates are used, the shift vector and the fixed point are
transformed by the current normalization transformation.  
.sp
The order of the transformation operations as built into the output 
matrix is: scale (relative to the fixed point); rotate (relative to 
the fixed point; shift.
.sp
Elements tran_matrix[0][2] and tran_matrix[1][2] are in normalized device 
coordinates and the other elements of tran_matrix are unitless.
.SH EXAMPLE
The following code
.nf

      pi = 3.1415926;
      point.x = point.y = 0.5;
      shift.delta_x = 0.25;
      shift.delta_y = 0.;
      scale.delta_x = 0.5;
      scale.delta_y = 1.5;
      geval_tran_matrix(&point,&shift,45.*pi/180.,&scale,GCOORD_WC,tm);

.fi
would produce a transformation matrix in tm that would: scale the
X coordinates by .5, scale the Y coordinates by 1.5 (relative to
the fixed point of (.5,.5) ); rotate by 45 degrees (relative to the
fixed point (.5,.5) ); and shift by .25 in X and 0. in Y.  The
input values for the fixed point and shift vector are in world
coordintes.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gaccum_tran_matrix(3NCARG),
.BR gclose_seg(3NCARG),
.BR gcreate_seg(3NCARG),
.BR gcopy_seg_ws(3NCARG),
.BR gdel_seg(3NCARG),
.BR ginq_name_open_seg(3NCARG),
.BR ginq_set_seg_names(3NCARG),
.BR gset_seg_tran(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
