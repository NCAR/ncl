.\"
.\"	$Id: gaccum_tran_matrix.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GACCUM_TRAN_MATRIX 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gaccum_tran_matrix (Accumulate transformation matrix) - Constructs a GKS segment
transformation matrix by starting with an existing matrix and composing it
with a shift vector, a rotation angle, and X and Y scale factors to
create a new transformation matrix.  The rotation and scaling are
done with respect to a user-defined fixed point.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gaccum_tran_matrix(const Gtran_matrix t_matrix, const Gpoint *point, const Gvec *shift, Gdouble angle, const Gvec *scale, Gcoord_switch coord_switch, Gtran_matrix tran_matrix);
.SH DESCRIPTION
.IP t_matrix 12
(Gfloat, Input) - A 2x3 GKS transformation matrix.
.IP point.x 12
(Gfloat, Input) - X coordinate value for a fixed point to be used for
the scaling and rotation parts of the output transformation.  point.x is
either in world coordinates or normalized device coordinates depending on the 
setting of the argument coord_switch described below.
.IP point.y 12
(Gfloat, Input) - Y coordinate value for a fixed point to be used for
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
(Gfloat, Input) - X coordinate scale factor to be used in the scaling
part of the output transformation.
.IP scale.delta_y 12
(Gfloat, Input) - Y coordinate scale factor to be used in the scaling
part of the output transformation.
.IP coord_switch 12
(Input) - A coordinate switch to indicate whether the
values for the arguments point and shift (described above) are in
world coordinates or normalized device coordinates.  Possible values include:
.RS
.IP GCOORD_WC
World coordinates
.IP GCOORD_NDC
Normalized device coordinates
.IP tran_matrix 12
(Gfloat, Output) - A 2x3 array that contains the GKS
transformation matrix in a form that can be used as input to other
GKS functions such as gset_seg_tran.  This matrix is constructed by composing
the scale, rotate, and shift input, described above, with the original
input matrix t_matrix.
.SH USAGE
If world coordinates are used, the shift vector and the fixed point are
transformed by the current normalization transformation.  
.sp
The order in which the transformations are applied is: input matrix,
scale, rotate, and shift.
.sp
Elements tran_matrix[0][2] and tran_matrix[1][2] are in normalized device 
coordinates and the other elements of tran_matrix are unitless.
.sp
gaccum_tran_matrix can be used to construct more general transformation matrices
than geval_tran_matrix.  The most common usage of gaccum_tran_matrix is to 
change the order 
in which the operations of scale, rotate, and shift are applied (which
is fixed in geval_tran_matrix).  The example below shows how to construct a 
transformation matrix that shifts first and then rotates.
.SH EXAMPLE
Assuming that the input matrix t_matrix is initially the identity, 
the following code
.nf

      pi = 3.1415926;
      point.x = point.y = 0.5;
      shift.delta_x = 0.25;
      shift.delta_y = 0.;
      scale.delta_x = 1.0;
      scale.delta_y = 1.0;
      gaccum_tran_matrix(t_matrix,&point,&shift,0.,&scale,GCOORD_WC,tout);
      for( i = 0; i < 2; i++ ) {
          for( j = 0; j < 3; j++ ) {
              t_matrix[i][j] = tout[i][j];
          }
      }
      shift.delta_x = 0.;
      shift.delta_y = 0.;
      gaccum_tran_matrix(t_matrix,&point,&shift,45.*pi/180.,&scale,GCOORD_WC,tout);

.fi
would produce a transformation matrix in tout that would shift
by (.25,0.) first, and then rotate by 45 degrees.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR geval_tran_matrix(3NCARG),
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
