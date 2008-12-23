.\"
.\"	$Id: gactm.m,v 1.16 2008-12-23 00:03:02 haley Exp $
.\"
.TH GACTM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GACTM (Accumulate transformation matrix) - Constructs a GKS segment
transformation matrix by starting with an existing matrix and composing it
with a shift vector, a rotation angle, and X and Y scale factors to
create a new transformation matrix.  The rotation and scaling are
done with respect to a user-defined fixed point.
.SH SYNOPSIS
CALL GACTM(MINP,X0,Y0,DX,DY,PHI,FX,FY,SW,MOUT)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gaccum_tran_matrix(const Gtran_matrix t_matrix, const Gpoint *point, const Gvec *shift, Gdouble angle, const Gvec *scale, Gcoord_switch coord_switch, Gtran_matrix tran_matrix);
.SH DESCRIPTION
.IP MINP 12
(Real, Input) - A 2x3 GKS transformation matrix.
.IP X0 12
(Real, Input) - An X coordinate value for a fixed point to be used for
the scaling and rotation parts of the output transformation.  X is 
either in world 
coordinates or normalized device coordinates depending on the 
setting of the argument SW described below.
.IP Y0 12
(Real, Input) - A Y coordinate value for a fixed point to be used for
the scaling and rotation parts of the output transformation.  Y is 
either in world 
coordinates or normalized device coordinates depending on the 
setting of the argument SW described below.
.IP DX 12
(Real, Input) - The X component of a shift vector to be used for the
scaling part of the output transformation.  DX is either in world
coordinates or normalized device coordinates depending on the 
setting of the argument SW described below.
.IP DY 12
(Real, Input) - The Y component of a shift vector to be used for the
scaling part of the output transformation.  DY is either in world
coordinates or normalized device coordinates depending on the 
setting of the argument SW described below.
.IP PHI 12
(Real, Input) - The rotation angle, in radians, to be used for the
rotation part of the output transformation.
.IP FX 12
(Real, Input) - An X coordinate scale factor to be used in the scaling
part of the output transformation.
.IP FY 12
(Real, Input) - A Y coordinate scale factor to be used in the scaling
part of the output transformation.
.IP SW 12
(Integer, Input) - A coordinate switch to indicate whether the
values for the arguments X0, Y0, DX, and DY (described above) are in
world coordinates or normalized device coordinates.  SW=0 indicates
world coordinates and SW=1 indicates normalized device coordinates.
.IP MOUT 12
(Real, Output) - A 2x3 array that contains the GKS
transformation matrix in a form that can be used as input to other
GKS functions such as GSSGT.  This matrix is constructed by composing
the scale, rotate, and shift input, described above, with the original
input matrix MINP.
.SH USAGE
If world coordinates are used, the shift vector and the fixed point are
transformed by the current normalization transformation.  
.sp
The order in which the transformations are applied is: input matrix,
scale, rotate, and shift.
.sp
Elements MOUT(1,3) and MOUT(2,3) are in normalized device coordinates
and the other elements of MOUT are unitless.
.sp
GACTM can be used to construct more general transformation matrices
than GEVTM.  The most common usage of GACTM is to change the order 
in which the operations of scale, rotate, and shift are applied (which
is fixed in GEVTM).  The example below shows how to construct a 
transformation matrix that shifts first and then rotates.
.SH EXAMPLE
Assuming that the input matrix TIN is initially the identity, 
the following code
.nf

      PI = 3.1415926
      CALL GACTM(TIN,.5,.5,.25,0.,0.,1.,1.,0,TOUT)
      DO 20 I=1,2
        DO 30 J=1,3
          TIN(I,J) = TOUT(I,J)
   30   CONTINUE
   20 CONTINUE
      CALL GACTM(TIN,.5,.5,0.,0.,45.*PI/180.,1.,1.,0,TOUT)

.fi
would produce a transformation matrix in TOUT that would shift
by (.25,0.) first, and then rotate by 45 degrees.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gevtm, gclsg, gcrsg, gcsgwk, gdsg, gqopsg, gqsgus, gssgt., gaccum_tran_matrix
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
