.\"
.\"	$Id: gevtm.m,v 1.16 2008-12-23 00:03:02 haley Exp $
.\"
.TH GEVTM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GEVTM (Evaluate transformation matrix) - Constructs a GKS segment
transformation matrix starting from a given point, a shift vector, a rotation
angle, and X and Y scale factors.
.SH SYNOPSIS
CALL GEVTM(X0,Y0,DX,DY,PHI,FX,FY,SW,MOUT)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void geval_tran_matrix(const Gpoint *point, const Gvec *shift, Gdouble angle, const Gvec *scale, Gcoord_switch coord_switch, Gtran_matrix tran_matrix);
.SH DESCRIPTION
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
GKS functions such as GSSGT.
.SH USAGE
If world coordinates are used, the shift vector and the fixed point are
transformed by the current normalization transformation.  
.sp
The order of the transformation operations as built into the output 
matrix is: scale (relative to the fixed point); rotate (relative to 
the fixed point; shift.
.sp
Elements MOUT(1,3) and MOUT(2,3) are in normalized device coordinates
and the other elements of MOUT are unitless.
.SH EXAMPLE
The following code
.nf


      PI = 3.1415926
      CALL GEVTM(.5,.5,.25,0.,45.*PI/180.,.5,1.5,0,TM)

.fi
would produce a transformation matrix in TM that would: scale the
X coordinates by .5, scale the Y coordinates by 1.5 (relative to
the fixed point of (.5,.5) ); rotate by 45 degrees (relative to the
fixed point (.5,.5) ); and shift by .25 in X and 0. in Y.  The
input values for the fixed point and shift vector are in world
coordintes.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gactm, gclsg, gcrsg, gcsgwk, gdsg, gqopsg, gqsgus, gssgt., geval_tran_matrix
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
