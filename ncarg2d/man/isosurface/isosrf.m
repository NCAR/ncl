'\" t
.TH ISOSRF 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ISOSRF - Draws an isosurface. 
.SH SYNOPSIS
CALL ISOSRF (F,LU,MU,LV,MV,MW,EYE,MUVWP2,SLAB,FISO,IFLAG)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_isosrf (float *f, int lu, int mu, int lv, int mv, 
.br
int mw, float eye[3], int muvwp2, float *slab, float fiso, 
.br
int iflag)
.SH UTILITY
This routine is part of the Isosurface utility in NCAR Graphics.  To
see the overview man page for this utility, type "man isosurface".
.SH DESCRIPTION 
.IP F 12 
(an input array of type REAL, dimensioned LU x LV x m, 
where "m" is greater than or equal to MW) is a three-dimensional
array of data defining the function f(u,v,w).
Only the portion of the array consisting of elements 
F(IU,IV,IW), for IU = 1 to MU, IV = 1 to MV, and IW = 1 to 
MW, is to be used. This may or may not be the entire array. 
The data are considered to lie in a box in 3-space with 
opposite corners (in the UVW coordinate system) (1.,1.,1.) 
and (REAL(MU),REAL(MV),REAL(MW)). The element F(IU,IV,IW) 
is considered to be the value of the function f at the 
point (REAL(IU),REAL(IV),REAL(IW)).
.IP LU 12
(an input expression of type INTEGER) is the first 
dimension of the array F.
.IP MU 12
(an input expression of type INTEGER) defines the range 
to be used for the first subscript of the array F.
.IP LV 12
(an input expression of type INTEGER) is the second 
dimension of the array F.
.IP MV 12 
(an input expression of type INTEGER) defines the range 
to be used for the second subscript of the array F.
.IP MW 12
(an input expression of type INTEGER) defines the range 
to be used for the third subscript of the array F.
.IP EYE 12
(an input array of type REAL, dimensioned 3) is the 
position of the eye in the UVW coordinate system. The eye 
position, at (EYE(1),EYE(2),EYE(3)), must be outside the 
box containing the data. The point being looked at is at 
the center of the data box and the projection plane is 
perpendicular to the line of sight. While gaining 
experience with the routine, a good choice for EYE is 
(5.*REAL(MU),3.5*REAL(MV),2.*REAL(MW)).
.IP MUVWP2 12
(an input expression of type INTEGER) has the value 
MAX(MU,MV,MW)+2.
.IP SLAB 12
(a scratch array of type REAL, dimensioned at least 
MUVWP2 x MUVWP2) is a workspace for ISOSRF.
.IP FISO 12
(an input expression of type REAL) is the value of 
fiso in the equation f(u,v,w)=fiso, which defines the 
isosurface to be drawn.
.IP IFLAG 12
(an input expression of type INTEGER) serves two 
purposes: 
.RS 
.IP \(bu 4
The absolute value of IFLAG determines which type of lines 
are drawn to approximate the isosurface. Three types of 
lines are considered: lines of constant U, lines of 
constant V, and lines of constant W. The following table 
lists the types of lines drawn for various values of 
ABS(IFLAG):
.sp
.TS
tab(/);
l c c c
c c c c.
ABS(IFLAG)/Constant U/Constant V/Constant W
.sp
1/no/no/yes
2/no/yes/no
3/no/yes/yes
4/yes/no/no
5/yes/no/yes
6/yes/yes/no
7/yes/yes/yes
.TE
.IP \(bu 4
The sign of IFLAG determines what is inside and what is
outside the solid bounded by the isosurface and thus which
lines are visible and what is done at the boundary of the
box containing the data. If IFLAG is positive, values
greater than FISO are considered to be inside the solid
formed by the isosurface. If IFLAG is negative, values less
than FISO are considered to be inside the solid formed by
the isosurface. If the algorithm draws a cube, reverse the
sign of IFLAG.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions, with the following exceptions:
.IP f 12
t is dimensioned mw by mv by mu
.IP mu 12
The third dimension of the array f
.IP mw 12
The first dimension of the array f
.SH USAGE
Transformations can be achieved by adjusting scaling statement
functions in ISOSRF, SET3D, and TR32.
.sp
The hidden-line algorithm is not exact, so visibility errors
can occur.
.sp
Three-dimensional perspective character labeling of isosurfaces is
possible by calling the routine PWRZI.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tisosr,
tpwrzi,
fisissrf,
fispwrzi.
.SH ACCESS
To use ISOSRF or c_isosrf load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
isosurface, isosurface_params, ezisos, 
isgeti, isgetr, isseti, issetr, pwrzi, 
ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
