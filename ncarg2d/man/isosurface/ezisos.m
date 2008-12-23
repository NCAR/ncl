'\" t
.TH EZISOS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
EZISOS - Draws an isosurface. 
.SH SYNOPSIS
CALL EZISOS (F,MU,MV,MW,EYE,SLAB,FISO)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ezisos (float *f, int mu, int mv, int mw, 
.br 
float eye[3], float *slab, float fiso)
.SH DESCRIPTION 
.IP F 12 
(an input array of type REAL, dimensioned MU x MV x MW) 
is a three-dimensional array of data defining the function 
f(u,v,w). The entire array (elements F(IU,IV,IW), for IU = 
1 to MU, IV = 1 to MV, and IW = 1 to MW) is to be used. The 
data are considered to lie in a box in 3-space with
opposite corners (in the UVW coordinate system) (1.,1.,1.) 
and (REAL(MU),REAL(MV),REAL(MW)). The element F(IU,IV,IW) 
is considered to be the value of the function f at the 
point (REAL(IU),REAL(IV),REAL(IW)).
.IP MU 12
(an input expression of type INTEGER) is the first 
dimension of the array F.
.IP MV 12
(an input expression of type INTEGER) is the second 
dimension of the array F.
.IP MW 12
(an input expression of type INTEGER) is the third 
dimension of the array F.
.IP EYE 12
(an input array of type REAL, dimensioned 3) is the 
position of the eye in the UVW coordinate system. The eye 
position, at (EYE(1),EYE(2),EYE(3)), must be outside the 
box containing the data. The point being looked at is at 
the center of the data box and the projection plane is 
perpendicular to the line of sight. While gaining 
experience with the routine, a good choice for EYE is 
(5.*REAL(MU),3.5*REAL(MV),2.*REAL(MW)).
.IP SLAB 12
(a scratch array of type REAL, dimensioned at least n 
x n, where "n" is defined to be MAX[MU,MV,MW]+2) is a
workspace for ISOSRF.
.IP FISO 12 
(an input expression of type REAL) is the value of 
fiso in the equation f(u,v,w)=fiso, which defines the
isosurface to be drawn.
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
EZISOS is called to draw an isosurface if all of the input array is to
be used (rather than a subset of it), if ISOSRF's argument IFLAG is to be
chosen internally, and if a frame advance is to be done after the isosurface
is drawn.  If any of these conditions is not met, use ISOSRF instead.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
tisosr,
fisissrf.
.SH ACCESS
To use EZISOS or c_ezisos, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
isosurface, isosurface_params, 
isgeti, isgetr, isosrf, isseti, issetr, pwrzi, 
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
