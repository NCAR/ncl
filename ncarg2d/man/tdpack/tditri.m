.TH TDITRI 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDITRI - Add triangles defining a simple surface to the triangles in a triangle
list.
.SH SYNOPSIS
CALL TDITRI (U, NU, V, NV, W, NW, F, LF1D, LF2D, FISO, RTRI, MTRI, NTRI, IRST)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tditri(float *u, int nu, float *v, int nv, float *w, int nw,
float *f, int lf1d, int lf2d, float fiso, float *rtri, int mtri, int *ntri,
int irst)
.SH DESCRIPTION
The arguments of TDITRI are as follows:
.IP "U" 8
(an input array, of type REAL, dimensioned NU) - values of an independent
variable "u". It must be the case that U(1) < U(2) < ... U(NU-1) < U(NU).
.IP "NU" 8
(an input expression of type INTEGER) - the dimension of U.
.IP "V" 8
(an input array, of type REAL, dimensioned NV) - values of an independent
variable "v". It must be the case that V(1) < V(2) < ... V(NV-1) < V(NV).
.IP "NV" 8
(an input expression of type INTEGER) - the dimension of V.
.IP "W" 8
(an input array, of type REAL, dimensioned NW) - values of an independent
variable "w". It must be the case that W(1) < W(2) < ... W(NV-1) < W(NV).
.IP "NW" 8
(an input expression of type INTEGER) - the dimension of W.
.IP "F" 8
(an input array, of type REAL, dimensioned NU x NV x NW and having FORTRAN
first and second dimensions LF1D and LF2D, respectively) - values of a
dependent variable "f(u,v,w)".  F(I,J,K) is the value of the function "f"
at the position (U(I),V(J),W(K)); the equation "f(u,v,w)=FISO" defines a
surface that one wishes to draw.
.IP "LF1D and LF2D" 8
(input expressions of type INTEGER) - the FORTRAN first and second dimensions
of the array F.  It must be the case that LF1D is greater than or equal to NU
and that LF2D is greater than or equal to NV.
.IP "FISO" 8
(an input expression of type REAL) - the value of the function f defining the
isosurface to be drawn.
.IP "RTRI" 8
(an input array, of type REAL, dimensioned 10 x MTRI) -
a list of triangles, probably created by means of calls to TDSTRI, TDITRI,
and/or TDMTRI, and sorted, probably by means of a call to TDOTRI.
.IP "MTRI" 8
(an input expression of type INTEGER) - the second dimension of RTRI
and thus the maximum number of triangles the triangle list will hold.
.IP "NTRI" 8
(an input/output variable of type INTEGER) - keeps track of the number of
triangles currently in the list.  It is the user's responsibility to zero this
initially and its value is increased by each call to a triangle-generating
routine like TDITRI. If NTRI becomes equal to MTRI, TDITRI does not take an
error exit; instead, it just stops generating triangles.  Therefore, it's a
good idea, after calling TDITRI, to check the value of NTRI against the
dimension MTRI; if they're equal, it probably means that the triangle list
filled up and that the rendered surface will be incomplete.
.IP "IRST" 8
(an input expression of type INTEGER) - specifies the index of the rendering
style to to be used for the triangles added to the triangle list by this call.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDITRI or c_tditri, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tdlbla,
tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
