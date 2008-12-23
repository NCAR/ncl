.TH TDLBLS 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDLBLS - Draw labels on selected sides of a projected box. This routine calls
TDPARA and will therefore change the definition of the reference parallelogram;
it also changes the value of the internal parameter 'CS2'.
.SH SYNOPSIS
CALL TDLBLS (UMIN, VMIN, WMIN, UMAX, VMAX, WMAX, UNLB, VNLB, WNLB, UILB, VILB,
WILB, IPCK)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdlbls(float umin, float vmin, float wmin, float umax, float vmax,
float wmax, char* unlb, char* vnlb, char* wnlb, char* uilb, char* vilb,
char* wilb, int ipck)
.SH DESCRIPTION
The arguments of TDLBLS are as follows:
.IP "UMIN, VMIN, WMIN, UMAX, VMAX, WMAX" 8
(input expressions of type REAL) - coordinate values defining the box in
3-space.  The names of these should make it clear what they are.
.IP "UNLB, VNLB, and WNLB" 8
(input, of type CHARACTER) - these strings contain numeric labels to be placed
on a U axis, a V axis, and a W axis, respectively.  The labels need not be in
any particular order, but they have to be separated by blanks and each has to
be readable using a FORTRAN format of the form "En.0", where "n" is the length
of the label.
.IP "UILB, VILB, and WILB" 8
(input, of type CHARACTER) - these strings contain informational labels for a
U axis, a V axis, and a W axis, respectively.
.IP "IPCK" 8
(an input expression of type INTEGER) - determines which edges of the box are
labelled. If IPCK is zero, all six outer edges are labelled, but if IPCK is
non-zero, only three edges are labelled, one set of three if IPCK is negative,
a different set if IPCK is positive.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDLBLS or c_tdlbls, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
