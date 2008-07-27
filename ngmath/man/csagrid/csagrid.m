.\"
.\"     $Id: csagrid.m,v 1.4 2008-07-27 03:35:34 haley Exp $
.\"
.TH Csagrid 3NCARG "January 1999" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Csagrid is a software package in the ngmath library that implements a 
cubic spline approximation algorithm to fit a function to input data. 
The input for the approximation is a set of randomly-spaced data. These 
data may be one-dimensional, two-dimensional, or three-dimensional. 
.sp
The output is a set of values at coordinates on a user-specified grid, 
or at a set of user-specified points (including at a single point). 
.sp
Functionally equivalent Fortran, C, and NCL interfaces are provided. 
.sp
Csagrid is based on the work of David Fulker and his package Splpack. 
.SH SYNOPSIS
.sp
Fortran one-dimensional input
.br
-----------------------------
.sp
 CSA1S        -  simple entry
.br
 CSA1XS       - expanded entry
.sp
Fortran two-dimensional input
.br
-----------------------------
.sp
 CSA2S        - simple entry, gridded output
.br
 CSA2XS       - expanded entry, gridded output
.br
 CSA2LS       - simple entry, list output
.br
 CSA2LXS      - expanded entry, list output
.sp
Fortran three-dimensional input
.br
-----------------------------
.sp
 CSA3S        - simple entry, gridded output
.br
 CSA3XS       - expanded entry, gridded output
.br
 CSA3LS       - simple entry, list output
.br
 CSA3LXS      - expanded entry, list output
.sp
C one-dimensional input
.br
-----------------------------
.sp
 c_csa1s        -  simple entry
.br
 c_csa1xs       - expanded entry
.sp
C two-dimensional input
.br
-----------------------------
.sp
 c_csa2s        - simple entry, gridded output
.br
 c_csa2xs       - expanded entry, gridded output
.br
 c_csa2ls       - simple entry, list output
.br
 c_csa2lxs      - expanded entry, list output
.sp
C three-dimensional input
.br
-----------------------------
.sp
 c_csa3s        - simple entry, gridded output
.br
 c_csa3xs       - expanded entry, gridded output
.br
 c_csa3ls       - simple entry, list output
.br
 c_csa3lxs      - expanded entry, list output
.sp
NCL one-dimensional input
.br
-----------------------------
.sp
 csa1s        -  simple entry
.br
 csa1xs       - expanded entry
.sp
NCL two-dimensional input
.br
-----------------------------
.sp
 csa2s        - simple entry, gridded output
.br
 csa2xs       - expanded entry, gridded output
.br
 csa2ls       - simple entry, list output
.br
 csa2lxs      - expanded entry, list output
.sp
NCL three-dimensional input
.br
-----------------------------
.sp
 csa3s        - simple entry, gridded output
.br
 csa3xs       - expanded entry, gridded output
.br
 csa3ls       - simple entry, list output
.br
 csa3lxs      - expanded entry, list output
.SH ACCESS 
To use Csagrid entries, load the NCAR Graphics library ngmath.
.SH SEE ALSO
csagrid,
csa1s,
csa1xs,
csa2s,
csa2xs,
csa2ls,
csa2lxs,
csa3s,
csa3xs,
csa3ls,
csa3lxs,
c_csa1s,
c_csa1xs,
c_csa2s,
c_csa2xs,
c_csa2ls,
c_csa2lxs,
c_csa3s,
c_csa3xs,
c_csa3ls,
c_csa3lxs.
.sp
Complete documentation for Csagrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/csagrid/csahome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
