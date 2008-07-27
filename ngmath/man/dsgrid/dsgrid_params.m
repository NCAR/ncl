.\"
.\"     $Id: dsgrid_params.m,v 1.6 2008-07-27 03:35:37 haley Exp $
.\"
.TH dsgrid_params 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
dsgrid_params - This document briefly describes all the
internal parameters of Dsgrid.
.SH DESCRIPTION 
Each entry below includes the name of a parameter, its Fortran type
(use the obvious equivalents for C), 
its default value, and a short description of the parameter.  
.IP "\'DMV\'   -   Real   -    any"
A special value for data initialization and use with the dmx control
parameter.
.IP "\'DMX\'   -   Real   -   > 0."
Used in calculating weights - use only input data the distance dmx.
.IP "\'ERF\'   -   Character   -   stderr"
Error file.
.IP "\'EXP\'   -   Real   -   > 0."
Power to use for inverse distances in computing weights.
.IP "\'SHD\'   -   Integer   -   0"
Flag for controlling whether the shadowing feature is on (0 = no; 1 = yes).
.SH SEE ALSO
dsgrid,
dsgrid_params,
dsgrid_errors,
dsgrid2s,
dsgrid3s,
dsseti,
dsgeti,
dssetr
dsgetr,
dssetc,
dsgetc,
dspnt2s,
dspnt3s,
dsgrid2d,
dsgrid3d,
dssetrd
dsgetrd,
dspnt2d,
dspnt3d,
c_dsgrid2s,
c_dsgrid3s,
c_dsseti,
c_dsgeti
c_dssetr,
c_dsgetr,
c_dssetc,
c_dsgetc,
c_dspnt2s,
c_dspnt3s,
c_dsgrid2d
c_dsgrid3d,
c_dssetrd,
c_dsgetrd,
c_dspnt2d,
c_dspnt3d.
.sp
Complete documentation for Dsgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/dsgrid/dshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
