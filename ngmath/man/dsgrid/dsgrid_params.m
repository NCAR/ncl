.\"
.\"     $Id: dsgrid_params.m,v 1.4 2000-07-13 03:17:56 haley Exp $
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

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.

