.\"
.\"     $Id: dsgrid_errors.m,v 1.3 2000-07-13 03:17:56 haley Exp $
.\"
.TH dsgrid_errors 3NCARG "September 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
dsgrid_errors - This document briefly describes all the
errors reported from Dsgrid.
.SH DESCRIPTION 
Each entry below includes the error number and its meaning.
.IP " 1"
Error number out of range.
.IP " 2"
Insufficient data in gridded region to triangulate.
.IP " 3"
Array dimension out of range.
.IP " 4"
Parameter name not known.
.IP " 5"
Cannot open error file.
.IP " 6"
Error allocating memory for input points.
.IP " 7"
Fortran DOUBLE PRECISION entries are not supported on UNICOS.
.IP " 9"
Error allocating memory for array of distances between input points.
.IP "10"
Error allocating memory for weights.
.IP "11"
Error allocating memory for distance ordering vector.
.IP "12"
Error allocating memory for output array.
.IP "13"
Number of input points must be greater than 2.
.IP "14"
No original data values within the specified distance - interpolated value set to missing value flag.
.SH SEE ALSO
dsgrid,
dsgrid_params,
dsgrid2s,
dsgrid3s,
dsgrid2d,
dsgrid3d,
dspnt2s,
dspnt2d,
dspnt3s,
dspnt3d
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

