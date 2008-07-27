.\"
.\"     $Id: dsgrid_errors.m,v 1.5 2008-07-27 03:35:37 haley Exp $
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

The use of this Software is governed by a License Agreement.
