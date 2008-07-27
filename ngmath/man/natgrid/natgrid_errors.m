.\"
.\"     $Id: natgrid_errors.m,v 1.5 2008-07-27 03:35:40 haley Exp $
.\"
.TH natgrid_errors 3NCARG "March 1997-1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
natgrid_errors - This document briefly describes all the
errors reported from Natgrid.
.SH DESCRIPTION 
Each entry below includes the error number and its meaning.
.IP " 1"
Insufficient data in gridded region to triangulate.
.IP " 2"
Duplicate input data coordinates are not allowed.
.IP " 3"
Unable to open file for writing algorithmic data.
.IP " 4"
WARNING:  The ratio of vertical to horizontal scales is too 
large for meaningful gradient estimation.  Rescale the data if 
gradients are required.
.IP " 5"
WARNING:  The ratio of vertical to horizontal scales is too small 
for meaningful gradient estimation.  Rescale the data if gradients 
are required.
.IP " 6"
WARNING:  The ratio of x-axis breadth to y-axis breadth of this 
gridded region may be too extreme for good interpolation.  
Changing the block proportions, or rescaling the x or y 
coordinate may be indicated.  Gradient calculations have been disabled.
.IP " 7"
Unable to allocate storage for ivector.
.IP " 8"
Unable to allocate storage for dvector.
.IP " 9"
Unable to allocate storage for **imatrix.
.IP "10"
Unable to allocate storage for imatrix[].
.IP "11"
Unable to allocate storage for **fmatrix.
.IP "12"
Unable to allocate storage for fmatrix[].
.IP "13"
Unable to allocate storage for **dmatrix.
.IP "14"
Unable to allocate storage for dmatrix[].
.IP "15"
Unable to allocate storage for raw data.
.IP "16"
Unable to allocate storage for a simplex.
.IP "17"
Unable to allocate storage for temp.
.IP "18"
Unable to allocate storage for neig.
.IP "19"
slopes have not been computed, set sdip.
.IP "20"
row argument out of range.
.IP "21"
column argument out of range.
.IP "22"
aspects have not been computed, set sdip.
.IP "23"
Parameter name not known.
.IP "24"
Cannot open error file.
.IP "25"
Automatic scaling has been done - aspects will be distorted and 
consequently are not returned.  Rescale your data manually, or 
by setting magx, magy, and magz appropriately.
.IP "26"
Automatic scaling has been done - slopes will be distorted and 
consequently are not returned.  Rescale your data manually, or 
by setting magx, magy, and magz appropriately.
.IP "27"
Coordinate is outside of the gridded region for a single 
point interpolation.
.IP "28"
Cannot compute aspects and slopes in conjunction with single 
point interpolation mode.
.IP "29"
Fortran DOUBLE PRECISION entries are not supported on UNICOS.
.IP "30"
Error number out of range.
.SH SEE ALSO
natgrid,
c_natgridd,
c_natgrids,
c_nngetaspectd,
c_nngetaspects,
c_nngetsloped,
c_nngetslopes,
natgridd,
natgrids,
nngetaspectd,
nngetaspects,
nngetslopes,
nngetslpped.
.sp
Complete documentation for Natgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/natgrid/nnhome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
