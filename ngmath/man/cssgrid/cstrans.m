.\"
.\"	$Id: cstrans.m,v 1.1 1999-06-29 00:23:03 fred Exp $
.\"
.TH CSTRANS 3NCARG "JUNE 1999" UNIX "NCAR GRAPHICS"
.SH NAME
CSTRANS - convert from lat/lon coordinates to Cartesian coordinates.
.SH SYNOPSIS
CALL CSTRANS (N, RLAT, RLON, X, Y, Z)
.SH DESCRIPTION
.IP N 12
(integer,input) The number of input lat/lon coordinates.
.IP RLAT 12
(real, input) An array containing the latitudes of the input coordinates. 
.IP RLON 12
(real, input) An array containing the longitudes of the input coordinates. 
.IP X 12
(real, input) An array containing the X component of the Cartesian 
coordinates of the output data. (X(I),Y(I),Z(I)) is
the Cartesian coordinate corresponding to the lat/lon 
coordinate (RLAT(I),RLON(I)) for I=1 to N. 
.IP Y 12
(real, input) An array containing the Y component of the Cartesian 
coordinates of the output data. (X(I),Y(I),Z(I)) is
the Cartesian coordinate corresponding to the lat/lon 
coordinate (RLAT(I),RLON(I)) for I=1 to N. 
.IP Z 12
(real, input) An array containing the Z component of the Cartesian 
coordinates of the output data. (X(I),Y(I),Z(I)) is
the Cartesian coordinate corresponding to the lat/lon 
coordinate (RLAT(I),RLON(I)) for I=1 to N. 
.SH USAGE
CSTRANS is called to find the equivalent Cartesian coordinates 
to specified latitude and longitude coordinates on a sphere. 
The coordinate of 0. latitude and 0. longitude is
converted to Cartesian coordinate (1.,0.,0.). Latitudes and 
longitudes are assumed to be in radians. 
.SH ACCESS
To use CSSTRI, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
csscoord,
csstri,
csvoro,
c_cssgrid,
c_csstri,
c_csscoord,
c_csvoro,
cssgrid_errors
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/cssgrid/csshome.html
.SH COPYRIGHT
Copyright (C) 1997-1999
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
