.\"
.\"	$Id: c_cstrans.m,v 1.1 1999-06-29 00:23:02 fred Exp $
.\"
.TH c_cstrans 3NCARG "June 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_cstrans - convert from lat/lon to Cartesian coordinates
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
    void   c_cstrans(int, float *, float *, float *, float *, float *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
    void c_cstrans (n, plat, plon, x, y, z)
.fi
.cs R
.sp
.SH DESCRIPTION
.IP n 12
The number of input data points, n > 2. 
.IP plat 12
Contains the latitudes of the input coordinates. 
.IP plon 12
Contains the longitudes of the input coordinates. 
.IP x 12
An array containing the X component of the Cartesian
coordinates of the output data. (x[i],y[i],z[i]) is
the Cartesian coordinate corresponding to the lat/lon
coordinate (rlat[i],rlon[i]) for i=0 to n-1.
.IP y 12
An array containing the Y component of the Cartesian
coordinates of the output data. (x[i],y[i],z[i]) is
the Cartesian coordinate corresponding to the lat/lon
coordinate (rlat[i],rlon[i]) for i=0 to n-1.
.IP z 12
An array containing the Z component of the Cartesian
coordinates of the output data. (x[i],y[i],z[i]) is
the Cartesian coordinate corresponding to the lat/lon
coordinate (rlat[i],rlon[i]) for i=0 to n-1.
.SH USAGE
c_cstrans is called to find the equivalent Cartesian 
coordinates to specified latitude and
longitude coordinates on a sphere. The coordinate of 0. 
latitude and 0. longitude is
converted to Cartesian coordinate (1.,0.,0.). Latitudes and 
longitudes are assumed to be in radians. 
.SH ACCESS
To use c_cstrans, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
csscoord,
csstri,
csvoro,
c_cssgrid,
c_csstri,
c_csscoord,
c_cstrans,
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
