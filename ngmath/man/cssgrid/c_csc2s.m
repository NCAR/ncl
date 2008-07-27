.\"
.\"	$Id: c_csc2s.m,v 1.4 2008-07-27 03:35:34 haley Exp $
.\"
.TH c_csc2s 3NCARG "MAY 2000" UNIX "NCAR GRAPHICS"
.SH NAME
c_csc2s - convert from Cartesian coordinates to lat/lon coordinates.
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
    void   c_csc2s(int, float *, float *, float *, float *, float *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
    c_csc2s(n, x, y, z, rlat, rlon);
.fi
.cs R
.sp
.SH DESCRIPTION
.IP n 12
The number of input Cartesian coordinates.
.IP x 12
An array containing the X component of the Cartesian 
coordinates of the input data. (rlat[i],rlon[i]) is
the lat/lon coordinate corresponding to the Cartesian
coordinate (x[i],y[i],z[i]) for i=0 to n-1. 
.IP y 12
An array containing the Y component of the Cartesian 
coordinates of the input data. (rlat[i],rlon[i]) is
the lat/lon coordinate corresponding to the Cartesian
coordinate (x[i],y[i],z[i]) for i=0 to n-1. 
.IP z 12
An array containing the Z component of the Cartesian 
coordinates of the input data. (rlat[i],rlon[i]) is
the lat/lon coordinate corresponding to the Cartesian
coordinate (x[i],y[i],z[i]) for i=0 to n-1. 
.IP rlat 12
An array containing the latitudes of the output coordinates,
in degrees. 
.IP rlon 12
An array containing the longitudes of the output coordinates,
in degrees. 
.SH USAGE
c_csc2s is called to find the equivalent lat/lon coordinates
to specified Cartesian coordinates on a unit sphere.
The Cartesian coordinate (1.,0.,0.) is
converted to lat/lon coordinate (0.,0.). Latitudes and 
longitudes are assumed to be in degrees. 
.SH RETURN VALUE
c_csc2s does not return a value.
.SH ACCESS
To use c_csc2s, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
c_cssgrid,
c_css2c.
.sp
Complete documentation for Cssgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/cssgrid/csshome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
