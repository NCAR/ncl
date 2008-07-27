.\"
.\"	$Id: c_css2cd.m,v 1.4 2008-07-27 03:35:34 haley Exp $
.\"
.TH c_css2cd 3NCARG "MAY 2000" UNIX "NCAR GRAPHICS"
.SH NAME
c_css2cd - convert from lat/lon coordinates to Cartesian coordinates.
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
    void   c_css2cd(int, double *, double *, double *, double *, double *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
    c_css2cd(n, rlat, rlon, x, y, z);
.fi
.cs R
.sp
.SH DESCRIPTION
.IP n 12
The number of input lat/lon coordinates.
.IP rlat 12
An array containing the latitudes of the input coordinates,
in degrees. 
.IP rlon 12
An array containing the longitudes of the input coordinates,
in degrees. 
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
c_css2cd is called to find the equivalent Cartesian coordinates on a
unit sphere
to specified latitude and longitude coordinates.
The coordinate of 0. latitude and 0. longitude is
converted to Cartesian coordinate (1.,0.,0.). Latitudes and 
longitudes are assumed to be in degrees. 
c_css2cd is a double precision version of c_css2c.
.SH RETURN VALUE
c_css2cd does not return a value.
.SH ACCESS
To use c_css2cd, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
c_cssgrid,
c_csc2sd.
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
