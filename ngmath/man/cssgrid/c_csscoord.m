.\"
.\"	$Id: c_csscoord.m,v 1.1 1999-06-29 00:23:01 fred Exp $
.\"
.TH c_csscoord 3NCARG "June 1999" UNIX "NCAR GRAPHICS"
.SH NAME
c_csscoord - convert from a Cartesian to a lat/lon coordinate
.SH FUNCTION PROTOTYPE
.nf
.cs R 24
    void   c_csscoord(float, float, float, float *, float *, float *);
.fi
.cs R
.sp
.SH SYNOPSIS
.nf
.cs R 24
    void c_csscoord (x, y, z, plat, plon, pnm)   
.fi
.cs R
.sp
.SH DESCRIPTION
.IP x,y,z 12
The Cartesian coordinate of the input point.
.IP plat 12
The latitude of the output coordinate.
.IP plon 12
The longitude of the output coordinate.
.IP pnm 12
The magnitude (Euclidean norm) of (X,Y,Z).
.SH USAGE
c_csscoord is called to find the equivalent latitude and 
longitude coordinate on a sphere to
a specified Cartesian coordinate on a unit sphere. 
The coordinate (1.,0.,0.) is mapped to the
latitude/longitude coordinate (0.,0.). The 
latitude/longitude coordinate is returned in radians. 
.SH ACCESS
To use c_csscoord, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
csstri,
cstrans,
csvoro,
c_cssgrid,
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
