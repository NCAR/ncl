.\"
.\"	$Id: csscoord.m,v 1.1 1999-06-29 00:23:02 fred Exp $
.\"
.TH CSSCOORD 3NCARG "JUNE 1999" UNIX "NCAR GRAPHICS"
.SH NAME
CSSCOORD - convert from a Cartesian to a lat/lon coordinate
.SH SYNOPSIS
CALL CSSCOORD (X, Y, Z, RLAT, RLON, PNM) 
.SH DESCRIPTION
.IP X,Y,Z 12
(real, input) The Cartesian coordinate of the input point. 
.IP RLAT 12
(real, output) The latitude of the output coordinate.
.IP RLON 12
(real, output) The longitude of the output coordinate.
.IP PNM 12
(real, output) The magnitude (Euclidean norm) of (X,Y,Z). 
.SH USAGE
CSSCOORD is called to find an equivalent latitude and 
longitude coordinate on a sphere to a specified Cartesian coordinate. 
The coordinate (1.,0.,0.) is mapped to the
latitude/longitude coordinate (0.,0.). The latitude/longitude 
coordinate is returned in radians. 
.SH ACCESS
To use CSSCOORD, load the NCAR Graphics library ngmath.
.SH SEE ALSO
css_overview,
cssgrid,
cstrans,
csstri,
csvoro,
c_cssgrid,
c_cstrans,
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
