.\"
.\"	$Id: kurvp1.m,v 1.4 2008-07-27 03:35:39 haley Exp $
.\"
.TH KURVP1 3NCARG "March 1998" UNIX "NCAR GRAPHICS"
.SH NAME
KURVP1 - calculate values for KURVP2 usage
.SH SYNOPSIS
CALL KURVP1 (N, X, Y, XP, YP, TEMP, S, SIGMA, IER)   
.sp
This subroutine calculates certain values that are used by KURVP2 in
order to compute an interpolatory spline under tension through a
sequence of points in the plane that form a closed curve. The actual
computation of the interpolated values must be done using KURVP2 .
.SH DESCRIPTION
.IP N 12
(integer,input) The number of input data points. (N > 1) 
.IP X 12
(real,input) An array containing the X values of the points. 
.IP Y 12
(real,input) An array containing the Y values of the points. Adjacent 
pairs of points must be distinct. 
.IP XP 12
(real,output) An array of length N. 
.IP YP 12
(real,output) An array of length N. 
.IP TEMP 12
(real,input) Scratch space. 
.IP S 12
(real,output) An array of length N (this will be used to pass to KURVP2). 
.IP SIGMA 12
(real,input) Tension factor. Values near zero result in a cubic spline; 
large values (e.g. 50) result in nearly a polygonal line. A typical value is 1. 
.IP IER 12
(integer,output) An error return value. If IER is returned as 0, then no errors
were detected. 
.sp
= 1 if N is less than 2. 
.br
= 2 if a pair of adjacent points coincide. 
.SH ACCESS
To use KURVP1, load the NCAR Graphics library ngmath.
.SH SEE ALSO
kurvp2,
fitgrid_params.
.sp
Complete documentation for Fitgrid is available at URL
.br
http://ngwww.ucar.edu/ngdoc/ng/ngmath/fitgrid/fithome.html
.SH COPYRIGHT
Copyright (C) 2000
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
